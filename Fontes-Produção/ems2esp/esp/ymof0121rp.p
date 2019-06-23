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
{include/i-prgvrs.i YMOF0121RP 1.00.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
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

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    FIELD arquivo          AS CHAR
    field hora-exec        as integer.

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
{utp/ut-liter.i PARÂMETROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÇÃO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÃO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usuário * l}
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
assign c-titulo-relat = "Carga de Naturezas".
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.





DEFINE VARIABLE chExcelapp  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorkbook  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE i-linha     AS INTEGER         NO-UNDO.
DEFINE VARIABLE i-formato   AS INTE            NO-UNDO.
DEFINE VARIABLE i-conta009  AS INTEGER         NO-UNDO.

DEFINE VARIABLE i-teste    AS INTEGER          NO-UNDO.
DEFINE VARIABLE i-conta    AS INTEGER          NO-UNDO.
DEFINE VARIABLE c-planilha AS CHARACTER        NO-UNDO.
DEFINE VARIABLE c-um       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE c-fam      AS CHARACTER        NO-UNDO.
DEFINE VARIABLE c-matriz   AS CHARACTER  FORMAT "x(200)"  NO-UNDO.

DEFINE VARIABLE i-posicao  AS INTEGER     NO-UNDO.

 DEF SHARED VAR c-seg-usuario AS CHAR FORMAT "x(16)" NO-UNDO.

 DEFINE VARIABLE nr-linha1 AS INTEGER     NO-UNDO.

 DEF TEMP-TABLE tt-natur-oper
    FIELD Natureza                          AS CHAR FORMAT "x(6)"
    FIELD cfop                              AS CHAR FORMAT "x(10)"
    FIELD Denominacao                       AS CHAR FORMAT "x(35)"
    FIELD tipo                              AS INT
    FIELD Especie                           AS CHAR FORMAT "x(8)"
    FIELD mercado                           AS INT
    FIELD canal-venda                       AS INT  
    FIELD cod-msg                           AS INT FORMAT ">>9"
    FIELD mod-doc-of                        AS INT FORMAT ">9"
    FIELD mod-cup-fis                       AS CHAR
    FIELD mod-nfe                           AS CHAR
    FIELD esp-tit                           AS CHAR FORMAT "x(2)"
    FIELD tp-recdesp                        AS INT FORMAT ">>9"
    FIELD op-trans                          AS LOG
    FIELD nat-compl                         AS CHAR FORMAT "x(6)"
    FIELD gera-nf-fat                       AS LOG
    FIELD nf-rateio                         AS LOG
    FIELD nf-prop                           AS INT
    FIELD nf-comercio                       AS LOG
    FIELD compr-vend-ativo                  AS LOG
    FIELD gera-fich-auto                    AS LOG 
    FIELD ini-cred-autom                    AS LOG
    FIELD venda-ambulante                   AS INT
    FIELD ger-dev-so-val                    AS CHAR
    FIELD op-triangular                     AS LOG
    FIELD nat-drawback                      AS LOG
    FIELD memo-export                       AS LOG
    FIELD nat-bonific                       AS LOG
    FIELD op-terceiro                       AS LOG
    FIELD tp-oper-terc                      AS INT
    FIELD alt-vl-item-terc                  AS CHAR
    FIELD  tp-compra                        AS INT
    FIELD natur-bonif                       AS CHAR FORMAT "x(6)"
    FIELD calc-automatico                   AS LOG
    FIELD impress-automatic                 AS CHAR
    FIELD baixa-estoq                       AS LOG
    FIELD contr-estoq-auto                  AS LOG
    FIELD gera-duplic                       AS LOG
    FIELD cr-automatic                      AS LOG
    FIELD gera-obr-fisc                     AS LOG
    FIELD atualiza-cotas                    AS LOG
    FIELD NFSnaotribpcalculocoefCIAP        AS LOG
    FIELD ConsNFSpcalcoefCIAP               AS LOG
    FIELD EnviarXMLNF-eManualemente         AS LOG
    FIELD NF-edeEstorno                     AS LOG
    FIELD GerarContabilizacao               AS LOG
    FIELD ContabilizacaoAutomatica          AS LOG
    FIELD atualiza-estatisticas             AS LOG
    FIELD estatistica-auto                  AS LOG
    FIELD op-entr-futura                    AS INT
    FIELD tp-base-ipi                       AS CHAR
    FIELD IncluirFreteBaseIPI               AS CHAR
    FIELD CodVinculacaoIPI                  AS CHAR FORMAT "x(30)"
    FIELD CodTributacaoIPI                  AS INT
    FIELD perc-ReducaoIPI                   AS DEC FORMAT ">>9.99"
    FIELD ImprIPIOutrasDANFE                AS LOG
    FIELD IncluirIPIBaseICMS                AS CHAR
    FIELD IncluirIPIICMSOutras              AS LOG
    FIELD IncluirIPIOutrosTotalNF           AS CHAR
    FIELD IncluirIPIOutrosBaseSubs          AS CHAR 
    FIELD EscrituracaoIPIFrete              AS LOG
    FIELD EstornaIPI                        AS LOG
    FIELD IPIImuneST                        AS CHAR
    FIELD IPINaoTributadoSitt               AS LOG
    FIELD SuspIPIImpt                       AS LOG
    FIELD tp-base-iss                       AS CHAR
    FIELD cod-contr-iss                     AS  INT
    FIELD perc-red-iss                      AS DEC FORMAT ">>9.99"
    FIELD ConsiICMSOutrosnaNF-e             AS LOG
    FIELD natur-vinculada                   AS CHAR 
    FIELD RetemIRnaFonte                    AS LOG
    FIELD percIRRF                          AS DEC FORMAT ">>9.99"
    FIELD codcontricms                      AS INT
    FIELD aliquotaicms                      AS DEC FORMAT ">>9.99"
    FIELD aliquotoicmcompl                  AS DEC FORMAT ">>9.99"
    FIELD base-icms                         AS INT
    FIELD tp-base-icms                      AS CHAR
    FIELD estorna-icms                      AS LOG
    FIELD perc-desc-icms                    AS DEC FORMAT ">9.99"
    FIELD perc-desc-zf                      AS DEC FORMAT ">9.99"
    FIELD perc-red-icms                     AS DEC FORMAT ">>9.99"
    FIELD destino-redu                      AS INT
    FIELD substituicao-tributaria           AS LOG
    FIELD percICMSSubsTrib                  AS DEC FORMAT ">9.99"
    FIELD ItemICMSCobradoSubsTributaria     AS LOG
    FIELD ICMSOutrosVlSubsTributaria        AS CHAR
    FIELD GerarCreditoSubsTributaria        AS CHAR
    FIELD DiminuiSubstituicaoTotalFrete     AS LOG
    FIELD ComsumidorFinal                   AS LOG
    FIELD ItemICMSSuspenso                  AS LOG
    FIELD ICMSPresumido                     AS LOG
    FIELD ItemICMSDiferido                  AS LOG
    FIELD NaotributadaICMS                  AS LOG
    FIELD ContribSubstituidoAntecip         AS LOG
    FIELD CredSubstTribAntecip              AS LOG
    FIELD ICMSSubsTribAntecip               AS LOG
    FIELD RetemINSSnaFonte                  AS CHAR
    FIELD perc-inss                         AS CHAR FORMAT ">9.99"
    FIELD perc-sat                          AS DEC FORMAT ">9.99"
    FIELD perc-senar                        AS DEC FORMAT ">9.99"
    FIELD tributacaoII                      AS CHAR
    FIELD SuspImpImportacao                 AS LOG
    FIELD ICMSSTaRepassarDeduzir            AS LOG
    FIELD ICMSSTaComplementar               as log
    FIELD perc-interno-pis                  AS DEC FORMAT ">9.99"
    FIELD perc-externo-pis                  AS DEC FORMAT ">9.99"
    FIELD perc-ate31102002                  AS DEC FORMAT ">9.99"
    FIELD tributacao-pis                    AS CHAR
    FIELD perc-ret-pis                      AS DEC FORMAT ">9.99"
    FIELD perc-DescZFMPIS                   AS DEC FORMAT ">9.99"
    FIELD perc-InternoCOFINS                AS DEC FORMAT ">9.99"
    FIELD perc-externoCOFINS                AS DEC FORMAT ">9.99"
    FIELD perc-anterior                     AS DEC FORMAT ">9.99"
    FIELD TributacaoCOFINS                  AS CHAR
    FIELD perc-RetencaoCOFINS               AS DEC FORMAT ">9.99"
    FIELD perc-DescZFMCOFINS                AS DEC FORMAT ">9.99"
    FIELD perc-RetencaoCSLL                 AS DEC FORMAT ">9.99"
    FIELD IncluiIPInaBaseContribSociais     AS LOG
    FIELD IncIPIOutrasnaBaseContribSociais  AS LOG
    FIELD IncIPInaBaseContribSociaisRetido  AS LOG
    FIELD IncIPIOutrasBaseContribSocRetido  AS LOG
    FIELD IncICMS-STnaBaseContribSocReitdo  AS LOG
    FIELD IncluiICMS-STnaBasedoIRetido      AS LOG
    FIELD DeduzDescontoZFMdoPrecoVenda      AS LOG
    FIELD IncluiFretenaBaseDescontoZFM      AS CHAR
    FIELD ConsideraICMScdespNFEntFat        AS CHAR
    FIELD AliqtotICMSRedICMSincidsobrprop   AS CHAR
    FIELD ICMSincidebasedoICMS              AS CHAR
    FIELD ICMSincidtotNF                    AS CHAR
    FIELD ConsICMScdespNFEReceb             AS CHAR
    FIELD ConsidPIScdespNFEFat              AS CHAR
    FIELD ConsCOFINScdespNFEFat             AS CHAR
    FIELD DesconsideraIISuspenso            AS CHAR
    FIELD DesconsideraIPISuspenso           AS CHAR
    FIELD DesconsideraICMSSuspenso          AS CHAR
    FIELD nr-linha                          AS INT.


 DEF TEMP-TABLE tt-log
     FIELD linha    AS INT
     FIELD registro AS CHAR
     FIELD LOG-1    AS CHAR FORMAT "x(50)".

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
         HEIGHT             = 2
         WIDTH              = 40.
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
    view frame f-cabec.
    view frame f-rodape.
    run utp/ut-acomp.p persistent set h-acomp.  
    
    {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}
    
    run pi-inicializar in h-acomp (input "Processando...":U). 
    
 /*    /*:T --- Colocar aqui o código de impressão --- */             */
 /*    for each [TABELA] no-lock                                      */
 /*        where [WHERE].                                             */

        
/*     end. */

    RUN pi-processa.


    
    run pi-finalizar in h-acomp.
    {include/i-rpclo.i}
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-carrega-plan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-plan Procedure 
PROCEDURE pi-carrega-plan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    i-linha = 2.
  DO i-conta = 1 TO 10000.


      c-planilha  = chWorksheet:range("a" + STRING(i-linha)):VALUE.

      IF c-planilha  = ""  OR c-planilha = ? THEN  LEAVE.


      run pi-acompanhar in h-acomp (input "Linha => " +  STRING(i-linha)).

      CREATE tt-natur-oper.
      ASSIGN  tt-natur-oper.Natureza             = entry(1,chWorksheet:range("a" + STRING(i-linha)):VALUE,",").      
      ASSIGN  tt-natur-oper.cfop                 = entry(1,chWorksheet:range("b" + STRING(i-linha)):VALUE,",").
      ASSIGN  tt-natur-oper.Denominacao          = chWorksheet:range("c" + STRING(i-linha)):VALUE.

      /*c-matriz = chWorksheet:range("d" + STRING(2)):VALUE.*/
      /*RUN pi-monta-string(INPUT-OUTPUT c-matriz).*/

      c-matriz = "Entrada,Sa¡da,Servi‡o".
      i-posicao = IF chWorksheet:range("d" + STRING(i-linha)):VALUE = ? THEN 1 ELSE LOOKUP(chWorksheet:range("d" + STRING(i-linha)):VALUE,c-matriz).
      ASSIGN  tt-natur-oper.tipo                 = i-posicao.
      ASSIGN  tt-natur-oper.Especie              = chWorksheet:range("e" + STRING(i-linha)):VALUE.


      /*c-matriz = chWorksheet:range("f" + STRING(2)):VALUE.
      RUN pi-monta-string(INPUT-OUTPUT c-matriz).*/

      c-matriz = "Interno,Externo,Diversos".
      i-posicao = IF chWorksheet:range("f" + STRING(i-linha)):VALUE = ? THEN 1 ELSE LOOKUP(chWorksheet:range("f" + STRING(i-linha)):VALUE,c-matriz).
      ASSIGN  tt-natur-oper.mercado              = i-posicao.

      ASSIGN  tt-natur-oper.canal-venda          = INT(chWorksheet:range("g" + STRING(i-linha)):VALUE).
      ASSIGN  tt-natur-oper.cod-msg              = INT(chWorksheet:range("h" + STRING(i-linha)):VALUE).
      ASSIGN  tt-natur-oper.mod-doc-of           = INT(chWorksheet:range("i" + STRING(i-linha)):VALUE).   
      ASSIGN  tt-natur-oper.mod-cup-fis          = IF (chWorksheet:range("j" + STRING(i-linha)):VALUE = ? OR int(chWorksheet:range("j" + STRING(i-linha)):VALUE) <= 0) THEN "" ELSE string(int(chWorksheet:range("j" + STRING(i-linha)):VALUE)).  
      
      ASSIGN  tt-natur-oper.mod-nfe              = chWorksheet:range("k" + STRING(i-linha)):VALUE.  
      ASSIGN  tt-natur-oper.esp-tit              = string(chWorksheet:range("l" + STRING(i-linha)):VALUE,"x(2)").  
      ASSIGN  tt-natur-oper.tp-recdesp           = int(chWorksheet:range("m" + STRING(i-linha)):VALUE).  
      ASSIGN  tt-natur-oper.op-trans             = IF chWorksheet:range("n" + STRING(i-linha)):VALUE = "SIM" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.nat-compl            = IF entry(1,chWorksheet:range("o" + STRING(i-linha)):VALUE,",") = ? THEN "" ELSE entry(1,chWorksheet:range("o" + STRING(i-linha)):VALUE,",") .  
      ASSIGN  tt-natur-oper.gera-nf-fat          = IF chWorksheet:range("p" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.nf-rateio            = IF chWorksheet:range("q" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.nf-prop              = IF (chWorksheet:range("r" + STRING(i-linha)):VALUE) = "sim" THEN 1 ELSE 0.  
      ASSIGN  tt-natur-oper.nf-comercio          = IF chWorksheet:range("s" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.compr-vend-ativo     = IF chWorksheet:range("t" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.

      ASSIGN  tt-natur-oper.gera-fich-auto       = IF chWorksheet:range("u" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
      ASSIGN  tt-natur-oper.ini-cred-autom       = IF chWorksheet:range("v" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.


      ASSIGN  tt-natur-oper.venda-ambulante      = IF (chWorksheet:range("w" + STRING(i-linha)):VALUE) = "SIM" THEN 1 ELSE 0.  
      ASSIGN  tt-natur-oper.ger-dev-so-val       = IF chWorksheet:range("x" + STRING(i-linha)):VALUE   = "sim" THEN "1" ELSE "2".  
      ASSIGN  tt-natur-oper.op-triangular        = IF chWorksheet:range("y" + STRING(i-linha)):VALUE   = "sim" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.nat-drawback         = IF chWorksheet:range("z" + STRING(i-linha)):VALUE   = "sim" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.memo-export          = IF chWorksheet:range("aa" + STRING(i-linha)):VALUE  = "sim" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.nat-bonific          = IF chWorksheet:range("ab" + STRING(i-linha)):VALUE  = "sim" THEN YES ELSE NO.  
      ASSIGN  tt-natur-oper.op-terceiro          = IF chWorksheet:range("ac" + STRING(i-linha)):VALUE  = "sim" THEN YES ELSE NO.


      c-matriz = {ininc/i04in245.i 3}.     
      i-posicao = IF (chWorksheet:range("ad" + STRING(i-linha)):VALUE = ? OR trim(string(chWorksheet:range("ad" + STRING(i-linha)):VALUE)) <= "0") THEN 1 ELSE LOOKUP(chWorksheet:range("ad" + STRING(i-linha)):VALUE,c-matriz).          
      ASSIGN  tt-natur-oper.tp-oper-terc         = i-posicao. 
      ASSIGN  tt-natur-oper.alt-vl-item-terc     = IF chWorksheet:range("ae" + STRING(i-linha)):VALUE = "SIM" THEN "1" ELSE "2".  


      c-matriz = {ininc/i09in245.i 3}.                        
      RUN pi-monta-string(INPUT-OUTPUT c-matriz).                                  
      i-posicao = IF chWorksheet:range("af" + STRING(i-linha)):VALUE = ? THEN 1 ELSE LOOKUP(chWorksheet:range("af" + STRING(i-linha)):VALUE,c-matriz).
      ASSIGN  tt-natur-oper.tp-compra                   = i-posicao. 
      ASSIGN  tt-natur-oper.natur-bonif                 = IF chWorksheet:range("ag" + STRING(i-linha)):VALUE = ? OR chWorksheet:range("ag" + STRING(i-linha)):VALUE = "" THEN "" ELSE chWorksheet:range("ag" + STRING(i-linha)):VALUE . 
      ASSIGN  tt-natur-oper.calc-automatico             = IF chWorksheet:range("ah" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.      
      ASSIGN  tt-natur-oper.impress-automatic           = IF chWorksheet:range("ai" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".      
      ASSIGN  tt-natur-oper.baixa-estoq                 = IF chWorksheet:range("aj" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.      
      ASSIGN  tt-natur-oper.contr-estoq-auto            = IF chWorksheet:range("ak" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.      
      ASSIGN  tt-natur-oper.gera-duplic                 = IF chWorksheet:range("al" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.      
      ASSIGN  tt-natur-oper.cr-automatic                = IF chWorksheet:range("am" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.      
      ASSIGN  tt-natur-oper.gera-obr-fisc               = IF chWorksheet:range("an" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.      
      ASSIGN  tt-natur-oper.atualiza-cotas              = IF chWorksheet:range("ao" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.      
      ASSIGN  tt-natur-oper.NFSnaotribpcalculocoefCIAP  = IF chWorksheet:range("ap" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE FALSE.    
      ASSIGN  tt-natur-oper.ConsNFSpcalcoefCIAP         = IF chWorksheet:range("aq" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE FALSE. 
      ASSIGN  tt-natur-oper.EnviarXMLNF-eManualemente   = IF chWorksheet:range("ar" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
      ASSIGN  tt-natur-oper.NF-edeEstorno               = IF chWorksheet:range("as" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
      ASSIGN  tt-natur-oper.GerarContabilizacao         = IF chWorksheet:range("at" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
      ASSIGN  tt-natur-oper.ContabilizacaoAutomatica    = IF chWorksheet:range("au" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
      ASSIGN  tt-natur-oper.atualiza-estatisticas       = IF chWorksheet:range("av" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.     
      ASSIGN  tt-natur-oper.estatistica-auto            = IF chWorksheet:range("aw" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO. 

      /*c-matriz = chWorksheet:range("ax" + STRING(2)):VALUE.                           
      RUN pi-monta-string(INPUT-OUTPUT c-matriz).*/                                     

      ASSIGN c-matriz = "Nenhum,Faturamento Antecipado,Remessa de Faturamento Antecipado,Faturamento com Entrega Futura,Remessa de Entrega Futura".
      i-posicao = IF chWorksheet:range("ax" + STRING(i-linha)):VALUE = ? THEN 1 ELSE LOOKUP(chWorksheet:range("ax" + STRING(i-linha)):VALUE,c-matriz).

      ASSIGN   tt-natur-oper.op-entr-futura              = i-posicao .
     c-matriz = {varinc/var00074.i 03 }.
     i-posicao = LOOKUP(chWorksheet:range("ay" + STRING(i-linha)):VALUE,c-matriz).

     ASSIGN   tt-natur-oper.tp-base-ipi                 = IF chWorksheet:range("ay" + STRING(i-linha)):VALUE = "L¡quido" THEN "2" ELSE IF chWorksheet:range("ay" + STRING(i-linha)):VALUE = ? THEN "2" ELSE "1". 
     ASSIGN   tt-natur-oper.IncluirFreteBaseIPI         = IF chWorksheet:range("az" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".    
     ASSIGN   tt-natur-oper.CodVinculacaoIPI            = IF chWorksheet:range("ba" + STRING(i-linha)):VALUE = ? OR  chWorksheet:range("ba" + STRING(i-linha)):VALUE = "" THEN "" ELSE chWorksheet:range("ba" + STRING(i-linha)):VALUE  .

     c-matriz = {ininc/i10in172.i 03}.                         
     i-posicao = IF chWorksheet:range("bb" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("bb" + STRING(i-linha)):VALUE,c-matriz). 
     ASSIGN tt-natur-oper.CodTributacaoIPI            = i-posicao. 
     ASSIGN tt-natur-oper.perc-ReducaoIPI             = dec(chWorksheet:range("bc" + STRING(i-linha)):VALUE).    
     ASSIGN tt-natur-oper.ImprIPIOutrasDANFE          = IF chWorksheet:range("bd" + STRING(i-linha)):VALUE = "SIM" THEN YES ELSE FALSE.     
     ASSIGN tt-natur-oper.IncluirIPIBaseICMS          = IF chWorksheet:range("be" + STRING(i-linha)):VALUE   = "SIM" THEN "1" ELSE "2".
     ASSIGN tt-natur-oper.IncluirIPIICMSOutras        = IF chWorksheet:range("bf" + STRING(i-linha)):VALUE = "SIM" THEN YES ELSE NO.
     ASSIGN tt-natur-oper.IncluirIPIOutrosTotalNF     = IF chWorksheet:range("bg" + STRING(i-linha)):VALUE = "SIM" THEN "1" ELSE "2".     
     ASSIGN tt-natur-oper.IncluirIPIOutrosBaseSubs    = IF chWorksheet:range("bh" + STRING(i-linha)):VALUE = "SIM" THEN "1" ELSE "2" .    
     ASSIGN tt-natur-oper.EscrituracaoIPIFrete        = IF chWorksheet:range("bi" + STRING(i-linha)):VALUE = "SIM" THEN YES ELSE NO.    
     ASSIGN tt-natur-oper.EstornaIPI                  = IF chWorksheet:range("bj" + STRING(i-linha)):VALUE = "SIM" THEN YES ELSE NO.    
     ASSIGN tt-natur-oper.IPIImuneST                  = IF chWorksheet:range("bk" + STRING(i-linha)):VALUE = "SIM" THEN "S" ELSE "N".  
     ASSIGN tt-natur-oper.IPINaoTributadoSitt         = IF chWorksheet:range("bl" + STRING(i-linha)):VALUE = "SIM" THEN YES ELSE FALSE.  
     ASSIGN tt-natur-oper.SuspIPIImpt                 = IF chWorksheet:range("bm" + STRING(i-linha)):VALUE = "SIM" THEN YES ELSE FALSE.

     c-matriz = "bruto,L¡quido".                        
     i-posicao = IF chWorksheet:range("bN" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("bn" + STRING(i-linha)):VALUE,c-matriz).
     ASSIGN tt-natur-oper.tp-base-iss   = string(i-posicao). 

     c-matriz = "Tributado,Isento,Outros,Reduzido".                         
     i-posicao = IF chWorksheet:range("bo" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("bo" + STRING(i-linha)):VALUE,c-matriz).  
     ASSIGN tt-natur-oper.cod-contr-iss                 = i-posicao. 
     ASSIGN tt-natur-oper.perc-red-iss                  = dec(chWorksheet:range("bp" + STRING(i-linha)):VALUE).  
     ASSIGN tt-natur-oper.ConsiICMSOutrosnaNF-e         = IF chWorksheet:range("bq" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.   
     ASSIGN tt-natur-oper.natur-vinculada               = IF (chWorksheet:range("br" + STRING(i-linha)):VALUE = ? OR  chWorksheet:range("br" + STRING(i-linha)):VALUE = "") THEN "" ELSE  chWorksheet:range("br" + STRING(i-linha)):VALUE .  
     ASSIGN tt-natur-oper.RetemIRnaFonte                = IF chWorksheet:range("bs" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
     ASSIGN tt-natur-oper.percIRRF                      = dec(chWorksheet:range("bt" + STRING(i-linha)):VALUE).

     c-matriz = "Tributado,isento,outros,reduzido,diferido".                            
     i-posicao = IF chWorksheet:range("bu" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("bu" + STRING(i-linha)):VALUE,c-matriz).    
     ASSIGN tt-natur-oper.codcontricms                  = i-posicao.
     ASSIGN tt-natur-oper.aliquotaicms                  = dec(chWorksheet:range("bv" + STRING(i-linha)):VALUE).  
     ASSIGN tt-natur-oper.aliquotoicmcompl              = dec(chWorksheet:range("bw" + STRING(i-linha)):VALUE).

     c-matriz = "Bruto,L¡quido".                         
     i-posicao = IF chWorksheet:range("bx" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("bx" + STRING(i-linha)):VALUE,c-matriz). 
     ASSIGN tt-natur-oper.base-icms                     = i-posicao. 
     c-matriz = chWorksheet:range("by" + STRING(2)):VALUE.                         
     RUN pi-monta-string(INPUT-OUTPUT c-matriz).                                   
     i-posicao = LOOKUP(chWorksheet:range("by" + STRING(i-linha)):VALUE,c-matriz). 

     ASSIGN tt-natur-oper.tp-base-icms                  = chWorksheet:range("by" + STRING(i-linha)):VALUE.
     ASSIGN tt-natur-oper.estorna-icms                  = IF chWorksheet:range("bz" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO. 
     ASSIGN tt-natur-oper.perc-desc-icms                = dec(chWorksheet:range("ca" + STRING(i-linha)):VALUE). 
     ASSIGN tt-natur-oper.perc-desc-zf                  = dec(chWorksheet:range("cb" + STRING(i-linha)):VALUE). 
     ASSIGN tt-natur-oper.perc-red-icms                 = dec(chWorksheet:range("cc" + STRING(i-linha)):VALUE). 

     c-matriz = "isento,outros".                          
     i-posicao = IF chWorksheet:range("cd" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("cd" + STRING(i-linha)):VALUE,c-matriz).  
     ASSIGN tt-natur-oper.destino-redu                  = i-posicao.
     ASSIGN tt-natur-oper.substituicao-tributaria       = IF chWorksheet:range("ce" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO. 
     ASSIGN tt-natur-oper.percICMSSubsTrib              = dec(chWorksheet:range("cf" + STRING(i-linha)):VALUE).   
     ASSIGN tt-natur-oper.ItemICMSCobradoSubsTributaria = IF chWorksheet:range("cg" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
     ASSIGN tt-natur-oper.ICMSOutrosVlSubsTributaria    = IF chWorksheet:range("ch" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".  
     ASSIGN tt-natur-oper.GerarCreditoSubsTributaria    = IF chWorksheet:range("ci" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".  
     ASSIGN tt-natur-oper.DiminuiSubstituicaoTotalFrete = IF chWorksheet:range("cj" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
     ASSIGN tt-natur-oper.ComsumidorFinal               = IF chWorksheet:range("ck" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
     ASSIGN tt-natur-oper.ItemICMSSuspenso              = IF chWorksheet:range("cl" + STRING(i-linha)):VALUE = "sim" THEN TRUE ELSE FALSE.  
     ASSIGN tt-natur-oper.ICMSPresumido                 = IF chWorksheet:range("cm" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE FALSE.   
     ASSIGN tt-natur-oper.ItemICMSDiferido              = IF chWorksheet:range("cn" + STRING(i-linha)):VALUE = "sim" THEN ?   ELSE FALSE.        
     ASSIGN tt-natur-oper.NaotributadaICMS              = IF chWorksheet:range("co" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.        
     ASSIGN tt-natur-oper.ContribSubstituidoAntecip     = IF chWorksheet:range("cp" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.        
     ASSIGN tt-natur-oper.CredSubstTribAntecip          = IF chWorksheet:range("cq" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE FALSE.          
     ASSIGN tt-natur-oper.ICMSSubsTribAntecip           = IF chWorksheet:range("cr" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE FALSE.          
     ASSIGN tt-natur-oper.RetemINSSnaFonte              = IF chWorksheet:range("cs" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".         
     ASSIGN tt-natur-oper.perc-inss                     = string(dec(chWorksheet:range("ct" + STRING(i-linha)):VALUE)).        
     ASSIGN tt-natur-oper.perc-sat                      = chWorksheet:range("cu" + STRING(i-linha)):VALUE.        
     ASSIGN tt-natur-oper.perc-senar                    = chWorksheet:range("cv" + STRING(i-linha)):VALUE.
     
     c-matriz = "tributado,isento,outros,reduzido".                         
     i-posicao = IF chWorksheet:range("cw" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("cw" + STRING(i-linha)):VALUE,c-matriz).
     
     ASSIGN tt-natur-oper.tributacaoII                  = string(i-posicao).       
     ASSIGN tt-natur-oper.SuspImpImportacao             = IF chWorksheet:range("cx" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.           
     ASSIGN tt-natur-oper.ICMSSTaRepassarDeduzir        = IF chWorksheet:range("cy" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.           
     ASSIGN tt-natur-oper.ICMSSTaComplementar           = IF chWorksheet:range("cz" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.           
     ASSIGN tt-natur-oper.perc-interno-pis              = dec(chWorksheet:range("da" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-externo-pis              = dec(chWorksheet:range("db" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-ate31102002              = dec(string(chWorksheet:range("dc" + STRING(i-linha)):VALUE,"99.99")). 
     
     c-matriz = "tributado,isento,outros,reduzido".                            
     i-posicao = IF chWorksheet:range("DD" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("DD" + STRING(i-linha)):VALUE,c-matriz).
     ASSIGN tt-natur-oper.tributacao-pis                = string(i-posicao).       
     ASSIGN tt-natur-oper.perc-ret-pis                  = dec(chWorksheet:range("de" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-DescZFMPIS               = dec(chWorksheet:range("df" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-InternoCOFINS            = dec(chWorksheet:range("dg" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-externoCOFINS            = dec(chWorksheet:range("dh" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-anterior                 = dec(chWorksheet:range("di" + STRING(i-linha)):VALUE).
     
     c-matriz = "tributado,isento,outros,reduzido".                            
     i-posicao = IF chWorksheet:range("dj" + STRING(i-linha)):VALUE = ? THEN 2 ELSE LOOKUP(chWorksheet:range("dj" + STRING(i-linha)):VALUE,c-matriz).
     ASSIGN tt-natur-oper.TributacaoCOFINS                  = string(i-posicao).     
     ASSIGN tt-natur-oper.perc-RetencaoCOFINS               = dec(chWorksheet:range("dk" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-DescZFMCOFINS                = dec(chWorksheet:range("dl" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.perc-RetencaoCSLL                 = dec(chWorksheet:range("dm" + STRING(i-linha)):VALUE).        
     ASSIGN tt-natur-oper.IncluiIPInaBaseContribSociais     = IF chWorksheet:range("dn" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.    
     ASSIGN tt-natur-oper.IncIPIOutrasnaBaseContribSociais  = IF chWorksheet:range("do" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
     ASSIGN tt-natur-oper.IncIPInaBaseContribSociaisRetido  = IF chWorksheet:range("dp" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
     ASSIGN tt-natur-oper.IncIPIOutrasBaseContribSocRetido  = IF chWorksheet:range("dq" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.
     ASSIGN tt-natur-oper.IncICMS-STnaBaseContribSocReitdo  = IF chWorksheet:range("dr" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.  
     ASSIGN tt-natur-oper.IncluiICMS-STnaBasedoIRetido      = IF chWorksheet:range("ds" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.     
     ASSIGN tt-natur-oper.DeduzDescontoZFMdoPrecoVenda      = IF chWorksheet:range("dt" + STRING(i-linha)):VALUE = "sim" THEN YES ELSE NO.     
     ASSIGN tt-natur-oper.IncluiFretenaBaseDescontoZFM      = IF chWorksheet:range("du" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".    
     ASSIGN tt-natur-oper.ConsideraICMScdespNFEntFat        = IF chWorksheet:range("dv" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".    
     ASSIGN tt-natur-oper.AliqtotICMSRedICMSincidsobrprop   = IF chWorksheet:range("dw" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".     
     ASSIGN tt-natur-oper.ICMSincidebasedoICMS              = IF chWorksheet:range("dx" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".    
     ASSIGN tt-natur-oper.ICMSincidtotNF                    = IF chWorksheet:range("dy" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".    
     ASSIGN tt-natur-oper.ConsICMScdespNFEReceb             = IF chWorksheet:range("dz" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".     
     ASSIGN tt-natur-oper.ConsidPIScdespNFEFat              = IF chWorksheet:range("ea" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".     
     ASSIGN tt-natur-oper.ConsCOFINScdespNFEFat             = IF chWorksheet:range("eb" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".     
     ASSIGN tt-natur-oper.DesconsideraIISuspenso            = IF chWorksheet:range("ec" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".     
     ASSIGN tt-natur-oper.DesconsideraIPISuspenso           = IF chWorksheet:range("ed" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".    
     ASSIGN tt-natur-oper.DesconsideraICMSSuspenso          = IF chWorksheet:range("ee" + STRING(i-linha)):VALUE = "sim" THEN "1" ELSE "2".

     ASSIGN tt-natur-oper.nr-linha = i-linha.


      i-linha = i-linha + 1 . 

  END.






















END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-carrega-tab-natur-oper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tab-natur-oper Procedure 
PROCEDURE pi-carrega-tab-natur-oper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE c-arq-log AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i-cont-natur AS INTEGER     NO-UNDO.
    DEF BUFFER b-tt-natur-oper FOR tt-natur-oper.

   /*  ASSIGN c-arq-log = "d:\temp\" + c-seg-usuario + "\" +  "ymof0121rp-log.txt" .            */
   /*                                                                                           */
   /*  OUTPUT TO VALUE(c-arq-log).                                                              */

    FOR EACH tt-natur-oper NO-LOCK
          BREAK BY tt-natur-oper.natureza:

        IF FIRST-OF(tt-natur-oper.natureza) THEN
            ASSIGN i-cont-natur = 0.

        FIND FIRST b-tt-natur-oper 
             WHERE b-tt-natur-oper.natureza = tt-natur-oper.natureza NO-ERROR.
        IF AVAIL(b-tt-natur-oper) THEN
            ASSIGN i-cont-natur = i-cont-natur + 1.

        IF i-cont-natur >= 2 THEN
            DELETE b-tt-natur-oper.
    END.


    FOR EACH tt-natur-oper:
        FIND natur-oper WHERE natur-oper.nat-operacao = tt-natur-oper.natureza NO-LOCK NO-ERROR.
        IF AVAIL natur-oper  THEN DO:
            CREATE tt-log.
            ASSIGN   tt-log.linha    = tt-natur-oper.nr-linha
                     tt-log.registro = tt-natur-oper.natureza
                     tt-log.log-1    = "Natureza " + tt-natur-oper.natureza + "  ja cadastrada".
                     DELETE tt-natur-oper.
                     NEXT.
        END.
        ELSE DO:
            IF tt-natur-oper.Especie = ? THEN DO:
                ASSIGN   tt-log.linha    = tt-natur-oper.nr-linha
                         tt-log.registro = tt-natur-oper.Especie
                         tt-log.log-1    = "Especie " + tt-natur-oper.Especie + "  nÆo pode ser cadastrada".
                         DELETE tt-natur-oper.
                         NEXT.
            END.
        END.
    END.

    FOR EACH tt-natur-oper:

           DISABLE TRIGGERS FOR LOAD OF natur-oper.
           DISABLE TRIGGERS FOR DUMP OF natur-oper.

          CREATE  natur-oper.
                  natur-oper.nat-operacao              = tt-natur-oper.Natureza   .  
                  SUBSTRING(natur-oper.char-2,21,5)    = tt-natur-oper.tp-base-iss . 
                  natur-oper.cod-cfop                  = tt-natur-oper.cfop       .                                                                                                                                               
                  natur-oper.denominacao               = tt-natur-oper.Denominacao.                                                                                                                                            
                  natur-oper.tipo                      = tt-natur-oper.tipo    .                                                                                                             
                  natur-oper.especie-doc               = tt-natur-oper.Especie .                                                                                                                                               
                  natur-oper.mercado                   = tt-natur-oper.mercado .                                                                                                         
                  natur-oper.cod-canal-venda           = tt-natur-oper.canal-venda.                                                                                                                                            
                  natur-oper.cod-mensagem              = tt-natur-oper.cod-msg    .                                                                                                                                            
                  natur-oper.cd-situacao               = tt-natur-oper.mod-doc-of .                                                                                                                                            
                  natur-oper.modelo-cupom              = tt-natur-oper.mod-cup-fis .  
                  substring(natur-oper.char-2,76,2)    = tt-natur-oper.mod-cup-fis .  
                  natur-oper.cod-model-nf-eletro       = IF tt-natur-oper.mod-nfe = ? THEN "" ELSE tt-natur-oper.mod-nfe    .                                                                                                                                           
                  natur-oper.cod-esp                   = tt-natur-oper.esp-tit     .                                                                                                                                           
                  natur-oper.tp-rec-desp               = tt-natur-oper.tp-recdesp  .                                                                                                                                           
                  natur-oper.transf                    = tt-natur-oper.op-trans.                                                                                                         
                  natur-oper.nat-comp                  = tt-natur-oper.nat-compl  .                                                                                                                                          
                  natur-oper.imp-nota                  = tt-natur-oper.gera-nf-fat .                                                                                                             
                  natur-oper.nota-rateio               = tt-natur-oper.nf-rateio .                                                                                                               
                  natur-oper.int-1                     = tt-natur-oper.nf-prop .                                                                                                                     
                  natur-oper.log-2                     = tt-natur-oper.nf-comercio .                                                                                                      
                  natur-oper.venda-ativo               = tt-natur-oper.compr-vend-ativo.                                                                                                          
                  natur-oper.int-2                     = tt-natur-oper.venda-ambulante.  
                  substr(natur-oper.char-2,88,5)       = string(tt-natur-oper.ger-dev-so-val,"x(5)").                                                        
                  natur-oper.log-oper-triang           = tt-natur-oper.op-triangular .                                                                                                                                              
                  natur-oper.log-natur-operac-draw     = tt-natur-oper.nat-drawback .                                                                                                                                                        
                  natur-oper.log-memorando             = tt-natur-oper.memo-export .                                                                                                                                                    
                  natur-oper.log-natureza-bonif        = tt-natur-oper.nat-bonific .                                                                                                                                                            
                  natur-oper.terceiros                 = tt-natur-oper.op-terceiro .                                                                                                                                                 
                  natur-oper.tp-oper-terc              = tt-natur-oper.tp-oper-terc .                                                                                                                                                     
                  substr(natur-oper.char-2,36,5)       = tt-natur-oper.alt-vl-item-terc .                                                                                                                                                  
                  natur-oper.tipo-compra               = tt-natur-oper.tp-compra     .                                                                            
                  natur-oper.cod-natur-oper-bonif      = tt-natur-oper.natur-bonif    .                                                                                                                                                  
                  natur-oper.calc-auto                 = tt-natur-oper.calc-automatico .                                                                         
                  natur-oper.sc-transven               = string(tt-natur-oper.impress-automatic,"x(5)") .                                                                                                                                                                      
                  natur-oper.baixa-estoq               = tt-natur-oper.baixa-estoq      .                                                                                                                                                
                  natur-oper.auto-ce                   = tt-natur-oper.contr-estoq-auto .                                                                                                                                                
                  natur-oper.emite-duplic              = tt-natur-oper.gera-duplic .                                                                                                                                                     
                  natur-oper.auto-cr                   = tt-natur-oper.cr-automatic .                                                                                                                                                    
                  natur-oper.ind-gera-of               = tt-natur-oper.gera-obr-fisc .                                                                                                                                                   
                  natur-oper.log-atual-cotas           = tt-natur-oper.atualiza-cotas .                                                                                                                                                  
                  natur-oper.credito-ciap              = tt-natur-oper.NFSnaotribpcalculocoefCIAP .                                                                                                                                      
                  natur-oper.log-acum-ciap             = tt-natur-oper.ConsNFSpcalcoefCIAP   .                                                                                                                                           
                  natur-oper.INd-contabilizacao        = tt-natur-oper.EnviarXMLNF-eManualemente.                                                                                                                                        
                  natur-oper.auto-ct                   = tt-natur-oper.ContabilizacaoAutomatica .                                                                                                                                                   
                  natur-oper.atual-estat               = tt-natur-oper.atualiza-estatisticas. 
                  natur-oper.auto-est                  = tt-natur-oper.estatistica-auto. 
                  natur-oper.cd-vinc-ipi               = tt-natur-oper.CodVinculacaoIPI    .                                                                                                                                               
                  natur-oper.cd-trib-ip                = tt-natur-oper.CodTributacaoIPI    .                                                                                                   
                  natur-oper.perc-red-ipi              = tt-natur-oper.perc-ReducaoIPI     .                                                                                                                                       
                  natur-oper.log-impr-ipi-danfe        = tt-natur-oper.ImprIPIOutrasDANFE  .                                                                                                                                                          
                  substr(natur-oper.char-2,6,5)        = "".                                                                                                                    
                  natur-oper.ipi-icm-out               = tt-natur-oper.IncluirIPIICMSOutras .                                                                                                      
                  substri(natur-oper.char-2,16,5)      = string(tt-natur-oper.IncluirIPIOutrosTotalNF,"x(5)").                                                                                                                              
                  substri(natur-oper.char-2,46,5)      = tt-natur-oper.IncluirIPIOutrosBaseSubs.                                                                                                                             
                  natur-oper.usa-pick                  = tt-natur-oper.EscrituracaoIPIFrete .                                                                                                           
                  natur-oper.manut-ipi                 = tt-natur-oper.EstornaIPI .                                                                                                                       
                  
                  substri(natur-oper.char-1,152,1)     = tt-natur-oper.IPIImuneST .                                                                                                                                    

                  natur-oper.log-ipi-nao-tributad      = tt-natur-oper.IPINaoTributadoSitt .                                                                                                                                
                  natur-oper.log-suspens-ipi-import    = tt-natur-oper.SuspIPIImpt . 
                                                                                                                         
                  natur-oper.cd-trib-iss                     = tt-natur-oper.cod-contr-iss .                                                                                                      
                  natur-oper.perc-red-iss                    = tt-natur-oper.perc-red-iss  .                                                                                                                                          
                  natur-oper.log-consid-icms-outras          = tt-natur-oper.ConsiICMSOutrosnaNF-e .                                                                                                                              
                  natur-oper.nat-vinculada                   = tt-natur-oper.natur-vinculada .                                                                                                                                       
                  natur-oper.ind-imprenda                    = tt-natur-oper.RetemIRnaFonte .                                                                                                                          
                  natur-oper.val-perc-impto-renda            = tt-natur-oper.percIRRF.                                                                                                                                                
                  natur-oper.cd-trib-icm                     = tt-natur-oper.codcontricms .                                                                                                                          
                  natur-oper.aliquota-icm                    = tt-natur-oper.aliquotaicms .                                                                                                     
                  natur-oper.aliq-icm-com                    = tt-natur-oper.aliquotoicmcompl .                                                                                                         
                  natur-oper.merc-base-icms                  = tt-natur-oper.base-icms   .                                                                                         
                  natur-oper.tp-base-icm                     = tt-natur-oper.tp-base-icms .                                                                                                             
                  natur-oper.manut-icm                       = tt-natur-oper.estorna-icms .                                                                                                        
                  natur-oper.per-des-icms                    = tt-natur-oper.perc-desc-icms.                                                                                                            
                  substr(natur-oper.char-2,66,5)             = string(tt-natur-oper.perc-desc-zf).                                                                                                 
                  natur-oper.perc-red-icm                    = tt-natur-oper.perc-red-icms .                                                                                                            
                  natur-oper.dec-2                           = tt-natur-oper.destino-redu   .                                                                               
                  natur-oper.subs-trib                       = tt-natur-oper.substituicao-tributaria .                                                                                                          
                  natur-oper.icms-subs-trib                  = tt-natur-oper.percICMSSubsTrib                  .                                                                                                                           
                  natur-oper.ind-it-icms                     = tt-natur-oper.ItemICMSCobradoSubsTributaria .                                                                                                       
                  substri(natur-oper.char-2,51,5)            = tt-natur-oper.ICMSOutrosVlSubsTributaria .                                                                                                                         
                  substri(natur-oper.char-2,56,5)            = tt-natur-oper.GerarCreditoSubsTributaria  .                                                                                                              
                  natur-oper.usa-nota-ent                    = tt-natur-oper.DiminuiSubstituicaoTotalFrete.                                                                                                    
                  natur-oper.consum-final                    = tt-natur-oper.ComsumidorFinal  .                                                                                                                
                  natur-oper.log-icms-presmdo                = tt-natur-oper.ICMSPresumido.
                  natur-oper.ind-it-sub-dif                  = tt-natur-oper.ItemICMSSuspenso .                                                                                                                                             
                  natur-oper.ind-it-sub-dif                  = tt-natur-oper.ItemICMSDiferido.                                                                                                                                                   
                  natur-oper.ind-tipo-vat                    = tt-natur-oper.NaotributadaICMS .                                                                                                                                                             
                  natur-oper.log-contrib-st-antec            = tt-natur-oper.ContribSubstituidoAntecip.                                                                                                  
                  natur-oper.log-cr-st-antec                 = tt-natur-oper.CredSubstTribAntecip.                                                            
                  natur-oper.log-icms-substto-antecip        = tt-natur-oper.ICMSSubsTribAntecip .                                                                                                                                              
                  /*substri(natur-oper.char-2,71,5)            = tt-natur-oper.RetemINSSnaFonte.                                                                                                                                   
                  Overlay(natur-oper.char-1,40,5)            = STRING(DEC(tt-natur-oper.perc-inss)).*/                                                                                                                 
                  natur-oper.val-perc-sat                    = tt-natur-oper.perc-sat.                                                                                                                  
                  natur-oper.val-perc-senar                  = tt-natur-oper.perc-senar.                                                                                                                  
                  substri(natur-oper.char-1,10,2)            = STRING(tt-natur-oper.tributacaoII).  
                  natur-oper.log-suspens-impto-import        = tt-natur-oper.SuspImpImportacao.                                                                                                                    
                  natur-oper.log-icms-substto-repas          = tt-natur-oper.ICMSSTaRepassarDeduzir.                                                                                                                
                  natur-oper.log-icms-substto-compltar       = tt-natur-oper.ICMSSTaComplementar.                                                                                                           
                  natur-oper.perc-pis[1]                     = tt-natur-oper.perc-interno-pis.                                                                                                                                        
                  natur-oper.perc-pis[2]                     = tt-natur-oper.perc-externo-pis .                                                                                                                                       
                  
                  OVERLAY(natur-oper.char-1,76,5)          = STRING(DEC(tt-natur-oper.perc-ate31102002)).                                                                                                          
                  
                  SUBSTRING(natur-oper.char-1,86,1)          = string(tt-natur-oper.tributacao-pis).                                                                                                         
                  natur-oper.cdd-perc-retenc-pis             = tt-natur-oper.perc-ret-pis.                                                                                                                       
                  natur-oper.val-perc-desc-pis-zfm           = tt-natur-oper.perc-DescZFMPIS .                                                                                                                   
                  natur-oper.per-fin-soc[1]                  = tt-natur-oper.perc-InternoCOFINS .                                                                                                                                          
                  natur-oper.per-fin-soc[2]                  = tt-natur-oper.perc-externoCOFINS .                                                                                                                                          
                  OVERLAY(natur-oper.char-1,81,5)          = STRING(DEC(tt-natur-oper.perc-anterior),">9.99")  .                                                                                                           
                  SUBSTRING(natur-oper.char-1,87,1)          = string(tt-natur-oper.TributacaoCOFINS) .                                                                                                         
                  natur-oper.cdd-perc-retenc-cofins          = tt-natur-oper.perc-RetencaoCOFINS.                                                                                                                   
                  natur-oper.val-perc-desc-cofins-zfm        = tt-natur-oper.perc-DescZFMCOFINS .                                                                                                                    
                  natur-oper.cdd-perc-retenc-csll            = tt-natur-oper.perc-RetencaoCSLL .                                                                                                         
                  natur-oper.log-ipi-contrib-social          = tt-natur-oper.IncluiIPInaBaseContribSociais   .                                                                                         
                  natur-oper.log-ipi-outras-contrib-social   = tt-natur-oper.IncIPIOutrasnaBaseContribSociais .                                                                                                                    
                  natur-oper.log-ipi-contrib-retid           = tt-natur-oper.IncIPInaBaseContribSociaisRetido .                                                                                                                    
                  natur-oper.log-ipi-outras-contrib-retid    = tt-natur-oper.IncIPIOutrasBaseContribSocRetido .                                                                                                              
                  natur-oper.log-icms-substto-base-contrib   = tt-natur-oper.IncICMS-STnaBaseContribSocReitdo .                                                                                        
                  natur-oper.log-icms-substto-base-irf-retid = tt-natur-oper.IncluiICMS-STnaBasedoIRetido  .                                                                                           
                  natur-oper.log-deduz-desc-zfm-tot-nf       = tt-natur-oper.DeduzDescontoZFMdoPrecoVenda  .     
                  natur-oper.auto-ct                         = tt-natur-oper.ContabilizacaoAutomatica.
                  natur-oper.ind-contab                      = tt-natur-oper.GerarContabilizacao.
                  substr(natur-oper.char-2,11,5)             = string(tt-natur-oper.tp-base-ipi,"X(5)").
                  substr(natur-oper.char-2,1,5)              = tt-natur-oper.IncluirIPIBaseICMS. 
                  /*OVERLAY(natur-oper.char-1,1,5)             = IF tt-natur-oper.op-entr-futura = 4 THEN "1" ELSE "0".*/
                        

                         RELEASE natur-oper.

                        /*  /* natur-oper.log-icms-despes-import          =  */                                              */
                        /*  /* PUT UNFORMAT /* tgConsideraAliqTotICMS */  =  * natur-oper.char-1 */                                              */
                        /*  natur-oper.log-integr-base-calc-icms-n =       tt-natur-oper.IncluiFretenaBaseDescontoZFM        */
                        /*  natur-oper.log-incid-icms-tot-nfr      =       tt-natur-oper.ConsideraICMScdespNFEntFat          */
                        /*  natur-oper.log-icms-despes-import-nfr  =       tt-natur-oper.AliqtotICMSRedICMSincidsobrprop     */
                        /*  natur-oper.log-pis-despes-import       =       tt-natur-oper.ICMSincidebasedoICMS                */
                        /*  natur-oper.log-cofins-despes-import    =       tt-natur-oper.ICMSincidtotNF                      */
                        /* =  .            */
                        /* =   .      */
                        /* =           */
                        /* =  tt-natur-oper.estatistica-auto.               */
                        /* =  tt-natur-oper.op-entr-futura .                */
                        /* =  .                    */
                        /* =  .            */

                         FIND FIRST ri-nat-operacao EXCLUSIVE-LOCK                                            
                              WHERE ri-nat-operacao.nat-operacao = tt-natur-oper.Natureza NO-ERROR.

                         IF AVAIL ri-nat-operacao THEN DELETE ri-nat-operacao.
                         ELSE DO:
                             CREATE ri-nat-operacao.
                             ASSIGN ri-nat-operacao.nat-operacao         = tt-natur-oper.Natureza
                                    ri-nat-operacao.ind-ini-cred-auto    = tt-natur-oper.ini-cred-autom
                                    ri-nat-operacao.ind-gera-ficha-auto  = tt-natur-oper.gera-fich-auto.
                         END.
                     
                         CREATE tt-log.                                                                                                           
                                tt-log.linha    = tt-natur-oper.nr-linha.                                                                                
                                tt-log.registro = tt-natur-oper.natureza.                                                                                
                                tt-log.log-1    = "Natureza " + tt-natur-oper.natureza + "Importada com sucesso".
                         

                         FIND natur-oper WHERE natur-oper.nat-opera‡ao = tt-natur-oper.Natureza EXCLUSIVE-LOCK NO-ERROR.
                         IF AVAIL natur-oper  THEN DO:
                             
                             
                             SUBSTRING(natur-oper.char-2,88,5)        = string(tt-natur-oper.ger-dev-so-val,"x(5)").   
                             SUBSTRING(natur-oper.char-2,36,5)        = string(tt-natur-oper.alt-vl-item-terc,"x(5)") . 
                             SUBSTRING(natur-oper.char-2,11,5)        = string(tt-natur-oper.tp-base-ipi,"X(5)").
                             OVERLAY(natur-oper.char-2,6,5)           = string(tt-natur-oper.IncluirFreteBaseIPI,"x(5)").
                             SUBSTRING(natur-oper.char-2,16,5)        = string(tt-natur-oper.IncluirIPIOutrosTotalNF,"x(5)").
                             SUBSTRING(natur-oper.char-2,46,5)        = string(tt-natur-oper.IncluirIPIOutrosBaseSubs,"x(5)"). 
                             SUBSTRING(natur-oper.char-1,152,1)       = STRING(tt-natur-oper.IPIImuneST,"X(1)").
                             SUBSTRING(natur-oper.char-2,21,5)        = STRING(tt-natur-oper.tp-base-iss,"x(5)").
                             SUBSTRING(natur-oper.char-2,66,5)        = STRING(tt-natur-oper.perc-desc-zf).
                             SUBSTRING(natur-oper.char-2,51,5)        = string(tt-natur-oper.ICMSOutrosVlSubsTributaria,"x(5)") . 
                             SUBSTRING(natur-oper.char-2,56,5)        = string(tt-natur-oper.GerarCreditoSubsTributaria,"x(5)").
                             OVERLAY(natur-oper.char-2,76,2)          = tt-natur-oper.mod-cup-fis . 
                             OVERLAY(natur-oper.char-2,78,7)          = "9.99.9".
                             natur-oper.ind-it-sub-dif                = tt-natur-oper.ItemICMSDiferido.
                             OVERLAY(natur-oper.char-2,71,5)        = STRING(tt-natur-oper.RetemINSSnaFonte,"X(5)"). 
                             OVERLAY(natur-oper.char-1,40,5)        = string(dec(tt-natur-oper.perc-inss)). 
                             substring(natur-oper.char-1,10,2)        = string(tt-natur-oper.tributacaoII,"99").
                             SUBSTRING(natur-oper.char-1,156,1)       = tt-natur-oper.DesconsideraICMSSuspenso.
                             SUBSTRING(natur-oper.char-1,155,1)       = tt-natur-oper.DesconsideraIPISuspenso.
                             SUBSTRING(natur-oper.char-1,154,1)       = tt-natur-oper.DesconsideraIISuspenso. 
                             SUBSTRING(natur-oper.char-2,143,1)       = tt-natur-oper.ConsCOFINScdespNFEFat.
                             natur-oper.log-cofins-despes-import      = IF tt-natur-oper.ConsCOFINScdespNFEFat = "1" THEN YES ELSE NO.
                             SUBSTRING(natur-oper.char-2,142,1)       = TT-natur-oper.ConsidPIScdespNFEFat.
                             natur-oper.log-pis-despes-import         = IF TT-natur-oper.ConsidPIScdespNFEFat  = "1" THEN YES ELSE NO.
                             SUBSTR(natur-oper.char-2,146,1)          = tt-natur-oper.ConsICMScdespNFEReceb.
                             natur-oper.log-icms-despes-import-nfr    = IF tt-natur-oper.ConsICMScdespNFEReceb = "1" THEN YES ELSE NO.
                             SUBSTR(natur-oper.char-2,145,1)          = tt-natur-oper.ICMSincidtotNF.
                             natur-oper.log-incid-icms-tot-nfr        = IF tt-natur-oper.ICMSincidtotNF        = "1" THEN YES ELSE NO.
                             natur-oper.log-integr-base-calc-icms-nfr = IF tt-natur-oper.ICMSincidebasedoICMS  = "1" THEN YES ELSE NO.
                             SUBSTRING(natur-oper.char-2,144,1)       = tt-natur-oper.ICMSincidebasedoICMS.
                             SUBSTRING(natur-oper.Char-1,157,1)       = tt-natur-oper.AliqtotICMSRedICMSincidsobrprop.
                             natur-oper.log-icms-despes-import        = IF tt-natur-oper.ConsideraICMScdespNFEntFat = "1" THEN YES ELSE NO.
                             SUBSTR(natur-oper.char-2,141,1)          = tt-natur-oper.ConsideraICMScdespNFEntFat.
                             SUBSTRING(natur-oper.char-1,160,1)       = IF tt-natur-oper.IncluiFretenaBaseDescontoZFM = "1" THEN "S" ELSE "N".
                             SUBSTRING(natur-oper.char-1,87,1)        = string(tt-natur-oper.TributacaoCOFINS).
                             


                         END.





    END.


    FOR EACH tt-log  BREAK BY tt-log.linha :

        nr-linha1 =  nr-linha1 + 1.

        IF nr-linha1 = 1  THEN DO:

            PUT "--------------------------------------------------------------"  SKIP.

            PUT UNFORMAT "Linha"     AT 1
                "Registro"  AT 7
                "Log"       AT 16 SKIP.
            PUT "--------------------------------------------------------------"  SKIP.
            PUT UNFORMAT  tt-log.linha    AT 1
                 tt-log.registro AT 7
                 tt-log.LOG-1    AT 16 SKIP.

        END.
        ELSE DO:
            PUT UNFORMAT  tt-log.linha    AT 1 
                 tt-log.registro          AT 7          
                 tt-log.LOG-1             AT 16 SKIP.
        END.

         
    END.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-excel Procedure 
PROCEDURE pi-cria-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE "Excel.Application" chExcelapp.
chExcelapp:VISIBLE = FALSE.
chWorkbook         = chExcelapp:Workbooks:ADD(tt-param.arq-entrada) .
chWorksheet        = chExcelapp:Sheets:ITEM(1).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-fecha-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fecha-excel Procedure 
PROCEDURE pi-fecha-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE c-nomearq AS CHARACTER   NO-UNDO.

     c-nomearq = "c:\temp\ymof0107" + c-seg-usuario + ".xlsx".

     IF SEARCH(c-nomearq) <> ? THEN 
     OS-COMMAND SILENT DEL VALUE(c-nomearq).
     chExcelapp:workbooks:item(1):SaveAs(c-nomearq,,,,,,,).   
     chExcelapp:workbooks:APPLICATION:QUIT.
     IF SEARCH(c-nomearq) <> ? THEN
     OS-COMMAND SILENT DEL value(c-nomearq) .

    if  valid-handle( chExcelapp) then release object chExcelapp.       
    if  valid-handle( chWorkbook) then release object chWorkbook.       
    if  valid-handle( chWorksheet) then release object chWorksheet.      





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-monta-string) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-string Procedure 
PROCEDURE pi-monta-string :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT-OUTPUT  PARAMETER p-matriz   AS CHARACTER  FORMAT "x(200)"  NO-UNDO.
    DEFINE VARIABLE r AS INTEGER     NO-UNDO.

    DO r = 1 TO NUM-ENTRIES(p-matriz):
        ENTRY(r,p-matriz,",") = TRIM(ENTRY(r,p-matriz,",")).
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
RUN pi-cria-excel.
RUN pi-carrega-plan.
RUN pi-carrega-tab-natur-oper.
RUN pi-fecha-excel.






END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



                                                                 

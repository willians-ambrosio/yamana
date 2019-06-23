/*******************************************************************************
**
** Programa  : ESMRI0001RP
**
** Objetivo  : Relat¢rio de BENS (RI)
**
** Autor     : Renato Oliveira
**
** Data      : Agosto/2018
**
** Versao    : 2.12.00.000 - Desenvolvimento Inicial
**
******************************************************************************/
{include/i-prgvrs.i ESMRI0001RP 2.12.00.000}
/**********************  Defini‡Æo tabela de Parƒmetros **********************/
define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char
    field usuario           as char
    field data-exec         as date
    field hora-exec         as INTEGER
    field cod-estabel-ini  like ri-bem.cod-estabel 
    field cod-estabel-fim  like ri-bem.cod-estabel  
    field serie-ini        like ri-bem.serie        
    field serie-fim        like ri-bem.serie        
    field nr-nota-fis-ini  like ri-bem.nr-nota-fis  
    field nr-nota-fis-fim  like ri-bem.nr-nota-fis  
    field cod-emitente-ini like ri-bem.cod-emitente
    field cod-emitente-fim like ri-bem.cod-emitente      
    field dt-emissao-ini   like ri-bem.dat-entrada
    field dt-emissao-fim   like ri-bem.dat-entrada
    field arquivo-excel    as char.

define temp-table tt-digita no-undo
    field it-codigo like ITEM.it-codigo
    index id it-codigo.
        
DEFINE temp-table tt-raw-digita
   field raw-digita      as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.    

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end. /* for each tt-raw-digita: */


DEF TEMP-TABLE tt-ficha NO-UNDO
    FIELD mes         AS INTEGER
    FIELD sequencia   AS INTEGER
    FIELD mes-ano     AS INTEGER FORMAT ">>>9"
    FIELD ano         AS INTEGER FORMAT "9" INIT 1
    FIELD fator       AS DECIMAL FORMAT ">9.9999"
    FIELD valor       AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD cod-estabel LIKE ri-bem.cod-estabel
    FIELD periodo-atual AS CHARACTER
    FIELD parcela-atual AS INTEGER
    INDEX ch-sequencia sequencia
    INDEX ch-mes mes.

/* Definicao dos Buffers */

/*-------------- Vari vel Excel ----------------*/
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.

/*------------------- Variaveis Documento --------------------*/
DEFINE VARIABLE h-acomp    AS HANDLE NO-UNDO.
DEFINE VARIABLE i-contador AS INT    NO-UNDO.
DEFINE VARIABLE c-arquivo  AS CHAR   NO-UNDO.
DEFINE VARIABLE i-grupo-ciap     AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-nr-meses-ciap  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChaveNfe LIKE nota-fiscal.cod-chave-aces-nf-eletro NO-UNDO.
DEF VAR i-numero-ordem    AS INTEGER FORMAT ">>,>>>,>>>"          NO-UNDO.
DEF VAR de-taxa           AS DECIMAL                              NO-UNDO.
DEF VAR mes-entrada       AS INTEGER                              NO-UNDO.
DEF VAR i-mes-aux         AS INTEGER                              NO-UNDO.
DEF VAR da-data           LIKE ri-bem.dat-entrada                 NO-UNDO.
DEF VAR de-vl-credito     LIKE ri-valor-bem.val-imposto EXTENT 2  NO-UNDO.

DEF BUFFER b-ri-bem       FOR ri-bem.
DEF BUFFER buffer-ri-bem  FOR ri-bem.
DEF BUFFER b-ri-bem-grupo FOR ri-bem-grupo.
DEF BUFFER b-tt-ficha     FOR tt-ficha.

/* In¡cio - Bloco Principal */
find first param-global NO-LOCK NO-ERROR.
find first param-compra NO-LOCK NO-ERROR.

{include/i-rpvar.i}

assign c-programa     = "ESMRI0001"
       c-versao       = "2.12"
       c-revisao      = "00.000"
       c-empresa      = param-global.grupo
       c-sistema      = "Especifico"
       c-titulo-relat = "Relat¢rio de BENS (RI)".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.  

RUN pi-inicializar IN h-acomp (INPUT "Aguarde, Gerando...").

IF tt-param.arquivo-excel <> "" THEN DO:
    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "ARQ" + STRING(TIME) + ".CSV".
    OUTPUT TO VALUE(c-arquivo) NO-CONVERT.
    PUT "Estab;Ser;Nota Fiscal;Sequencia da Nota;ID_BEM;Natureza Opera‡Æo;Descri‡Æo do Bem;C¢digo do Emitente;Nome do Emitente;Nr Patrim“nio;Sequencia Patrim“nio;Quantidade;Unidade;Modelo do Documento;Chave NF-e;NCM;Data de Entrada;Data de Inicio do Credito ICMS;Ordem Produ‡Æo;Ordem Investimento;Centro de Custo;Valor do Icms do Item (Valor do Imposto);Valor ICMS Complementar;Valor ICMS Nao Trib. Desp. Acess¢ria;Observa‡Æo;Data;Sa¡da;Nota Sa¡da;S‚rie Sa¡da;Emitente;Sequencia;Natureza de Opera‡Æo;Conta Cont bil;N£mero de Meses;Parcela;Per¡odo;Valor da Parcela" AT 01 SKIP.
END.
ELSE DO:
   {include/i-rpout.i}
   {include/i-rpcab.i}
   VIEW FRAME f-cabec.
   VIEW FRAME f-rodape.
END.

/* --------------------------------------------------------- */
FOR FIRST ri-grupos NO-LOCK
    WHERE ri-grupos.log-ciap :
    ASSIGN i-grupo-ciap    = ri-grupos.cod-grupo
           i-nr-meses-ciap = ri-grupos.num-meses.
END.

/* --------------------------------------------------------- */
DEFINE VARIABLE iParcela            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSeqAtual           AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-periodo           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-periodo-atual     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-val-imposto       AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-vl-icmsco-it      AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-vl-icmsnt-desp    AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-observacao        AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-dat-movto         AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-idi-tipo-movto    AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-nr-nota-fis       AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-serie             AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-cod-emitente      AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-nr-seq-docto      AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-nat-operacao      AS CHARACTER   NO-UNDO.     
DEFINE VARIABLE c-num-meses         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-parcela-atual     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-valor             AS CHARACTER   NO-UNDO.
/* --------------------------------------------------------- */
FOR EACH ri-bem NO-LOCK
   WHERE ri-bem.dat-entrada  >= tt-param.dt-emissao-ini  
     AND ri-bem.dat-entrada  <= tt-param.dt-emissao-fim
     AND ri-bem.cod-estabel  >= tt-param.cod-estabel-ini 
     AND ri-bem.cod-estabel  <= tt-param.cod-estabel-fim 
     AND ri-bem.serie        >= tt-param.serie-ini       
     AND ri-bem.serie        <= tt-param.serie-fim       
     AND ri-bem.nr-nota-fis  >= tt-param.nr-nota-fis-ini 
     AND ri-bem.nr-nota-fis  <= tt-param.nr-nota-fis-fim 
     AND ri-bem.cod-emitente >= tt-param.cod-emitente-ini
     AND ri-bem.cod-emitente <= tt-param.cod-emitente-fim
    BY ri-bem.dat-entrada:

    RUN pi-acompanhar IN h-acomp(STRING(ri-bem.dat-entrada,"99/99/9999") + "-" +
                                 TRIM(ri-bem.nr-nota-fis) + "-" +
                                 TRIM(ri-bem.it-codigo)).

    FOR EACH tt-ficha:  DELETE tt-ficha. END. 

    FIND FIRST ri-bem-grupo WHERE 
               ri-bem-grupo.id-bem       = ri-bem.id-bem     AND   
               ri-bem-grupo.cod-grupo    = i-grupo-ciap      AND   
               ri-bem-grupo.data-1      <> ?                 NO-LOCK NO-ERROR.
    IF AVAIL ri-bem-grupo THEN
    DO:   
       ASSIGN c-val-imposto   = STRING(ri-bem-grupo.val-imposto)
              c-num-meses     = STRING(ri-bem-grupo.num-meses).  

       RUN pi-processa-ficha.
    END.
    
    ASSIGN iParcela      = 0
           iSeqAtual     = 0. 

    FOR EACH b-tt-ficha
          BY b-tt-ficha.sequencia:

        ASSIGN iParcela = iParcela + 1
               c-periodo        = string(b-tt-ficha.mes,"99") + "/" + string(b-tt-ficha.ano,"9999")
               c-periodo-atual  = STRING(MONTH(TODAY),"99") + "/" + STRING(YEAR(TODAY) ,"9999"). 

        IF c-periodo = c-periodo-atual THEN
           ASSIGN b-tt-ficha.parcela-atual = iParcela
                  b-tt-ficha.periodo-atual = c-periodo-atual 
                  iSeqAtual                = b-tt-ficha.sequencia.
    END.

    FIND FIRST tt-ficha WHERE
               tt-ficha.periodo-atual <> "" NO-LOCK NO-ERROR.

    ASSIGN cChaveNfe = "".

    FIND FIRST nota-fiscal WHERE
               nota-fiscal.cod-estabel = ri-bem.cod-estabel  AND
               nota-fiscal.serie       = ri-bem.serie        AND
               nota-fiscal.nr-nota-fis = ri-bem.nr-nota-fis  NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN cChaveNfe = nota-fiscal.cod-chave-aces-nf-eletro.

    /* ri-movto-bem.observacao */
    FIND FIRST ri-movto-bem WHERE 
               ri-movto-bem.cod-estabel  = ri-bem.cod-estabel AND 
               ri-movto-bem.id-bem       = ri-bem.id-bem      NO-LOCK NO-ERROR.
    IF AVAIL ri-movto-bem THEN
    DO:
       ASSIGN c-observacao     = string(ri-movto-bem.observacao    )   
              c-dat-movto      = string(ri-movto-bem.dat-movto     )   
              c-idi-tipo-movto = string(ri-movto-bem.idi-tipo-movto).   

       FIND FIRST ri-docto-movto-bem OF  ri-movto-bem NO-LOCK NO-ERROR.
       IF AVAIL ri-movto-bem THEN
       DO:
          ASSIGN c-nr-nota-fis   = string(ri-docto-movto-bem.nr-nota-fis ) 
                 c-serie         = string(ri-docto-movto-bem.serie       ) 
                 c-cod-emitente  = string(ri-docto-movto-bem.cod-emitente) 
                 c-nr-seq-docto  = string(ri-docto-movto-bem.nr-seq-docto) 
                 c-nat-operacao  = string(ri-docto-movto-bem.nat-operacao). 

          FIND FIRST mov-ciap WHERE 
                     mov-ciap.cod-estabel  =  ri-docto-movto-bem.cod-estabel   AND
                     mov-ciap.serie        =  ri-docto-movto-bem.serie         AND
                     mov-ciap.nr-doc-fis   =  ri-docto-movto-bem.nr-nota-fis   AND
                     mov-ciap.cod-emitente =  ri-docto-movto-bem.cod-emitente  AND
                     mov-ciap.nat-operacao =  ri-docto-movto-bem.nat-operacao  AND
                     mov-ciap.nr-seq-doc   =  ri-docto-movto-bem.nr-seq-docto  NO-LOCK NO-ERROR.
          IF AVAIL mov-ciap THEN
             ASSIGN c-vl-icmsco-it    =  string(mov-ciap.vl-icmsco-it  ) 
                    c-vl-icmsnt-desp  =  string(mov-ciap.vl-icmsnt-desp). 
                      
       END.
    END.

    IF AVAIL tt-ficha THEN
       ASSIGN c-parcela-atual = string(tt-ficha.parcela-atual)   
              c-periodo-atual = string(tt-ficha.periodo-atual) 
              c-valor         = string(tt-ficha.valor        ).


/*                                                                            */
/*     FIND FIRST ri-valor-bem WHERE                                          */
/*                ri-valor-bem.id-bem       = ri-bem.id-bem AND               */
/*                ri-valor-bem.cod-grupo    = i-grupo-ciap  AND               */
/*                ri-valor-bem.nr-sequencia = 10            NO-LOCK NO-ERROR. */

    IF tt-param.arquivo-excel <> "" THEN
    DO:    
       PUT  ri-bem.cod-estabel         AT 01     ";"  /*A */
            ri-bem.serie                         ";"  /*B */
            ri-bem.nr-nota-fis                   ";"  /*C */
            ri-bem.nr-seq-docto                  ";"  /*D */
            ri-bem.id-ficha-bem                  ";"  /*E */
            ri-bem.nat-operacao                  ";"  /*F */
            ri-bem.desc-bem                      ";"  /*G */
            ri-bem.cod-emitente                  ";"  /*H */
            ri-bem.nome-emit                     ";"  /*I */
            ri-bem.nr-patrimonio                 ";"  /*J */
            ri-bem.seq-patrimonio                ";"  /*K */
            ri-bem.quantidade                    ";"  /*L */
            ri-bem.un                            ";"  /*M */
            ri-bem.cd-modelo-docto               ";"  /*N */
            cChaveNfe                            ";"  /*O */
            ri-bem.cod-ncm                       ";"  /*P */
            ri-bem.dat-entrada                   ";"  /*Q */    
            ri-bem.dat-inic-cred                 ";"  /*R */    
            ri-bem.nr-ord-produ                  ";"  /*S */    
            ri-bem.num-ord-inv                   ";"  /*T */
            ri-bem.sc-codigo                     ";"  /*U */ 
            ri-bem.val-contabil                  ";"  /*V */ 
            c-vl-icmsco-it                       ";"  /*X */    
            c-vl-icmsnt-desp                     ";"  /*W */    
            c-observacao                         ";"  /*Y */    
            c-dat-movto                          ";"  /*Z */    
            c-idi-tipo-movto                     ";"  /*AA*/    
            c-nr-nota-fis                        ";"  /*AB*/    
            c-serie                              ";"  /*AC*/    
            c-cod-emitente                       ";"  /*AD*/    
            c-nr-seq-docto                       ";"  /*AE*/    
            c-nat-operacao                       ";"  /*AF*/   
            ri-bem.cod-cta-pat                   ";"  /*AG*/                                                       
            c-num-meses                          ";"  /*AH*/   
            c-parcela-atual                      ";"  /*AI*/
            c-periodo-atual                      ";"  /*AJ*/
            c-valor                              ";"  /*AK*/
            .

       
       ASSIGN c-val-imposto     = ""  
              c-vl-icmsco-it    = ""  
              c-vl-icmsnt-desp  = ""  
              c-observacao      = ""  
              c-dat-movto       = ""  
              c-idi-tipo-movto  = ""  
              c-nr-nota-fis     = ""  
              c-serie           = ""  
              c-cod-emitente    = ""  
              c-nr-seq-docto    = ""  
              c-nat-operacao    = ""
              c-num-meses       = ""
              cChaveNfe         = ""
              c-parcela-atual   = ""
              c-periodo-atual   = ""
              c-valor           = "".
    END.
    ELSE 
    DO:
        DISP ri-bem.cod-estabel
             ri-bem.serie        
             ri-bem.nr-nota-fis  
             ri-bem.nr-seq-docto 
             ri-bem.id-ficha-bem       
             ri-bem.nat-operacao 
             ri-bem.dat-entrada  
             ri-bem.desc-bem     
             ri-bem.cod-emitente 
             ri-bem.nome-emit    
             ri-bem.val-contabil 
             WITH FRAME f-dados WIDTH 300 STREAM-IO DOWN.
    END.

    ASSIGN i-contador = 0.
/*     FOR EACH ri-bem-grupo NO-LOCK OF ri-bem                       */
/*        WHERE ri-bem-grupo.cod-grupo = 1: /* ICMS */               */
/*                                                                   */
/*         ASSIGN i-contador = i-contador + 1.                       */
/*                                                                   */
/*         IF tt-param.arquivo-excel <> "" THEN DO:                  */
/*             IF i-contador = 1 THEN                                */
/*                 PUT ri-bem-grupo.val-imposto ";"                  */
/*                     ri-bem-grupo.num-meses                        */
/*                     SKIP.                                         */
/*             ELSE                                                  */
/*                 PUT ";;;;;;;;;;;" AT 01                           */
/*                     ri-bem-grupo.val-imposto ";"                  */
/*                     ri-bem-grupo.num-meses                        */
/*                     SKIP.                                         */
/*         END.                                                      */
/*         ELSE DO:                                                  */
/*                                                                   */
/*             IF i-contador = 1 THEN DO:                            */
/*                 DISP ri-bem-grupo.val-imposto                     */
/*                      ri-bem-grupo.num-meses                       */
/*                      WITH FRAME f-dados WIDTH 300 STREAM-IO DOWN. */
/*                 DOWN WITH FRAME f-dados.                          */
/*             END.                                                  */
/*             ELSE DO:                                              */
/*                 DISP "" @ ri-bem.cod-estabel                      */
/*                      "" @ ri-bem.serie                            */
/*                      "" @ ri-bem.nr-nota-fis                      */
/*                      "" @ ri-bem.nr-seq-docto                     */
/*                      "" @ ri-bem.id-ficha-bem                     */
/*                      "" @ ri-bem.nat-operacao                     */
/*                      "" @ ri-bem.dat-entrada                      */
/*                      "" @ ri-bem.desc-bem                         */
/*                      "" @ ri-bem.cod-emitente                     */
/*                      "" @ ri-bem.nome-emit                        */
/*                      "" @ ri-bem.val-contabil                     */
/*                      ri-bem-grupo.val-imposto                     */
/*                      ri-bem-grupo.num-meses                       */
/*                      WITH FRAME f-dados WIDTH 300 STREAM-IO DOWN. */
/*                 DOWN WITH FRAME f-dados.                          */
/*             END.                                                  */
/*                                                                   */
/*         END.                                                      */
/*                                                                   */
/*     END.                                                          */

    IF i-contador = 0 AND tt-param.arquivo-excel = "" THEN 
        DOWN WITH FRAME f-dados.

END.

{include/i-rpclo.i}

IF tt-param.arquivo-excel <> "" THEN DO:

    OUTPUT CLOSE.

    create "excel.application" chExcelApplication.

    chExcelApplication:workbooks:Open(c-arquivo).

    chExcelApplication:Cells:Select.
    chExcelApplication:Cells:EntireColumn:AutoFit.
    chExcelApplication:Range("a1:AK1"):horizontalAlignment=1.
    chExcelApplication:Range("a1:AK1"):interior:Color= 1 .
    chExcelApplication:Range("a1:AK1"):font:colorindex = 2.
    chExcelApplication:Range("a1:AK1"):font:bold = true.
    chExcelApplication:Range("A:AK"):numberFormat="@".
    chExcelApplication:Range("O:O"):numberFormat="99999999999999999999999999999999999999999999".
    chExcelApplication:Columns("AJ:AJ"):numberFormat="mm/aaaa".
    chExcelApplication:Columns("Q:R"):numberFormat="dd/mm/aaaa".
    chExcelApplication:Columns("Z:Z"):numberFormat="dd/mm/aaaa".

    /* chExcelApplication:Range("n:r"):numberFormat="###.###.##0,00". */
    chExcelApplication:range("A1"):SELECT.

    RUN pi-finalizar IN h-acomp.

    chExcelApplication:visible = TRUE.

    /* chExcelApplication:quit. */
    chExcelApplication:DisplayAlerts = NO.
    OS-DELETE VALUE(tt-param.arquivo-excel) NO-ERROR.
    chExcelApplication:Workbooks:Item(1):SaveAs(tt-param.arquivo-excel,51,,,FALSE,FALSE,) NO-ERROR.
    release object chExcelApplication.

    OS-DELETE VALUE(c-arquivo) NO-ERROR.
END.
ELSE
    RUN pi-finalizar IN h-acomp.

RETURN "OK":U.
/*-------------------------------------------------------------------------------------------------*/
/* Final DO Programa                                                                               */
/*-------------------------------------------------------------------------------------------------*/
PROCEDURE pi-processa-ficha:
   
    DEF VAR de-acumulado    AS DEC.
    DEF VAR de-val-contabil LIKE ri-bem.val-contabil .

    ASSIGN i-nr-meses-ciap = ri-bem-grupo.num-meses
           de-vl-credito[1] = 0
           de-vl-credito[2] = 0
           de-val-contabil  = 0
           i-mes-aux        = 0.

    FIND FIRST ri-valor-bem NO-LOCK
         WHERE ri-valor-bem.id-bem       = ri-bem.id-bem
           AND ri-valor-bem.cod-grupo    = i-grupo-ciap
           AND ri-valor-bem.nr-sequencia = 10 NO-ERROR. /*** Valor do imposto de movimento de implanta‡Æo do bem ***/

    IF AVAIL ri-valor-bem THEN
        ASSIGN de-vl-credito[1] = ri-valor-bem.val-imposto
               de-val-contabil  = ri-bem.val-contabil.

    /*** Converte o valor do cr‚dito para a moeda selecionada ***/
    RUN pi-converte-valores (INPUT  de-vl-credito[1],
                             OUTPUT de-vl-credito[2]).

    RUN pi-cria-tt-ficha (OUTPUT de-val-contabil,
                          OUTPUT de-vl-credito[2]).  

END PROCEDURE. /***  ***/
/*******************************************************************************************************************************************/                                
PROCEDURE pi-converte-valores:
     
DEF INPUT  PARAMETER de-vl-corr LIKE ri-valor-bem.val-imposto NO-UNDO.
DEF OUTPUT PARAMETER de-vl-conv LIKE ri-valor-bem.val-imposto NO-UNDO.

/*     {rip/ri9999.i "tt-param.cod-moeda" "ri-bem.dat-entr" "de-taxa"} */
/*                                                                                                 */
/*     IF  {1} <> 0 THEN DO:                                                                       */
/*                                                                                                 */
/*         FIND FIRST ri-cotacao WHERE                                                             */
/*                    ri-cotacao.mo-codigo       = {1}                                         AND */
/*                    ri-cotacao.periodo         = STRING(YEAR({2})) + STRING(MONTH({2}),"99") AND */
/*                    ri-cotacao.cotacao-mensal <> 0 NO-LOCK NO-ERROR.                             */
/*                                                                                                 */
/*         IF  AVAIL ri-cotacao THEN                                                               */
/*             ASSIGN {3} = ri-cotacao.cotacao-mensal.                                             */
/*                                                                                                 */
/*         IF  {3} = 0 THEN DO:                                                                    */
/*                                                                                                 */
/*             &if  "{5}" <> "msg"                                                                 */
/*                                                                                                 */
/*                  &then RUN utp/ut-msgs.p (INPUT "show",                                         */
/*                                           INPUT 1175,                                           */
/*                                           INPUT string({1}) + "~~" + STRING({2},"99/99/9999")). */
/*                                                                                                 */
/*                  &else RUN utp/ut-msgs.p (INPUT "msg",                                          */
/*                                           INPUT 1175,                                           */
/*                                           INPUT string({1}) + "~~" + STRING({2},"99/99/9999")). */
/*             &endif.                                                                             */
/*             {4}                                                                                 */
/*         END.                                                                                    */
/*     END.                                                                                        */
/*     ELSE DO:                                                                                    */
/*         ASSIGN {3} = 1. */
/*     END.                */
    
    ASSIGN de-vl-conv = de-vl-corr / 1.

END PROCEDURE. /***  ***/
/*******************************************************************************************************************************************/                                        
PROCEDURE pi-cria-tt-ficha:
    
    DEF OUTPUT PARAMETER de-val-contabil  LIKE ri-bem.val-contabil.
    DEF OUTPUT PARAMETER de-vl-cred-ficha AS DEC DECIMALS 2 NO-UNDO.

    DEF VAR da-data     AS DATE NO-UNDO.
    DEF VAR da-data-fim AS DATE NO-UNDO.
    DEF VAR i-nr-mes    AS INT  NO-UNDO.
    DEF VAR i-nr-parc   AS INT  NO-UNDO.
    DEF VAR d-val-imp   AS DEC  NO-UNDO.

    FOR EACH tt-ficha:
        DELETE tt-ficha.
    END.


    ASSIGN da-data   = ri-bem-grupo.data-1
           da-data   = da-data - 30
           i-nr-parc = 0.

    FOR FIRST ri-valor-bem NO-LOCK
        WHERE ri-valor-bem.id-bem    = ri-bem.id-bem
        AND   ri-valor-bem.cod-grupo = ri-bem-grupo.cod-grupo:
        ASSIGN d-val-imp = ri-valor-bem.val-imposto.
    END.

    DO  i-mes-aux = 1 to i-nr-meses-ciap:

        ASSIGN da-data = da-data + 30
               da-data = DATE(MONTH(da-data),15,YEAR(da-data)).

        ASSIGN da-data-fim = da-data + 20
               da-data-fim = da-data-fim - DAY(da-data-fim).

        CREATE tt-ficha.
        ASSIGN tt-ficha.cod-estabel = ri-bem.cod-estabel
               tt-ficha.sequencia   = i-mes-aux
               tt-ficha.mes         = MONTH(da-data)
               tt-ficha.mes-ano     = i-mes-aux MODULO 12
               tt-ficha.mes-ano     = IF tt-ficha.mes-ano = 0 
                                      THEN 12
                                      ELSE tt-ficha.mes-ano
               tt-ficha.ano         = YEAR(da-data).
                                                   
        FIND FIRST ri-fatur-mes 
            WHERE  ri-fatur-mes.cod-estabel = tt-ficha.cod-estabel 
            AND    ri-fatur-mes.mes         = tt-ficha.mes         
            AND    ri-fatur-mes.ano         = tt-ficha.ano NO-LOCK NO-ERROR.

        RUN pi-retorna-valor-credito (INPUT  ri-bem.id-bem,
                                      INPUT  da-data-fim,
                                      OUTPUT de-vl-cred-ficha,
                                      OUTPUT de-val-contabil).
       
        RUN pi-converte-valores (INPUT  de-vl-cred-ficha,
                                 OUTPUT de-vl-cred-ficha).
  
        IF  ri-bem.dt-baixa <> ?  THEN DO:
            IF YEAR(ri-bem.dt-baixa) * 12 + MONTH(ri-bem.dt-baixa) 
             < YEAR(da-data-fim    ) * 12 + MONTH(da-data-fim)
            THEN ASSIGN de-vl-cred-ficha = 0.
        END.

        IF   ri-bem.log-1 = YES AND ( YEAR(da-data-fim) * 12 + MONTH(da-data-fim) < YEAR(ri-bem.dat-entrada) * 12 + MONTH(ri-bem.dat-entrada) ) THEN DO:
             ASSIGN de-vl-cred-ficha = 0.
        END.

        IF NOT AVAIL ri-fatur-mes THEN NEXT.
        
        IF AVAIL ri-fatur-mes
        AND ri-fatur-mes.mes = 1 
        AND d-val-imp <> de-vl-cred-ficha THEN DO:

            FIND LAST ri-movto-bem NO-LOCK
                WHERE ri-movto-bem.id-bem = ri-bem.id-bem NO-ERROR.

            IF  AVAIL ri-movto-bem 
            AND AVAIL ri-bem-grupo
            AND ri-movto-bem.idi-tipo-movto = 16 THEN DO:

                /*** Calcula o prazo de quatro anos que o bem tem a partir de quando passou a tomar cr‚dito ***/
                RUN rip/ri9999.p (ri-bem-grupo.data-1, DATE(ri-fatur-mes.mes,1,ri-fatur-mes.ano), OUTPUT i-nr-parc).
            
                ASSIGN i-nr-mes = IF i-nr-parc = 0 THEN i-nr-parc ELSE (i-nr-parc - 1) 
                       d-val-imp = de-vl-cred-ficha. 
            END.
        END.

        IF AVAIL ri-bem-grupo THEN
            FIND FIRST ped-curva USE-INDEX ch-curva NO-LOCK
                WHERE ped-curva.vl-aberto = 412
                AND   ped-curva.codigo    = ri-bem-grupo.id-bem
                AND   ped-curva.tot-ped   = ri-bem-grupo.cod-grupo
                AND   ped-curva.it-codigo = STRING(i-mes-aux) NO-ERROR.

        /*IF  ri-bem.dat-inic-cred <> ? THEN*/
        ASSIGN  tt-ficha.fator = ri-fatur-mes.val-vendas-trib / ri-fatur-mes.val-vendas
                tt-ficha.fator = IF tt-ficha.fator = ? 
                                 THEN 0 
                                 ELSE TRUNCATE(tt-ficha.fator,4) /* Demonstrar o mesmo valor do RI0401 */
                tt-ficha.valor = (IF AVAIL ped-curva 
                                  THEN ped-curva.vl-lucro-br
                                  ELSE tt-ficha.fator * de-vl-cred-ficha / (i-nr-meses-ciap - i-nr-mes)) 
                de-vl-credito[2] = de-vl-cred-ficha.
    END.    
END PROCEDURE. /*  */
/*******************************************************************************************************************************************/
PROCEDURE pi-calcula-valor-credito:
     

    DEF INPUT        PARAM i-id-bem           LIKE ri-bem.id-bem            NO-UNDO.
    DEF INPUT        PARAM DA-DAT-ENTRADA-ORI LIKE RI-BEM.DAT-ENTRADA       NO-UNDO.
    DEF INPUT        PARAM da-dat-entrada     LIKE ri-bem.dat-entrada       NO-UNDO.
    DEF OUTPUT       PARAM de-vl-credito      LIKE ri-valor-bem.val-imposto NO-UNDO.

    DEF VAR da-maior-data LIKE da-dat-entrada INITIAL ?                 NO-UNDO.

    /* Considera todos os ri-valor-bem que sÆo de CIAP e que a data de entrada seja igual a data do movimento */
    ASSIGN de-vl-credito = 0.

    IF DA-DAT-ENTRADA < DA-DAT-ENTRADA-ORI THEN
        ASSIGN DA-DAT-ENTRADA = DATE(MONTH(DA-DAT-ENTRADA-ORI) + 1,1,YEAR(DA-DAT-ENTRADA-ORI)) - 1. /*** Buscar o £ltimo dia do mˆs ***/
               
    ASSIGN da-maior-data = ?.

    FOR EACH  ri-valor-bem NO-LOCK
        WHERE ri-valor-bem.id-bem    = i-id-bem
        AND   ri-valor-bem.dat-movto <= da-dat-entrada,
        FIRST ri-grupos NO-LOCK
        WHERE ri-grupos.cod-grupo = ri-valor-bem.cod-grupo
        AND   ri-grupos.log-ciap
        BY ri-valor-bem.dat-movto desc :

        ASSIGN da-maior-data = IF da-maior-data = ? 
                               THEN ri-valor-bem.dat-movto
                               ELSE IF ri-valor-bem.dat-movto > da-maior-data
                                    THEN ri-valor-bem.dat-movto
                                    ELSE da-maior-data.
    END.   
    
    FOR EACH  ri-valor-bem NO-LOCK
        WHERE ri-valor-bem.id-bem     = i-id-bem 
        AND   ri-valor-bem.dat-movto  = da-maior-data
        AND   ri-valor-bem.cod-grupo  = i-grupo-ciap,
        FIRST ri-grupos NO-LOCK
        WHERE ri-grupos.cod-grupo = ri-valor-bem.cod-grupo
        AND   ri-grupos.log-ciap
        BREAK BY ri-valor-bem.nr-sequencia desc:
        
        ASSIGN de-vl-credito = de-vl-credito + ri-valor-bem.val-imposto.

        IF LAST-OF(ri-valor-bem.nr-sequencia) THEN LEAVE.
    END.

END PROCEDURE. /*  */
/***************************************************************************************************/                                                                              
PROCEDURE pi-retorna-valor-credito:
     
    DEF INPUT  PARAMETER i-id-bem LIKE ri-bem.id-bem NO-UNDO.
    DEF INPUT  PARAMETER d-data-entrada AS DATE NO-UNDO.
    DEF OUTPUT PARAMETER de-vl-credito AS DEC NO-UNDO.
    DEF OUTPUT PARAMETER de-val-contabil AS DEC NO-UNDO.

    DEF VAR de-acumulado AS DEC NO-UNDO.

    FIND FIRST buffer-ri-bem WHERE buffer-ri-bem.id-ficha-bem = i-id-bem NO-LOCK NO-ERROR.
    IF AVAIL buffer-ri-bem THEN        
        FOR FIRST b-ri-bem NO-LOCK WHERE b-ri-bem.id-ficha-bem = i-id-bem:
        
            find first b-ri-bem-grupo
                 where b-ri-bem-grupo.id-bem = b-ri-bem.id-bem
                   and b-ri-bem-grupo.cod-grupo = i-grupo-ciap
                 no-lock no-error.
                 
            if not avail b-ri-bem-grupo then next.     
                 
            RUN pi-calcula-valor-credito (INPUT b-ri-bem.id-bem,
                                          INPUT B-RI-BEM.DAT-ENTRADA,
                                          INPUT IF d-data-entrada = ? 
                                                THEN (IF b-ri-bem.log-1 = NO 
                                                      THEN b-ri-bem-grupo.data-1
                                                      ELSE b-ri-bem.dat-entrada)
                                                ELSE d-data-entrada,
                                          OUTPUT de-vl-credito).
          
            ASSIGN de-val-contabil = de-val-contabil + b-ri-bem.val-contabil
                   de-acumulado    = de-acumulado    + de-vl-credito.
        END.
    ELSE DO:        
        FOR FIRST b-ri-bem NO-LOCK WHERE b-ri-bem.id-bem = i-id-bem:
        
            find first b-ri-bem-grupo
                 where b-ri-bem-grupo.id-bem = b-ri-bem.id-bem
                   and b-ri-bem-grupo.cod-grupo = i-grupo-ciap
                 no-lock no-error.
                 
            if not avail b-ri-bem-grupo then next.     
                 
            RUN pi-calcula-valor-credito (INPUT b-ri-bem.id-bem,
                                          INPUT B-RI-BEM.DAT-ENTRADA,
                                          INPUT IF d-data-entrada = ? 
                                                THEN (IF b-ri-bem.log-1 = NO 
                                                      THEN b-ri-bem-grupo.data-1
                                                      ELSE b-ri-bem.dat-entrada)
                                                ELSE d-data-entrada,
                                          OUTPUT de-vl-credito).
          
            ASSIGN de-val-contabil = de-val-contabil + b-ri-bem.val-contabil
                   de-acumulado    = de-acumulado    + de-vl-credito.
        END.
    END.

    ASSIGN de-vl-credito = de-acumulado.

END PROCEDURE.
/*-------------------------------------------------------------------------------------------------*/




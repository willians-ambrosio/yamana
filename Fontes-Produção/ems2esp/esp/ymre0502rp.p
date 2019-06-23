/************************************************************************
* Programa..: YMRE0502RP.P                                                *
* Descri‡Æo.: 
* AUTOR.....: Sergio Luiz Neto da Silveira (DSC Praxis)
* DATA......: 05/01/2017                                                *
              
************************************************************************/
/*
{esp/esbuffer.i}
*/

/* include de controle de versÆo */
{include/i-prgvrs.i YMRE0501RP 12.1.17.001}

/* pr‚processador para ativar ou nÆo a saðda para RTF */
&GLOBAL-DEFINE RTF NO

/* pr‚processador para setar o tamanho da p gina */
&SCOPED-DEFINE pagesize 0   
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

    define temp-table tt-param no-undo
        field destino          as integer
        field arquivo          as char format "x(35)"
        field usuario          as char format "x(12)"
        field data-exec        as date
        field hora-exec        as integer

        field classifica       as integer
        field desc-classifica  as char format "x(40)"
        field modelo-rtf       as char format "x(35)"
        field l-habilitaRtf    as LOG

        field c-estabel-ini    like doc-fiscal.cod-estabel
        field c-estabel-fim    like doc-fiscal.cod-estabel
        field c-serie-ini      like doc-fiscal.serie
        field c-serie-fim      like doc-fiscal.serie
        field i-cod-emi-ini    like doc-fiscal.cod-emitente
        field i-cod-emi-fim    like doc-fiscal.cod-emitente
        field c-doc-ini        like doc-fiscal.nr-doc-fis
        field c-doc-fim        like doc-fiscal.nr-doc-fis
        field da-dt-ini        like doc-fiscal.dt-docto 
        field da-dt-fim        like doc-fiscal.dt-docto
        field da-emi-ini       like doc-fiscal.dt-emis-doc  
        field da-emi-fim       like doc-fiscal.dt-emis-doc
        field c-pais-ini       like doc-fiscal.pais
        field c-pais-fim       like doc-fiscal.pais
        field c-estado-ini     like doc-fiscal.estado
        field c-estado-fim     like doc-fiscal.estado  
        field c-especie-ini    like doc-fiscal.esp-docto
        field c-especie-fim    like doc-fiscal.esp-docto
        field c-natoper-ini    like doc-fiscal.nat-operacao
        field c-natoper-fim    like doc-fiscal.nat-operacao
        field c-cfop-ini       as char format "9.999xxxxx"
        field c-cfop-fim       as char format "9.999xxxxx"
        FIELD conteudo         AS INTEGER        
        .
    
define temp-table tt-digita no-undo
    field esp-docto       as character format "x(03)"
    index id esp-docto.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de parƒmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

DEFINE VARIABLE p-cfa  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p-oper AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p-cfop AS CHARACTER   NO-UNDO.

DEFINE BUFFER  bemitente    FOR emitente.
DEFINE BUFFER  bestabelec   FOR estabelec.
DEFINE BUFFER  beunid-feder FOR ems2cadme.unid-feder.
DEFINE BUFFER  bunid-feder  FOR ems2cadme.unid-feder.
DEFINE BUFFER  bf-es-cfa    FOR es-cfa.

DEFINE VARIABLE p-aliq-icm AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-aliq-icm AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i-exc      AS INTEGER     NO-UNDO.

DEFINE VARIABLE h-acomp             AS HANDLE     NO-UNDO.
DEFINE VARIABLE v-arq-xlt           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chExcelApplication  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkItem          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i-linha             AS INTEGER NO-UNDO.

DEFINE VARIABLE c-observacao     LIKE doc-fiscal.observacao    NO-UNDO.
DEFINE VARIABLE c-observacao-aux AS CHARACTER FORMAT "x(100)"  NO-UNDO.

DEFINE VARIABLE c-cidade                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-bairro                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cep                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-rota                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-transp                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-placa                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE de-peso-bruto            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c-saida                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-desc-item              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-narrativa-item         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-narrativa-item-doc-est AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-obs-cfa                AS CHAR        NO-UNDO.

DEFINE VARIABLE c-cst-pis          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cst-cofins       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cst-ipi          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cst-icms         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-sefazsp          AS INTEGER     NO-UNDO.

define variable v-serie        like doc-fiscal.serie        no-undo.
define variable v-nr-doc-fis   like doc-fiscal.nr-doc-fis   no-undo.
DEFINE VARIABLE vl-frete       AS   DECIMAL                 NO-UNDO.

/* include padr’o para variÿveis de relat½rio  */
{utp/ut-glob.i}
{include/i-rpvar.i}

/*{include/i-rpout.i}*/

DEFINE VARIABLE c-file AS CHARACTER   NO-UNDO.
/* executando de forma persistente o utilitÿrio de acompanhamento */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Processando *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

IF tt-param.destino = 3 THEN DO:
   ASSIGN c-file = session:TEMP-DIRECTORY + "ymre0502_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv". 
   
   OUTPUT TO value(c-file).
END.   
   
IF tt-param.destino = 2 THEN DO:
   ASSIGN c-file = tt-param.arquivo. 
   
   FILE-INFO:FILE-NAME = c-file.
   
   
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN DO:
      OS-DELETE VALUE(c-file) NO-ERROR.
   END.
   
   OUTPUT TO VALUE(tt-param.arquivo).
END.   

   
 
 
 
 
   
   

RUN pi-le-doc-fiscal.



/* fechamento do output do relat½rio  */
 
RUN pi-finalizar IN h-acomp.




/*{include/i-rpclo.i}*/

OUTPUT CLOSE.

IF tt-param.destino = 3 THEN 
   RUN Pi-GeraExcel.

/* /*IF tt-param.destino = 2 THEN */
/*     RETURN "OK":U.*/ */
/*    */
/* IF tt-param.destino = 3 THEN */
/*     RETURN c-file. */




PROCEDURE Pi-GeraExcel:

    CREATE "Excel.Application" chExcelApplication.
    chExcelApplication:Visible = true.
    chExcelApplication:Workbooks:OpenText(c-file, , , , , , TRUE ).
    chWorkSheet = chExcelApplication:Sheets:Item(1).
/*     chWorkSheet:rows("1:1"):Select().        */
/*     chExcelApplication:selection:INSERT(1).  */
                                                           
    /* Altera a largura da coluna de acordo com o tamanho do seu conteudo */
    chExcelApplication:Cells:Select.
    chExcelApplication:Cells:EntireColumn:AutoFit.

    chWorkSheet:Range("A2"):Select(). 

/* *******************************/                                                     

    RELEASE OBJECT chExcelApplication.
/*     RELEASE OBJECT chWorkbook. */
    RELEASE OBJECT chWorksheet.



END PROCEDURE.

PROCEDURE Pi-GeraExcel2:
DEFINE VARIABLE c-local AS CHARACTER   NO-UNDO.

ASSIGN c-local = session:TEMP-DIRECTORY + "temp-ymre0501.csv".

    CREATE "Excel.Application" chExcelApplication.
    chExcelApplication:Visible = true.
    chWorkSheet = chExcelApplication:Sheets:Item(1).
    chExcelApplication:Workbooks:OpenText(c-local, , , , , , TRUE ).
/*     chWorkSheet:rows("1:1"):Select().        */
/*     chExcelApplication:selection:INSERT(1).  */
                                                           
    /* Altera a largura da coluna de acordo com o tamanho do seu conteudo */
    chExcelApplication:Cells:Select.
    chExcelApplication:Cells:EntireColumn:AutoFit.

    chWorkSheet:Range("A2"):Select(). 

/* *******************************/                                                     

    RELEASE OBJECT chExcelApplication.
/*     RELEASE OBJECT chWorkbook. */
    RELEASE OBJECT chWorksheet.



END PROCEDURE.

PROCEDURE pi-le-doc-fiscal:
    DEFINE BUFFER b_item-doc-est FOR item-doc-est.
    
    

    PUT "Estab"
        ";Num. Docto"
        ";Esp. Doc"
        ";Serie"
        ";Dt Emissao"
        ";Dt Docto"
        ";Nat Operacao"
        ";Desc. Nat Oper"
        ";CFOP Entrada"
        ";CFOP Sa¡da"
        ";Cod Emitente"
        ";Nome Abrev Emitente"
        ";Nome Emitente"
        ";CPF-CNPJ"
        ";Tipo Emitente"
        ";Cidade"
        ";UF"
        ";Pais"
        ";Seq Docto"
        ";Cod Item"
        ";Descricao Item"
        ";Narrativa Item"
        ";UM"
        ";Qtde"
        ";NCM"
        ";Valor Liq Merc"
        ";Valor Frete"
        ";Desc Liq"
        ";Vl Total Item"
        ";Base PIS"
        ";Aliq. Pis"
        ";Valor PIS"
        ";CST Pis"
        ";Base Cofins"
        ";Aliq Cofins"
        ";Val Cofins"
        ";CST Cofins"
        ";Base IPI ITem"
        ";Aliq IPI Item"
        ";Valor IPI Item"
        ";Valor IPI Outras"
        ";Valor IPI Outras Rec"
        ";Valor IPI NÆo Trib."
        ";CST IPI Item"
        ";Base ICMS Item"
        ";Aliq ICMS Item"
        ";Vl ICMS Item"
        ";Vl ICMS Outras"
        ";Vl ICMS NÆo Trib."
        ";Base ICMS ST"
        ";% ICMS ST"
        ";VL ICMS ST"
        ";% ICMS Compl"
        ";Vl ICMS Compl"
        ";CST ICMS"
        ";Vl Contabil"
        ";Tipo Doc"
        ";Situacao"
        ";Num Pedido"
        ";Narrativa Item Doc Entr"
        ";Al¡quota COFINS Pago"
        ";COFINS Pago"
        ";Al¡quota PIS Pago"
        ";PIS Pago"
        ";Chave de Acesso NF-e"
        ";Conta Cont bil"
        ";Centro de Custo"
        ";CFA"
        ";Benef¡cio ICMS"
        ";% Redu‡Æo"
        ";Redu‡Æo BC"
        ";NÆo incide"
        ";Benef¡cio PIS/COFINS"
        ";Benef. PIS/COFINS Compra Nacional"
        ";% Difal recebimento"
        ";Reg. Tributa‡Æo (Normal/Simples Nacional)" 
        ";Benef¡cio ICMS recebimento"
        ";Benef¡cio PIS/COFINS  recebimento"
        ";BC ICMS - XML"
        ";%Reduc ICMS - XML"
        ";Aliq ICMS - XML"
        ";Vl ICMS - XML"
        ";BC ICMS ST - XML"
        ";MVA - XML"
        ";ALIQ ST - XML"
        ";VL ST - XML"
        ";BC IPI - XML"
        ";ALIQ IPI - XML"
        ";VAL IPI - XML"
        .
        PUT SKIP.

    FIND FIRST es-natoper-rec 
        WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario
        NO-LOCK NO-ERROR.

    FOR EACH doc-fiscal NO-LOCK
       WHERE doc-fiscal.cod-estabel  >= tt-param.c-estabel-ini 
         AND doc-fiscal.cod-estabel  <= tt-param.c-estabel-fim 
         AND doc-fiscal.serie        >= tt-param.c-serie-ini  
         AND doc-fiscal.serie        <= tt-param.c-serie-fim  
         AND doc-fiscal.nr-doc-fis   >= tt-param.c-doc-ini    
         AND doc-fiscal.nr-doc-fis   <= tt-param.c-doc-fim    
         AND doc-fiscal.nat-operacao >= tt-param.c-natoper-ini
         AND doc-fiscal.nat-operacao <= tt-param.c-natoper-fim
         AND doc-fiscal.cod-emitente >= tt-param.i-cod-emi-ini
         AND doc-fiscal.cod-emitente <= tt-param.i-cod-emi-fim
         AND doc-fiscal.dt-emis-doc  >= tt-param.da-emi-ini   
         AND doc-fiscal.dt-emis-doc  <= tt-param.da-emi-fim   
         AND doc-fiscal.dt-docto     >= tt-param.da-dt-ini   
         AND doc-fiscal.dt-docto     <= tt-param.da-dt-fim
         AND doc-fiscal.pais         >= tt-param.c-pais-ini
         AND doc-fiscal.pais         <= tt-param.c-pais-fim
         AND doc-fiscal.estado       >= tt-param.c-estado-ini
         AND doc-fiscal.estado       <= tt-param.c-estado-fim
         AND doc-fiscal.esp-docto    >= tt-param.c-especie-ini
         AND doc-fiscal.esp-docto    <= tt-param.c-especie-fim
         AND doc-fiscal.cod-cfop     >= tt-param.c-cfop-ini
         AND doc-fiscal.cod-cfop     <= tt-param.c-cfop-fim
        ,EACH it-doc-fisc OF doc-fiscal NO-LOCK
        
               BREAK 
                  BY doc-fiscal.cod-estabel 
                  BY doc-fiscal.serie       
                  BY doc-fiscal.nr-doc-fis  
                  BY doc-fiscal.cod-emitente
                  BY doc-fiscal.nat-operacao
                  BY it-doc-fisc.it-codigo:

        /*
        IF it-doc-fisc.nr-seq-doc <> es-item-doc-est-natoper.sequencia THEN DO:
            
            IF it-doc-fisc.nr-seq-doc <> es-item-doc-est-natoper.sequencia-it-doc-fisc THEN DO:
                
                NEXT.
            END.
                
        END.
        */

        RUN pi-acompanhar IN h-acomp (INPUT "Documento : " + doc-fiscal.cod-estabel + " " + doc-fiscal.serie + " " + doc-fiscal.nr-doc-fis + " " + doc-fiscal.nat-operacao).

        FIND ITEM WHERE ITEM.it-codigo = it-doc-fisc.it-codigo NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN NEXT.

        /* Somente naturezas Placebo */
        IF tt-param.conteudo = 2 AND 
           (NOT AVAIL es-natoper-rec  OR 
           (es-natoper-rec.nat-operacao-est <> doc-fiscal.nat-operacao AND
            es-natoper-rec.nat-operacao-int <> doc-fiscal.nat-operacao))THEN
            NEXT.
        

        FIND FIRST item-dist OF ITEM NO-LOCK NO-ERROR.
        ASSIGN i-sefazsp =  IF AVAIL item-dist THEN item-dist.cdn-sefazsp ELSE 0.

        IF TEMP-TABLE tt-digita:HAS-RECORDS = YES THEN
        DO:
           IF NOT CAN-FIND (FIRST tt-digita WHERE tt-digita.esp-docto = doc-fiscal.esp-docto NO-LOCK) THEN NEXT.
        END.

        ASSIGN v-serie      = ""
               v-nr-doc-fis = ""
               c-observacao = ""
               c-observacao-aux = "".

        ASSIGN c-cidade        = ""
               c-bairro        = ""
               c-cep           = ""
               c-rota          = ""
               c-transp        = ""
               c-placa         = ""
               de-peso-bruto   = 0
               c-saida         = ""
               vl-frete        = 0.

        FOR FIRST es-item-doc-est-natoper NO-LOCK
            WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
            AND   es-item-doc-est-natoper.serie-docto  = doc-fiscal.serie
            AND   es-item-doc-est-natoper.nro-docto    = doc-fiscal.nr-doc-fis
            AND   es-item-doc-est-natoper.nat-operacao = doc-fiscal.nat-operacao
            AND   es-item-doc-est-natoper.cod-emitente = doc-fiscal.cod-emitente
            AND   es-item-doc-est-natoper.sequencia-it-doc-fisc = it-doc-fisc.nr-seq-doc:
        END.

        IF doc-fiscal.ind-ori-doc = 1 THEN /* FATURAMENTO */
        DO:
            FIND nota-fiscal WHERE nota-fiscal.cod-estabel = doc-fiscal.cod-estabel
                               AND nota-fiscal.serie       = doc-fiscal.serie      
                               AND nota-fiscal.nr-nota-fis = doc-fiscal.nr-doc-fis  NO-LOCK NO-ERROR.

            IF AVAIL nota-fiscal THEN DO:
               FIND FIRST devol-cli use-index ch-nfe WHERE devol-cli.cod-estabel  = nota-fiscal.cod-estabel 
                                                       AND devol-cli.serie-docto  = nota-fiscal.serie       
                                                       AND devol-cli.nro-docto    = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.

               ASSIGN c-cidade      = nota-fiscal.cidade
                      c-bairro      = nota-fiscal.bairro
                      c-cep         = nota-fiscal.cep
                      c-rota        = nota-fiscal.cod-rota
                      c-transp      = nota-fiscal.nome-transp
                      c-placa       = nota-fiscal.placa
                      de-peso-bruto = nota-fiscal.peso-bru-tot
                      c-saida       = STRING(nota-fiscal.dt-saida,"99/99/9999").
            END.

            IF AVAIL devol-cli THEN
               ASSIGN v-serie      = devol-cli.serie      
                      v-nr-doc-fis = devol-cli.nr-nota-fis.

        END.
        ELSE IF doc-fiscal.ind-ori-doc = 2 THEN /* RECEBIMENTO */
        DO:
            FIND docum-est WHERE docum-est.serie-docto  = doc-fiscal.serie
                             AND docum-est.nro-docto    = doc-fiscal.nr-doc-fis  
                             AND docum-est.cod-emitente = doc-fiscal.cod-emitente
                             AND docum-est.nat-operacao = doc-fiscal.nat-operacao NO-LOCK NO-ERROR.

            IF AVAIL docum-est THEN DO:
               FIND FIRST item-doc-est OF docum-est NO-LOCK NO-ERROR.

               ASSIGN c-cidade      = docum-est.cidade
                      c-bairro      = docum-est.bairro
                      c-cep         = docum-est.cep
                      c-rota        = ""
                      c-transp      = docum-est.nome-transp
                      c-placa       = docum-est.cod-placa[1]
                      de-peso-bruto = docum-est.peso-bruto-tot.

            END.

            IF AVAIL item-doc-est THEN DO:
                ASSIGN vl-frete = item-doc-est.valor-frete.

                FIND it-nota-fisc WHERE it-nota-fisc.cod-estabel = docum-est.cod-estabel
                                    AND it-nota-fisc.serie       = item-doc-est.serie-comp
                                    AND it-nota-fisc.nr-nota-fis = item-doc-est.nro-comp
                                    AND it-nota-fisc.nr-seq-fat  = item-doc-est.seq-comp
                                    AND it-nota-fisc.it-codigo   = item-doc-est.it-codigo NO-LOCK NO-ERROR.

                IF AVAIL it-nota-fisc THEN
                   ASSIGN v-serie      = it-nota-fisc.serie
                          v-nr-doc-fis = it-nota-fisc.nr-nota-fis
                          vl-frete     = it-nota-fisc.vl-frete-it.
            END.
        END.

        FIND FIRST natur-oper OF doc-fiscal NO-LOCK NO-ERROR.

        FIND FIRST emitente WHERE emitente.cod-emitente = doc-fiscal.cod-emitente NO-LOCK NO-ERROR.

        PUT UNFORMATTED 
            "'"  doc-fiscal.cod-estabel   /* Estabelecimento                   */
            ";'" doc-fiscal.nr-doc-fis   /* Numero do Documento               */
            ";'" doc-fiscal.esp-docto
            ";'" doc-fiscal.serie        /* S²rie                             */
            ";"  doc-fiscal.dt-emis-doc  /* Data emissÆo                      */
            ";"  doc-fiscal.dt-docto     /* Data documento                    */
            ";'" doc-fiscal.nat-operacao /* Natureza de opera‡Æo do documento */
            ";'" replace(natur-oper.denominacao,CHR(10)," ")  /* Natureza de opera‡Æo do documento */
            ";'" doc-fiscal.cod-cfop     /* CFOP Entrada */
            ";'" IF available(es-item-doc-est-natoper) THEN es-item-doc-est-natoper.cod-cfop  ELSE ""   /* CFOP Sa¡da */
            ";"  doc-fiscal.cod-emitente /* Codigo do emitente                */
            ";'" doc-fiscal.nome-ab-emi  /* Nome abreviado do emitente        */

        /* Adicionar aqui RVP */
            ";'" emitente.nome-emit
            ";'" emitente.cgc .

        IF emitente.identific = 1 THEN
           PUT UNFORMATTED ";'Cliente".
        
        IF emitente.identific = 2 THEN
           PUT UNFORMATTED ";'Fornecedor".
        
        IF emitente.identific = 3 THEN
           PUT UNFORMATTED ";'Ambos".
            
         /*    */

        PUT UNFORMATTED    
            ";'" doc-fiscal.cidade       
            ";'" doc-fiscal.estado       
            ";'" doc-fiscal.pais         
            .

        ASSIGN c-desc-item = ITEM.desc-item
               c-desc-item = replace(c-desc-item,CHR(10),"")
               c-desc-item = replace(c-desc-item,CHR(11),"")
               c-desc-item = replace(c-desc-item,CHR(12),"")
               c-desc-item = replace(c-desc-item,CHR(13),"")
               c-desc-item = replace(c-desc-item,";","")
               c-desc-item = substring(c-desc-item,1,100).
        ASSIGN c-narrativa-item = ITEM.narrativa
               c-narrativa-item = replace(c-narrativa-item,CHR(10),"")
               c-narrativa-item = replace(c-narrativa-item,CHR(11),"")
               c-narrativa-item = replace(c-narrativa-item,CHR(12),"")
               c-narrativa-item = replace(c-narrativa-item,CHR(13),"")
               c-narrativa-item = replace(c-narrativa-item,";","")
               c-narrativa-item = substring(c-narrativa-item,1,100).

        ASSIGN c-narrativa-item = replace(c-narrativa-item,CHR(10),"")
               c-narrativa-item = replace(c-narrativa-item,'"','').


        PUT UNFORMATTED 
           ";"  it-doc-fisc.nr-seq-doc    /* Seq Docto                         */ /* (*) */
           ";'" it-doc-fisc.it-codigo     /* Item                              */ /* (*) */
           ";'" c-desc-item              /* desc Item                              */ /* (*) */
           ";'" c-narrativa-item         /* narrativa Item                              */ /* (*) */
           ";'" it-doc-fisc.un           /* Quantidade                        */ /* (*) */
           ";"  it-doc-fisc.quantidade   /* Quantidade                        */ /* (*) */
           ";'" it-doc-fisc.class-fiscal /* NCM                               */ /* (*) */
            .

        PUT UNFORMATTED  
            ";" it-doc-fisc.vl-merc-liq /* Valor mercadoria                  */ /* (#) */
            ";" vl-frete /* VL Frete*/
            .
                
        IF doc-fiscal.ind-ori-doc = 2 THEN DO: /* RECEBIMENTO */
          FIND FIRST b_item-doc-est NO-LOCK
               WHERE b_item-doc-est.cod-emitente = it-doc-fisc.cod-emitente
                 AND b_item-doc-est.serie-docto  = it-doc-fisc.serie
                 AND b_item-doc-est.nro-docto    = it-doc-fisc.nr-doc-fis
                 AND b_item-doc-est.it-codigo    = it-doc-fisc.it-codigo
                 AND b_item-doc-est.nat-operacao = it-doc-fisc.nat-operacao
                 AND b_item-doc-est.sequencia    = it-doc-fisc.nr-seq-doc
                 NO-ERROR.

          IF AVAIL b_item-doc-est THEN
             PUT UNFORMATTED
                 ";" b_item-doc-est.desconto[1].
          ELSE 
              PUT UNFORMATTED ";" 0.
        END.
        ELSE
            PUT UNFORMATTED ";" 0.

        ASSIGN c-cst-pis = "".
        FIND FIRST dwf-docto-item-impto NO-LOCK
             WHERE dwf-docto-item-impto.cod-estab         = it-doc-fisc.cod-estabel 
               AND dwf-docto-item-impto.cod-serie         = it-doc-fisc.serie       
               AND dwf-docto-item-impto.cod-docto         = it-doc-fisc.nr-doc-fis  
               AND dwf-docto-item-impto.cod-emitente      = STRING(it-doc-fisc.cod-emitente)
               AND dwf-docto-item-impto.cod-natur-operac  = it-doc-fisc.nat-operacao
               AND dwf-docto-item-impto.num-seq-item      = it-doc-fisc.nr-seq-doc
               AND dwf-docto-item-impto.cod-impto         = "PIS"
               NO-ERROR.
        ASSIGN c-cst-pis = IF AVAIL dwf-docto-item-impto THEN dwf-docto-item-impto.cod-tributac ELSE "Nao Integrado".

        ASSIGN c-cst-cofins = "".
        FIND FIRST dwf-docto-item-impto NO-LOCK
             WHERE dwf-docto-item-impto.cod-estab         = it-doc-fisc.cod-estabel 
               AND dwf-docto-item-impto.cod-serie         = it-doc-fisc.serie       
               AND dwf-docto-item-impto.cod-docto         = it-doc-fisc.nr-doc-fis  
               AND dwf-docto-item-impto.cod-emitente      = STRING(it-doc-fisc.cod-emitente)
               AND dwf-docto-item-impto.cod-natur-operac  = it-doc-fisc.nat-operacao
               AND dwf-docto-item-impto.num-seq-item      = it-doc-fisc.nr-seq-doc
               AND dwf-docto-item-impto.cod-impto         = "COFINS"
               NO-ERROR.
        ASSIGN c-cst-cofins = IF AVAIL dwf-docto-item-impto THEN dwf-docto-item-impto.cod-tributac ELSE "Nao Integrado".

        PUT UNFORMATTED 
            ";" it-doc-fisc.vl-tot-item  /* Valor total                      */ /* (#) */ 
            ";" it-doc-fisc.val-base-calc-pis
            ";" DEC(SUBSTRING(it-doc-fisc.char-2,21,5)) /* PIS */
            ";" it-doc-fisc.val-pis
            ";" c-cst-pis
            ";" it-doc-fisc.val-base-calc-cofins
            ";" DEC(SUBSTRING(it-doc-fisc.char-2,29,5)) /* COFINS */
            ";" it-doc-fisc.val-cofins          
            ";" c-cst-cofins
            .

        ASSIGN c-cst-ipi = "".
        FIND FIRST dwf-docto-item-impto NO-LOCK
             WHERE dwf-docto-item-impto.cod-estab         = it-doc-fisc.cod-estabel 
               AND dwf-docto-item-impto.cod-serie         = it-doc-fisc.serie       
               AND dwf-docto-item-impto.cod-docto         = it-doc-fisc.nr-doc-fis  
               AND dwf-docto-item-impto.cod-emitente      = STRING(it-doc-fisc.cod-emitente)
               AND dwf-docto-item-impto.cod-natur-operac  = it-doc-fisc.nat-operacao
               AND dwf-docto-item-impto.num-seq-item      = it-doc-fisc.nr-seq-doc
               AND dwf-docto-item-impto.cod-impto         = "IPI"
               NO-ERROR.
        ASSIGN c-cst-ipi = IF AVAIL dwf-docto-item-impto THEN dwf-docto-item-impto.cod-tributac ELSE "Nao Integrado".

        PUT UNFORMATTED 
            ";" it-doc-fisc.vl-bipi-it   /* Base IPI                         */ 
            ";" it-doc-fisc.aliquota-ipi /* Aliq.IPI                         */ 
            ";" it-doc-fisc.vl-ipi-it    /* Valor IPI Tribut                 */ 
            ";" it-doc-fisc.vl-ipiou-it  /* Valor IPI Outras                 */ 
            ";" (IF AVAILABLE b_item-doc-est THEN b_item-doc-est.valor-ipi[1] ELSE 0) /* Valor IPI Outras Recebimento */
            ";" it-doc-fisc.vl-ipint-it  /* Valor IPI Isentas                */ 
            ";" c-cst-ipi
            .

        PUT UNFORMATTED 
            ";" it-doc-fisc.vl-bicms-it  /* Base ICMS                        */ 
            ";" it-doc-fisc.aliquota-icm /* Aliq.ICMS                        */ 
            ";" it-doc-fisc.vl-icms-it   /* Valor ICMS Tribut                */ 
            ";" it-doc-fisc.vl-icmsou-it /* Valor ICMS Outras                */ 
            ";" it-doc-fisc.vl-icmsnt-it /* Valor ICMS Isentas               */ 
            .

        ASSIGN c-cst-icms = "".
        FIND FIRST dwf-docto-item-impto NO-LOCK
             WHERE dwf-docto-item-impto.cod-estab         = it-doc-fisc.cod-estabel 
               AND dwf-docto-item-impto.cod-serie         = it-doc-fisc.serie       
               AND dwf-docto-item-impto.cod-docto         = it-doc-fisc.nr-doc-fis  
               AND dwf-docto-item-impto.cod-emitente      = STRING(it-doc-fisc.cod-emitente)
               AND dwf-docto-item-impto.cod-natur-operac  = it-doc-fisc.nat-operacao
               AND dwf-docto-item-impto.num-seq-item      = it-doc-fisc.nr-seq-doc
               AND dwf-docto-item-impto.cod-impto         = "ICMS"
               NO-ERROR.
        ASSIGN c-cst-icms = IF AVAIL dwf-docto-item-impto THEN dwf-docto-item-impto.cod-tributac ELSE "Nao Integrado".

        PUT UNFORMATTED 
            ";" it-doc-fisc.vl-bsubs-it     /* Base ICMS-ST                     */ /* (#) */
            ";999"                          /* % ICMS ST */
            ";" it-doc-fisc.vl-icmsub-it    /* Valor ICMS-ST                    */ /* (#) */
            ";999"                          /* % ICMS Compl */
            ";" it-doc-fisc.vl-icms-comp    /* ICMS Complementar                */
            ";" c-cst-icms
            ";" doc-fiscal.vl-cont-doc      /* Valor contÿbil */
            ";'" ENTRY (doc-fiscal.tipo-nat,"Entrada,Saida,Servico")         /* 1- entrada ; 2- Saida ; 3- Servi‡o */
            ";'" ENTRY (doc-fiscal.ind-sit-doc,"Normal,Cancelado,Incompleto")
            .

        /* bno - 20/08/2013 mais ipi */
        ASSIGN c-narrativa-item-doc-est = (IF AVAILABLE b_item-doc-est THEN b_item-doc-est.narrativa ELSE "")
               c-narrativa-item-doc-est = replace(c-narrativa-item-doc-est,CHR(10),"")
               c-narrativa-item-doc-est = replace(c-narrativa-item-doc-est,CHR(11),"")
               c-narrativa-item-doc-est = replace(c-narrativa-item-doc-est,CHR(12),"")
               c-narrativa-item-doc-est = replace(c-narrativa-item-doc-est,CHR(13),"")
               c-narrativa-item-doc-est = replace(c-narrativa-item-doc-est,";","")
               c-narrativa-item-doc-est = substring(c-narrativa-item-doc-est,1,500)
               c-narrativa-item-doc-est = replace(c-narrativa-item-doc-est,'"','').

        IF AVAILABLE b_item-doc-est THEN
            PUT UNFORMATTED 
                ";" b_item-doc-est.num-pedido
                .
        ELSE 
            PUT UNFORMATTED 
                ";".

        PUT UNFORMATTED 
            ";'" c-narrativa-item-doc-est.

        PUT UNFORMATTED 
            ";" /* Al¡quota COFINS Pago */
            ";" /* COFINS Pago */
            ";" /* Al¡quota PIS Pago */
            ";" /* PIS Pago */
            ";" /* Chave de Acesso NF-e */
            .

/*         IF LENGTH(it-doc-fisc.conta-contabil) > 14 THEN           */
/*             PUT UNFORMATTED                                       */
/*                 ";'" substring(it-doc-fisc.conta-contabil,01,08)  */
/*                 ";'" substring(it-doc-fisc.conta-contabil,11,06). */
/*         ELSE                                                      */
/*             PUT UNFORMATTED                                       */
/*                 ";'" substring(it-doc-fisc.conta-contabil,01,08)  */
/*                 ";'" substring(it-doc-fisc.conta-contabil,09,06). */

        PUT UNFORMATTED 
            ";'" it-doc-fisc.ct-codigo
            ";'" it-doc-fisc.sc-codigo.

        /* Verificar benef¡cios */

        RELEASE es-cfa NO-ERROR.

        FOR FIRST bestabelec FIELDS (estado pais ep-codigo)
            WHERE bestabelec.cod-estabel = docum-est.cod-estabel NO-LOCK: 
        END.

        FOR FIRST bunid-feder FIELDS(per-icms-int estado)
            WHERE bunid-feder.pais   = bestabelec.pais
              AND bunid-feder.estado = bestabelec.estado NO-LOCK:
        END.

        ASSIGN p-cfa     = ""
               c-obs-cfa = "".

        FOR FIRST ext-item-cfa FIELDS(classe)
            WHERE ext-item-cfa.it-codigo = it-doc-fisc.it-codigo
              AND ext-item-cfa.ep-codigo = bestabelec.ep-codigo NO-LOCK:
            ASSIGN p-cfa = ext-item-cfa.classe.
        END.

        IF  AVAILABLE(es-item-doc-est-natoper) 
        AND es-item-doc-est-natoper.classe <> "" THEN
            FIND FIRST es-cfa
                 WHERE es-cfa.classe = es-item-doc-est-natoper.classe 
                 NO-LOCK NO-ERROR.

        IF  NOT AVAIL es-cfa 
        AND p-cfa <> "" THEN 
            FIND FIRST bf-es-cfa
                  WHERE bf-es-cfa.classe = p-cfa 
                  NO-LOCK NO-ERROR.
        
        PUT UNFORMATTED
            ";" 
            IF  AVAIL es-cfa                          THEN es-cfa.classe                  + "-" + es-cfa.descricao    ELSE 
            IF  AVAIL es-item-doc-est-natoper
            AND es-item-doc-est-natoper.classe  <> "" THEN es-item-doc-est-natoper.classe + "-Nao Cadastrada"         ELSE
            IF  AVAIL bf-es-cfa                       THEN bf-es-cfa.classe               + "-" + bf-es-cfa.descricao ELSE 
            IF  p-cfa                           <> "" THEN p-cfa                                                      ELSE "".


        ASSIGN c-obs-cfa = IF  NOT AVAIL es-item-doc-est-natoper
                           OR  es-item-doc-est-natoper.classe = "" THEN "DOCTO SEM CFA" ELSE "".

        IF  available(es-item-doc-est-natoper) 
        AND es-item-doc-est-natoper.cod-beneficio-icms <> 0 THEN DO:

            FOR FIRST estabelec FIELDS (estado cod-estabel)
                WHERE estabelec.cod-estabel = it-doc-fisc.cod-estabel NO-LOCK: END.
            FIND FIRST es-beneficio
                WHERE es-beneficio.cod-beneficio = es-item-doc-est-natoper.cod-beneficio-icms NO-LOCK NO-ERROR.
            PUT UNFORMATTED
                ";" IF AVAIL es-beneficio THEN STRING(es-beneficio.cod-beneficio) + "-" + es-beneficio.desc-beneficio
                                          ELSE STRING(es-item-doc-est-natoper.cod-beneficio-icms).
            FIND FIRST es-ben-estab
                WHERE es-ben-estab.cod-estabel   = it-doc-fisc.cod-estabel
                  AND es-ben-estab.cod-beneficio = es-item-doc-est-natoper.cod-beneficio-icms NO-LOCK NO-ERROR.
            IF AVAIL es-ben-estab THEN
                PUT UNFORMATTED
                    ";" IF estabelec.estado = doc-fiscal.estado THEN es-ben-estab.icms-ben-icms-est-perc-redu ELSE es-ben-estab.icms-ben-icms-inter-perc-redu /* % Redu‡Æo */
                    ";" IF estabelec.estado = doc-fiscal.estado THEN es-ben-estab.icms-ben-icms-est-red-bc    ELSE es-ben-estab.icms-ben-icms-inter-red-bc    /* Redu‡Æo BC */
                    ";" IF estabelec.estado = doc-fiscal.estado THEN STRING(es-ben-estab.icms-ben-icms-est-nao-incide,"Sim/NÆo") ELSE STRING(es-ben-estab.icms-ben-icms-inter-nao-incide,"Sim/NÆo") /* NÆo incide */
                    .
            ELSE
                PUT UNFORMATTED
                    ";"  /* % Redu‡Æo */
                    ";"  /* Redu‡Æo BC */
                    ";". /* NÆo incide */
        END.
        ELSE
            PUT UNFORMATTED
                ";"  /* Benef¡cio ICMS */
                ";"  /* % Redu‡Æo */
                ";"  /* Redu‡Æo BC */
                ";". /* NÆo incide */

        IF available(es-item-doc-est-natoper) AND es-item-doc-est-natoper.cod-beneficio-piscof <> 0 THEN DO:
            FIND FIRST es-beneficio
                WHERE es-beneficio.cod-beneficio = es-item-doc-est-natoper.cod-beneficio-piscof NO-LOCK NO-ERROR.
            PUT UNFORMATTED
                ";" IF AVAIL es-beneficio THEN STRING(es-beneficio.cod-beneficio) + "-" + es-beneficio.desc-beneficio
                                          ELSE STRING(es-item-doc-est-natoper.cod-beneficio-piscof).
            FIND FIRST es-ben-estab
                WHERE es-ben-estab.cod-estabel   = it-doc-fisc.cod-estabel
                  AND es-ben-estab.cod-beneficio = es-item-doc-est-natoper.cod-beneficio-piscof NO-LOCK NO-ERROR.
            IF AVAIL es-ben-estab THEN
                PUT UNFORMATTED
                    ";" STRING(es-ben-estab.pis-cofins-ben-pis-cof-compr-nac,"Sim/NÆo") /* Benef. PIS/COFINS Compra Nacional */
                    .
            ELSE
                PUT UNFORMATTED
                    ";". /* Benef. PIS/COFINS Compra Nacional */
            
        END.
        ELSE
            PUT UNFORMATTED
                ";"  /* Benef¡cio PIS/COFINS */
                ";". /* Benef. PIS/COFINS Compra Nacional */

        IF AVAIL(docum-est) THEN
            FIND FIRST nfe-nota-fiscal-rec NO-LOCK 
                 WHERE nfe-nota-fiscal-rec.chave-acesso-nfe = docum-est.cod-chave-aces-nf-eletro /*ELSE doc-fiscal.cod-chave-aces-nf-eletro*/  NO-ERROR.

        IF AVAILABLE(es-item-doc-est-natoper) THEN
           FIND FIRST nfe-it-nota-fisc-rec NO-LOCK                                                 
                WHERE nfe-it-nota-fisc-rec.chave-acesso-nfe     = docum-est.cod-chave-aces-nf-eletro                           
                  AND nfe-it-nota-fisc-rec.seq-item             = INT(es-item-doc-est-natoper.sequencia / 10) NO-ERROR.

        FOR FIRST bestabelec FIELDS (estado pais ep-codigo)
            WHERE bestabelec.cod-estabel = docum-est.cod-estabel NO-LOCK: END.
        FOR FIRST bunid-feder FIELDS(per-icms-int estado)
            WHERE bunid-feder.pais   = bestabelec.pais
              AND bunid-feder.estado = bestabelec.estado NO-LOCK:
        END.



        FOR FIRST ext-item-cfa FIELDS(classe)
            WHERE ext-item-cfa.it-codigo = it-doc-fisc.it-codigo
              AND ext-item-cfa.ep-codigo = bestabelec.ep-codigo NO-LOCK:
            ASSIGN p-cfa = ext-item-cfa.classe.
        END.


        IF emitente.pais     = "Brasil"         AND
           bestabelec.estado <> emitente.estado THEN
            ASSIGN p-oper = "I".
        ELSE
            ASSIGN p-oper = "E".

            IF AVAIL(nfe-it-nota-fisc-rec) THEN
                ASSIGN p-cfop = string(nfe-it-nota-fisc-rec.item-CFOP).


            ASSIGN d-aliq-icm = 0.

        IF  p-oper = "I" AND 
           (LOOKUP(p-cfa,es-natoper-rec.cod-cfa-aplic,";") > 0) AND
            /* bno - 30/07/2015 */
            NOT LOGICAL (INDEX (es-natoper-rec.excec-difal-cfop,p-cfop))
            /* bno - 30/07/2015 */
            THEN DO:


            ASSIGN p-aliq-icm = it-doc-fisc.aliquota-icm.

                IF AVAIL(nfe-nota-fiscal-rec) THEN DO:
                    IF p-aliq-icm = 0 THEN DO:
                        FOR FIRST beunid-feder FIELDS(per-icms-int est-exc perc-exc per-icms-ext)
                        WHERE beunid-feder.pais   = emitente.pais
                          AND beunid-feder.estado = emitente.estado NO-LOCK:
            
                            /*ASSIGN p-aliq-icm = beunid-feder.per-icms-int.*/
            
                            DO i-exc = 1 TO 25:
                                IF beunid-feder.est-exc[i-exc] = bestabelec.estado /*bunid-feder.estado*/ THEN
                                    ASSIGN p-aliq-icm = beunid-feder.perc-exc[i-exc].
                            END.
            
                            IF p-aliq-icm = 0 THEN
                                ASSIGN p-aliq-icm = beunid-feder.per-icms-ext.
            
                        END.
                    END.
                END.

                FIND FIRST beunid-feder 
                        WHERE beunid-feder.pais   = emitente.pais
                          AND beunid-feder.estado = emitente.estado NO-LOCK NO-ERROR.

                IF p-aliq-icm = 0 THEN
                    ASSIGN d-aliq-icm = 0.
                ELSE
                    ASSIGN d-aliq-icm = ABS(bunid-feder.per-icms-int - p-aliq-icm).
        END.


        

        
            PUT UNFORMATTED                                                                     
                ";" d-aliq-icm                                                                    /*% Difal recebimento*/
                ";" IF avail(nfe-nota-fiscal-rec) THEN nfe-nota-fiscal-rec.emit-crt ELSE 0        /*Reg. Tributa‡Æo (Normal/Simples Nacional)*/ 
                ";" IF AVAILABLE (es-item-doc-est-natoper)  THEN es-item-doc-est-natoper.cod-beneficio-icms    ELSE 0                                /*Benef¡cio ICMS recebimento*/
                ";" IF AVAILABLE (es-item-doc-est-natoper)  THEN es-item-doc-est-natoper.cod-beneficio-piscof  ELSE 0                                /*Benef¡cio PIS/COFINS  recebimento*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-vBC     ) ELSE "XML Inexistente"  /*BC ICMS - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-pRedBC  ) ELSE "XML Inexistente"  /*%Reduc ICMS - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-pICMS   ) ELSE "XML Inexistente"  /*Aliq ICMS - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-vICMS   ) ELSE "XML Inexistente"  /*Vl ICMS - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-vBCST   ) ELSE "XML Inexistente"  /*BC ICMS ST - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-pMVAST  ) ELSE "XML Inexistente"  /*MVA - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-pICMSST ) ELSE "XML Inexistente"  /*ALIQ ST - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-vICMSST ) ELSE "XML Inexistente"  /*VL ST - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-ipi-vBC ) ELSE "XML Inexistente"  /*BC IPI - XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-ipi-pIPI) ELSE "XML Inexistente"  /*ALIQ IPI XML*/
                ";" IF avail(nfe-it-nota-fisc-rec) THEN string(nfe-it-nota-fisc-rec.imp-ipi-vIPI) ELSE "XML Inexistente". /*VAL IPI -XML*/

            PUT ";" c-obs-cfa FORMAT "x(13)".


        PUT SKIP.
    END.

END PROCEDURE.





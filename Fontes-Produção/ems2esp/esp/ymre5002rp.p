/******************************************************************************************************************************************
** Programa: esp/esjd3001rp.p
** Data    : 15-05-2015
** Autor   : Mauricio Cerqueira Miranda
** Objetivo: Relat¢rio Cross Reference
********************************************************************************************************************************************/

/* include de controle de vers’o */
{include/i-prgvrs.i ESJD3001 "DTS11"}


/* ---------------------CONFIGURA EXCEL-------------------------- */
DEFINE VARIABLE chExcelApplication        AS COM-HANDLE.
DEFINE VARIABLE chWorkbook                AS COM-HANDLE.
DEFINE VARIABLE chWorksheet               AS COM-HANDLE.
DEFINE VARIABLE chChart                   AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange          AS COM-HANDLE.
DEFINE VARIABLE chWorkSheetRageSetup      AS COM-HANDLE.
DEFINE VARIABLE iCount                    AS INTEGER.
DEFINE VARIABLE cRange                    AS CHARACTER.
DEFINE VARIABLE i-linha                   AS INTEGER INITIAL 4.
/* -------------------------------------------------------------- */

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

define temp-table tt-param no-undo
    field destino             as integer
    field arquivo             as char format "x(35)"
    field usuario             as char format "x(12)"
    field data-exec           as date
    field hora-exec           as integer
    field classifica          as integer
    field desc-classifica     as char format "x(40)"
    field modelo-rtf          as char format "x(35)"
    field l-habilitaRtf       as LOG
    FIELD c-cod-estabel-ini   AS CHAR
    FIELD c-cod-estabel-fim   AS CHAR
    FIELD i-cod-imposto-ini   AS INT
    FIELD i-cod-imposto-fim   AS INT
    FIELD c-espec-imposto-ini AS CHAR
    FIELD c-espec-imposto-fim AS CHAR
    FIELD i-cod-fornec-ini    AS INT
    FIELD i-cod-fornec-fim    AS INT
    FIELD c-nro-docum-ini     AS CHAR
    FIELD c-nro-docum-fim     AS CHAR
    FIELD c-serie-ini         AS CHAR
    FIELD c-serie-fim         AS CHAR
    FIELD c-cfop-ini          AS CHAR
    FIELD c-cfop-fim          AS CHAR
    FIELD c-cod-natureza-ini  AS CHAR
    FIELD c-cod-natureza-fim  AS CHAR
    FIELD dt-transac-ini      AS DATE
    FIELD dt-transac-fim      AS DATE
    FIELD dt-emis-ini         AS DATE
    FIELD dt-emis-fim         AS DATE
    FIELD dt-venc-tit-ini     AS DATE
    FIELD dt-venc-tit-fim     AS DATE
    FIELD dt-venc-tit-imp-ini AS DATE
    FIELD dt-venc-tit-imp-fim AS DATE
    FIELD dt-pgto-tit-ini     AS DATE
    FIELD dt-pgto-tit-fim     AS DATE
    FIELD r-tipo-nf           AS INT.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de par³metros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*Carregando a tt-digita*/
create tt-param.
raw-transfer raw-param to tt-param.


/* carregando tt-digita */
For Each tt-raw-digita:
    Create tt-digita.
    Raw-transfer tt-raw-digita.raw-digita To tt-digita.
End.     

DEFINE TEMP-TABLE tt-saida-excel NO-UNDO
    FIELD cod-estabel   LIKE docum-est.cod-estabel     
    FIELD int-1         like dupli-imp.int-1           
    FIELD descricao     like tipo-tax.descricao        
    FIELD cod-esp       like dupli-imp.cod-esp         
    FIELD descricao-esp like espec-ap.descricao        
    FIELD cod-emitente  like docum-est.cod-emitente    
    FIELD nome-emit     like emitente.nome-emit        
    FIELD nro-docto     like docum-est.nro-docto       
    FIELD serie-docto   like docum-est.serie-docto     
    FIELD nat-operacao  like docum-est.nat-operacao    
    FIELD dt-trans      like docum-est.dt-trans        
    FIELD dt-emissao    like docum-est.dt-emissao      
    FIELD dat-validade  like docum-est.dat-validade    
    FIELD dat-pagamento AS DATE 
    FIELD dt-venc-imp   like dupli-imp.dt-venc-imp     
    FIELD dt-pgto-imp   AS DATE
    FIELD it-codigo     like item-doc-est.it-codigo    
    FIELD desc-item     like ITEM.desc-item            
    FIELD cod-retencao  like dupli-imp.cod-retencao    
    FIELD tot-valor     like docum-est.tot-valor       
    FIELD base-calc     AS DEC          
    FIELD aliquota      like dupli-imp.aliquota        
    FIELD vl-imposto    like dupli-imp.vl-imposto.      

define variable h-acomp as handle no-undo.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Extra‡Æo de Dados").

FOR EACH docum-est NO-LOCK      
   WHERE docum-est.serie-docto  >= tt-param.c-serie-ini
     AND docum-est.serie-docto  <= tt-param.c-serie-fim
     AND docum-est.nro-docto    >= tt-param.c-nro-docum-ini
     AND docum-est.nro-docto    <= tt-param.c-nro-docum-fim
     AND docum-est.cod-emitente >= tt-param.i-cod-fornec-ini
     AND docum-est.cod-emitente <= tt-param.i-cod-fornec-fim
     AND docum-est.nat-operacao >= tt-param.c-cod-natureza-ini
     AND docum-est.nat-operacao <= tt-param.c-cod-natureza-fim
     AND docum-est.cod-estabel  >= tt-param.c-cod-estabel-ini
     AND docum-est.cod-estabel  <= tt-param.c-cod-estabel-fim
     AND docum-est.dt-trans     >= tt-param.dt-transac-ini
     AND docum-est.dt-trans     <= tt-param.dt-transac-fim
     AND docum-est.dt-emissao   >= tt-param.dt-emis-ini
     AND docum-est.dt-emissao   <= tt-param.dt-emis-fim ,
    EACH item-doc-est OF docum-est,
    EACH dupli-apagar OF docum-est:

    IF tt-param.r-tipo-nf = 2 THEN
        IF docum-est.cod-observa <> 4 THEN NEXT.

    FOR FIRST es-item-doc-est-natoper NO-LOCK
        WHERE es-item-doc-est-natoper.serie-docto     = docum-est.serie-docto 
          AND es-item-doc-est-natoper.nro-docto       = docum-est.nro-docto   
          AND es-item-doc-est-natoper.cod-emitente    = docum-est.cod-emitente
          AND es-item-doc-est-natoper.nat-operacao    = docum-est.nat-operacao
          AND es-item-doc-est-natoper.cod-cfop-saida >= tt-param.c-cfop-ini 
          AND es-item-doc-est-natoper.cod-cfop-saida <= tt-param.c-cfop-fim:
    END.

    IF NOT AVAIL(es-item-doc-est-natoper) THEN
        NEXT.

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = docum-est.cod-emitente NO-ERROR.
    FIND FIRST ITEM     NO-LOCK WHERE ITEM.it-codigo = item-doc-est.it-codigo NO-ERROR.

    FIND FIRST tt-saida-excel
         WHERE tt-saida-excel.serie-docto   = dupli-apagar.serie-docto 
           AND tt-saida-excel.nro-docto     = dupli-apagar.nro-docto    
           AND tt-saida-excel.cod-emitente  = docum-est.cod-emitente
           AND tt-saida-excel.nat-operacao  = dupli-apagar.nat-operacao 
           AND tt-saida-excel.cod-esp       = dupli-apagar.cod-esp NO-ERROR.
    IF NOT AVAIL(tt-saida-excel) THEN DO:

        FIND FIRST espec-ap NO-LOCK 
             WHERE espec-ap.cod-esp = dupli-apagar.cod-esp NO-ERROR.


        FIND FIRST tit_ap NO-LOCK
             WHERE tit_ap.cod_estab       = docum-est.cod-estabel
               AND tit_ap.cdn_fornecedor  = dupli-apagar.cod-emitente
               AND tit_ap.cod_espec_docto = dupli-apagar.cod-esp
               AND tit_ap.cod_ser_docto   = dupli-apagar.serie-docto
               AND tit_ap.cod_tit_ap      = dupli-apagar.nro-docto
               AND tit_ap.cod_parcela     = dupli-apagar.parcela NO-ERROR.

        CREATE tt-saida-excel.
        ASSIGN tt-saida-excel.cod-estabel   = docum-est.cod-estabel     
               tt-saida-excel.cod-esp       = dupli-apagar.cod-esp         
               tt-saida-excel.descricao-esp = espec-ap.descricao        
               tt-saida-excel.cod-emitente  = docum-est.cod-emitente    
               tt-saida-excel.nome-emit     = emitente.nome-emit        
               tt-saida-excel.nro-docto     = docum-est.nro-docto       
               tt-saida-excel.serie-docto   = docum-est.serie-docto     
               tt-saida-excel.nat-operacao  = docum-est.nat-operacao    
               tt-saida-excel.dt-trans      = docum-est.dt-trans        
               tt-saida-excel.dt-emissao    = docum-est.dt-emissao      
               tt-saida-excel.dat-validade  = IF avail(tit_ap) THEN tit_ap.dat_vencto_tit_ap ELSE ?    
               tt-saida-excel.dat-pagamento = IF avail(tit_ap) THEN tit_ap.dat_liquidac_tit_ap ELSE ?
               tt-saida-excel.it-codigo     = item-doc-est.it-codigo    
               tt-saida-excel.desc-item     = ITEM.desc-item            
               tt-saida-excel.tot-valor     = docum-est.tot-valor       
               tt-saida-excel.base-calc     = docum-est.valor-mercad.
    END.

    FOR EACH dupli-imp 
       WHERE dupli-imp.cod-emitente = dupli-apagar.cod-emitente 
         AND dupli-imp.nat-operacao = dupli-apagar.nat-operacao 
         AND dupli-imp.serie-docto  = dupli-apagar.serie-docto 
         AND dupli-imp.nro-docto    = dupli-apagar.nro-docto
         AND dupli-imp.parcela      = dupli-apagar.parcela
         AND dupli-imp.int-1       >= tt-param.i-cod-imposto-ini
         AND dupli-imp.int-1       <= tt-param.i-cod-imposto-fim
         AND dupli-imp.cod-esp     >= tt-param.c-espec-imposto-ini
         AND dupli-imp.cod-esp     <= tt-param.c-espec-imposto-fim 
         AND dupli-imp.dt-venc-imp >= tt-param.dt-venc-tit-imp-ini 
         AND dupli-imp.dt-venc-imp <= tt-param.dt-venc-tit-imp-fim NO-LOCK:


        FIND FIRST espec-ap NO-LOCK 
             WHERE espec-ap.cod-esp = dupli-imp.cod-esp NO-ERROR.

        FIND FIRST tit_ap NO-LOCK
             WHERE tit_ap.cod_estab       = docum-est.cod-estabel
               AND tit_ap.cdn_fornecedor  = dupli-imp.cod-emitente
               AND tit_ap.cod_espec_docto = dupli-imp.cod-esp
               AND tit_ap.cod_ser_docto   = dupli-imp.serie-docto
               AND tit_ap.cod_tit_ap      = dupli-imp.nro-docto
               AND tit_ap.cod_parcela     = dupli-imp.parcela NO-ERROR.

        FIND FIRST tipo-tax NO-LOCK WHERE tipo-tax.cod-tax = dupli-imp.int-1  NO-ERROR .

            run pi-acompanhar in h-acomp (input "Documento: " + string(docum-est.nro-docto)).
            
            CREATE tt-saida-excel.
            ASSIGN tt-saida-excel.cod-estabel   = docum-est.cod-estabel     
                   tt-saida-excel.int-1         = dupli-imp.int-1           
                   tt-saida-excel.descricao     = tipo-tax.descricao        
                   tt-saida-excel.cod-esp       = dupli-imp.cod-esp         
                   tt-saida-excel.descricao-esp = espec-ap.descricao        
                   tt-saida-excel.cod-emitente  = docum-est.cod-emitente    
                   tt-saida-excel.nome-emit     = emitente.nome-emit        
                   tt-saida-excel.nro-docto     = docum-est.nro-docto       
                   tt-saida-excel.serie-docto   = docum-est.serie-docto     
                   tt-saida-excel.nat-operacao  = docum-est.nat-operacao    
                   tt-saida-excel.dt-trans      = docum-est.dt-trans        
                   tt-saida-excel.dt-emissao    = docum-est.dt-emissao      
                   tt-saida-excel.dat-validade  = docum-est.dat-validade    
                   tt-saida-excel.dt-venc-imp   = dupli-imp.dt-venc-imp     
                   tt-saida-excel.dt-pgto-imp   = IF avail(tit_ap) THEN tit_ap.dat_liquidac_tit_ap ELSE ?
                   tt-saida-excel.it-codigo     = item-doc-est.it-codigo    
                   tt-saida-excel.desc-item     = ITEM.desc-item            
                   tt-saida-excel.cod-retencao  = dupli-imp.cod-retencao    
                   tt-saida-excel.tot-valor     = docum-est.tot-valor       
                   tt-saida-excel.base-calc     = docum-est.valor-mercad          
                   tt-saida-excel.aliquota      = dupli-imp.aliquota        
                   tt-saida-excel.vl-imposto    = dupli-imp.vl-imposto .
    END.
END.


RUN pi-finalizar IN h-acomp.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.            
RUN pi-inicializar IN h-acomp (INPUT "Gerando Excel").  

RUN pi-cria-cabecalho.
RUN pi-cria-excel.
RUN pi-finaliza-handle.
RUN pi-finalizar IN h-acomp.

PROCEDURE pi-cria-cabecalho:
   
CREATE "Excel.Application" chexcelApplication.
       chexcelApplication:VISIBLE = FALSE.
       chWorkbook = chExcelApplication:Workbooks:Add().
       chExcelApplication:AlertBeforeOverwriting = false.
       chWorkSheet = chExcelApplication:Sheets:Item(1).
       chWorkSheet:name = "Relatorio Reten‡Æo".
       chExcelApplication:ActiveWindow:Zoom = 90.

       cRange = "A1:W1".
       chexcelApplication:SELECTION:MergeCells = TRUE.
       chexcelApplication:Range(cRange):FONT:bold = TRUE.
       chexcelApplication:Range(cRange):Merge.
       chWorkSheet:Range(cRange):value = "Relatorio Reten‡Æo".
       chexcelApplication:Range(cRange):FONT:NAME = "Courier New".
       chexcelApplication:Range(cRange):FONT:SIZE = "11".
       chexcelApplication:Range(cRange):HorizontalAlignment = 3.

       cRange = "A3".
       chWorkSheet:Range(cRange):value = "Estabelecimento".

       cRange = "B3".
       chWorkSheet:Range(cRange):value = "Cod. Imposto".
       
       cRange = "C3".
       chWorkSheet:Range(cRange):value = "Desc. Imposto".
       
       cRange = "D3".
       chWorkSheet:Range(cRange):value = "Especie Imposto".

       cRange = "E3".
       chWorkSheet:Range(cRange):value = "Desc. Especie".

       cRange = "F3".
       chWorkSheet:Range(cRange):value = "Emitente".

       cRange = "G3".
       chWorkSheet:Range(cRange):value = "Nome Emit.".

       cRange = "H3".
       chWorkSheet:Range(cRange):value = "Nro. Docto".

       cRange = "I3".
       chWorkSheet:Range(cRange):value = "Serie".

       cRange = "K3".
       chWorkSheet:Range(cRange):value = "Nat. Operacao".

       cRange = "K3".
       chWorkSheet:Range(cRange):value = "Dt. Trans.".

       cRange = "L3".
       chWorkSheet:Range(cRange):value = "Dt. Emissao".

       cRange = "M3".
       chWorkSheet:Range(cRange):value = "Dt. Venc.".

       cRange = "N3".
       chWorkSheet:Range(cRange):value = "Dt. Pagamento".

       cRange = "O3".
       chWorkSheet:Range(cRange):value = "Dt. Venc. Imposto".

       cRange = "P3".
       chWorkSheet:Range(cRange):value = "Dt. Pgto. Imposto".

       cRange = "Q3".
       chWorkSheet:Range(cRange):value = "Item".

       cRange = "R3".
       chWorkSheet:Range(cRange):value = "Desc. Item".

       cRange = "S3".
       chWorkSheet:Range(cRange):value = "Cod. Reten‡Æo".

       cRange = "T3".
       chWorkSheet:Range(cRange):value = "Valor Contabil".

       cRange = "U3".
       chWorkSheet:Range(cRange):value = "Base Calculo".

       cRange = "V3".
       chWorkSheet:Range(cRange):value = "Aliquota".

       cRange = "W3".
       chWorkSheet:Range(cRange):value = "Vlr. Imposto".

       
       chexcelApplication:Range("A3:W3"):FONT:bold = TRUE.
       chexcelApplication:Range("A3:W3"):FONT:NAME = "Arial".
       chexcelApplication:Range("A3:W3"):FONT:SIZE = "9".
       chexcelApplication:Range("A3:W3"):HorizontalAlignment = 3.
       chexcelApplication:Range("A3:W3"):AutoFilter(,,,).

       RUN pi-gera-borda("A3:W3").

       chexcelApplication:ActiveWindow:SplitColumn    = 0.
       chexcelApplication:ActiveWindow:SplitRow       = 3.
       chexcelApplication:ActiveWindow:FreezePanes    = True. 
END PROCEDURE.

PROCEDURE pi-cria-excel:
    
    FOR EACH tt-saida-excel:

       RUN pi-acompanhar in h-acomp(INPUT "Documento: " + STRING(tt-saida-excel.nro-docto) ).
       
       chWorkSheet:Range("A" + string(i-linha)):VALUE = tt-saida-excel.cod-estabel  .  
       chWorkSheet:Range("B" + string(i-linha)):VALUE = tt-saida-excel.int-1        .        
       chWorkSheet:Range("C" + string(i-linha)):VALUE = tt-saida-excel.descricao    .              
       chWorkSheet:Range("D" + string(i-linha)):VALUE = tt-saida-excel.cod-esp      .                 
       chWorkSheet:Range("E" + string(i-linha)):VALUE = tt-saida-excel.descricao-esp. 
       chWorkSheet:Range("F" + string(i-linha)):VALUE = tt-saida-excel.cod-emitente .        
       chWorkSheet:Range("G" + string(i-linha)):VALUE = tt-saida-excel.nome-emit    .       
       chWorkSheet:Range("H" + string(i-linha)):VALUE = tt-saida-excel.nro-docto    .
       chWorkSheet:Range("I" + string(i-linha)):VALUE = tt-saida-excel.serie-docto  .
       chWorkSheet:Range("J" + string(i-linha)):VALUE = tt-saida-excel.nat-operacao .
       chWorkSheet:Range("K" + string(i-linha)):VALUE = tt-saida-excel.dt-trans     .
       chWorkSheet:Range("L" + string(i-linha)):VALUE = tt-saida-excel.dt-emissao   .
       chWorkSheet:Range("M" + string(i-linha)):VALUE = tt-saida-excel.dat-validade .
       chWorkSheet:Range("N" + string(i-linha)):VALUE = tt-saida-excel.dat-pagamento.
       chWorkSheet:Range("O" + string(i-linha)):VALUE = tt-saida-excel.dt-venc-imp  .
       chWorkSheet:Range("P" + string(i-linha)):VALUE = tt-saida-excel.dt-pgto-imp  .
       chWorkSheet:Range("Q" + string(i-linha)):VALUE = tt-saida-excel.it-codigo    .
       chWorkSheet:Range("R" + string(i-linha)):VALUE = tt-saida-excel.desc-item    .
       chWorkSheet:Range("S" + string(i-linha)):VALUE = tt-saida-excel.cod-retencao .
       chWorkSheet:Range("T" + string(i-linha)):VALUE = tt-saida-excel.tot-valor    .
       chWorkSheet:Range("U" + string(i-linha)):VALUE = tt-saida-excel.base-calc    .
       chWorkSheet:Range("V" + string(i-linha)):VALUE = tt-saida-excel.aliquota     .
       chWorkSheet:Range("W" + string(i-linha)):VALUE = tt-saida-excel.vl-imposto   .

       RUN pi-gera-borda(INPUT "A" + string(i-linha) + ":W" + string(i-linha)).

       ASSIGN i-linha = i-linha + 1.
   END.

END PROCEDURE.

PROCEDURE pi-gera-borda :

    DEFINE INPUT PARAM c-selecao AS CHAR NO-UNDO.

    chWorkSheet:Range(c-selecao):SELECT.
    ASSIGN chexcelApplication:Selection:Borders(5):LineStyle   = -4142
           chexcelApplication:Selection:Borders(6):LineStyle   = -4142
           chexcelApplication:Selection:Borders(7):LineStyle   = 1
           chexcelApplication:Selection:Borders(7):Weight      = 2
           chexcelApplication:Selection:Borders(7):ColorIndex  = -4142
           chexcelApplication:Selection:Borders(8):LineStyle   = 1
           chexcelApplication:Selection:Borders(8):Weight      = 2
           chexcelApplication:Selection:Borders(8):ColorIndex  = -4142
           chexcelApplication:Selection:Borders(9):LineStyle   = 1
           chexcelApplication:Selection:Borders(9):Weight      = 2
           chexcelApplication:Selection:Borders(9):ColorIndex  = -4142
           chexcelApplication:Selection:Borders(10):LineStyle  = 1
           chexcelApplication:Selection:Borders(10):Weight     = 2
           chexcelApplication:Selection:Borders(10):ColorIndex = -4142
           chexcelApplication:Selection:Borders(11):LineStyle  = 1
           chexcelApplication:Selection:Borders(12):LineStyle  = -4142.

END PROCEDURE.


PROCEDURE pi-finaliza-handle:
    chWorkSheet:Range("A:W"):EntireColumn:AutoFit.
    chexcelapplication:VISIBLE = TRUE.
    
    IF VALID-HANDLE(chexcelApplication) THEN
        RELEASE OBJECT chexcelApplication.
        
    IF VALID-HANDLE(chWorkbook) THEN
        RELEASE OBJECT chWorkbook.
    
    IF VALID-HANDLE(chWorkSheet) THEN
        RELEASE OBJECT chWorksheet.
END PROCEDURE.

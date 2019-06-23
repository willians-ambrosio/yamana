/********************************************************************************
** Programa : esmi001rp
** Objetivo : 
** Autor    : Bruno Bertulli
** Data     : 30/04/2013
** Alterado : 
*********************************************************************************/

def temp-table tt-raw-digita
    field raw-digita as raw.
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
def var h-acomp as handle no-undo.

/* variaveis locais */
define temp-table tt-param no-undo
    field destino            as integer
    field arquivo            as char format "x(35)"
    field usuario            as char format "x(12)"
    field data-exec          as date
    field hora-exec          as integer
    field classifica         as integer
    field desc-classifica    as char format "x(40)"
    field modelo-rtf         as char format "x(35)"
    field l-habilitaRtf      as LOG
    FIELD cd-equipto-fim     AS CHARACTER 
    FIELD cd-equipto-ini     AS CHARACTER     
    FIELD dt-ent-fim         AS DATE          
    FIELD dt-ent-ini         AS DATE          
    FIELD dt-ord-fim         AS DATE          
    FIELD dt-ord-ini         AS DATE          
    FIELD fi-cc-ini          AS CHARACTER 
    FIELD fi-cc-fim          AS CHARACTER 
    FIELD fi-ct-ini          AS CHARACTER 
    FIELD fi-ct-fim          AS CHARACTER 
    FIELD fi-item-fim        AS CHARACTER 
    FIELD fi-item-ini        AS CHARACTER 
    FIELD fi-preco-fim       AS DECIMAL   
    FIELD fi-preco-ini       AS DECIMAL   
    FIELD fi-req-fim         AS INT 
    FIELD fi-req-ini         AS INT 
    FIELD fi-sit-fim         AS INT 
    FIELD fi-sit-ini         AS INT 
    FIELD nr-ord-fim         AS INTEGER 
    FIELD nr-ord-ini         AS INTEGER
    FIELD Frotas             AS LOG
    FIELD Manutencao         AS LOG    
    .

DEF TEMP-TABLE  tt-movto
    FIELD  tt-cod-eqpto         LIKE mmv-ord-manut.cod-eqpto        
    FIELD  tt-nr-ord-produc     LIKE req-ord-produc.nr-ord-produ    
    FIELD  tt-it-codigo         LIKE item.it-codigo                 
    FIELD  tt-qt-requisitada    LIKE it-requisicao.qt-requisitada   
    FIELD  tt-nr-requisicao     LIKE ordem-compra.nr-requisicao     
    FIELD  tt-numero-ordem      LIKE ordem-compra.numero-ordem      
    FIELD  tt-num-pedido        LIKE ordem-compra.num-pedido        
    FIELD  tt-preco-fornec      LIKE ordem-compra.preco-fornec      
    FIELD  tt-data-entrega      LIKE prazo-compra.data-entrega      
    FIELD  tt-situacao          LIKE prazo-compra.situacao             
    FIELD  tt-ct-codigo         LIKE mmv-ord-manut.ct-codigo        
    FIELD  tt-cc-codigo         LIKE mmv-ord-manut.cc-codigo        
    FIELD  tt-num-ord-inv       LIKE ordem-compra.num-ord-inv       
    FIELD  tt-desc-item         LIKE item.desc-item
    FIELD  tt-cod-emit          LIKE ordem-compra.cod-emitente
    FIELD  tt-quant-receb       LIKE prazo-compra.quant-receb.         

DEFINE VARIABLE i-linha         AS INTEGER    NO-UNDO.

create tt-param.
raw-transfer raw-param to tt-param.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Inicializando...").

find first param-global no-lock no-error.

/* ===> Main Block <=== */

IF tt-param.frota THEN DO:     

    EMPTY TEMP-TABLE tt-movto.

    for each ordem-compra no-lock
        where ordem-compra.dat-ordem    >= TT-PARAM.dt-ord-ini
        and   ordem-compra.dat-ordem    <= TT-PARAM.dt-ord-fim
        AND   ordem-compra.numero-ordem >= TT-PARAM.nr-ord-ini
        and   ordem-compra.numero-ordem <= TT-PARAM.nr-ord-fim
        and   ordem-compra.preco-fornec >= TT-PARAM.fi-preco-ini
        and   ordem-compra.preco-fornec <= TT-PARAM.fi-preco-fim,
        each item no-lock
            where item.it-codigo = ordem-compra.it-codigo,
            each req-ord-produc no-lock
                where req-ord-produc.nr-requisicao = ordem-compra.nr-requisicao
                and   req-ord-produc.sequencia     = ordem-compra.sequencia
                AND   req-ord-produc.nr-requisicao >= TT-PARAM.fi-req-ini
                and   req-ord-produc.nr-requisicao <= TT-PARAM.fi-req-fim,
                each mmv-ord-manut no-lock
                    where mmv-ord-manut.nr-ord-produ = req-ord-produc.nr-ord-produ
                    and   mmv-ord-manut.cc-codigo   >= TT-PARAM.fi-cc-ini
                    and   mmv-ord-manut.cc-codigo   <= TT-PARAM.fi-cc-fim
                    and   mmv-ord-manut.cod-eqpto   >= TT-PARAM.cd-equipto-ini
                    and   mmv-ord-manut.cod-eqpto   <= TT-PARAM.cd-equipto-fim
                    and   mmv-ord-manut.ct-codigo   >= TT-PARAM.fi-ct-ini
                    and   mmv-ord-manut.ct-codigo   <= TT-PARAM.fi-ct-fim,
                    each emitente no-lock
                        where emitente.cod-emitente = ordem-compra.cod-emitente,
                        each it-requisicao no-lock
                            where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao 
                            and   it-requisicao.sequencia     = req-ord-produc.sequencia
                            and   it-requisicao.it-codigo    >= TT-PARAM.fi-item-ini
                            and   it-requisicao.it-codigo    <= TT-PARAM.fi-item-fim,
                            each prazo-compra no-lock
                                where prazo-compra.numero-ordem  = ordem-compra.numero-ordem
                                and   prazo-compra.data-entrega >= TT-PARAM.dt-ent-ini
                                and   prazo-compra.data-entrega <= TT-PARAM.dt-ent-fim
                                and   prazo-compra.situacao     >= TT-PARAM.fi-sit-ini
                                and   prazo-compra.situacao     <= TT-PARAM.fi-sit-fim
                                break by mmv-ord-manut.cod-eqpto
                                      by emitente.nome-abrev
                                      by req-ord-produc.nr-requisicao
                                      by it-requisicao.nr-requisicao
                                      by ordem-compra.num-pedido
                                      by it-requisicao.it-codigo
                                      by item.it-codigo:

        RUN pi-acompanhar IN h-acomp (INPUT "Ordem: " + STRING (ordem-compra.numero-ordem) + " Item: " + it-requisicao.it-codigo).

        CREATE tt-movto.
        ASSIGN tt-movto.tt-cod-eqpto         = mmv-ord-manut.cod-eqpto     
               tt-movto.tt-nr-ord-produ      = req-ord-produc.nr-ord-produ 
               tt-movto.tt-it-codigo         = item.it-codigo              
               tt-movto.tt-qt-requisitada    = it-requisicao.qt-requisitada
               tt-movto.tt-nr-requisicao     = ordem-compra.nr-requisicao  
               tt-movto.tt-numero-ordem      = ordem-compra.numero-ordem   
               tt-movto.tt-num-pedido        = ordem-compra.num-pedido     
               tt-movto.tt-preco-fornec      = ordem-compra.preco-fornec   
               tt-movto.tt-data-entrega      = prazo-compra.data-entrega   
               tt-movto.tt-situacao          = prazo-compra.situacao
               tt-movto.tt-ct-codigo         = mmv-ord-manut.ct-codigo     
               tt-movto.tt-cc-codigo         = mmv-ord-manut.cc-codigo     
               tt-movto.tt-num-ord-inv       = ordem-compra.num-ord-inv    
               tt-movto.tt-desc-item         = item.desc-item
               tt-movto.tt-cod-emit          = ordem-compra.cod-emitente
               tt-movto.tt-quant-receb       = prazo-compra.quant-receb
               .
    END.
    
    RUN pi-imprime(INPUT "Frotas").

END.     

IF tt-param.manutencao THEN DO:     

    EMPTY TEMP-TABLE tt-movto.

    for each ordem-compra no-lock
        where ordem-compra.dat-ordem    >= TT-PARAM.dt-ord-ini
        and   ordem-compra.dat-ordem    <= TT-PARAM.dt-ord-fim
        AND   ordem-compra.numero-ordem >= TT-PARAM.nr-ord-ini
        and   ordem-compra.numero-ordem <= TT-PARAM.nr-ord-fim
        and   ordem-compra.preco-fornec >= TT-PARAM.fi-preco-ini
        and   ordem-compra.preco-fornec <= TT-PARAM.fi-preco-fim,
        each item no-lock
            where item.it-codigo = ordem-compra.it-codigo,
            each req-ord-produc no-lock
                where req-ord-produc.nr-requisicao = ordem-compra.nr-requisicao
                and   req-ord-produc.sequencia     = ordem-compra.sequencia
                AND   req-ord-produc.nr-requisicao >= TT-PARAM.fi-req-ini
                and   req-ord-produc.nr-requisicao <= TT-PARAM.fi-req-fim,
                each ord-manut no-lock
                    where ord-manut.nr-ord-produ = req-ord-produc.nr-ord-produ
                    and   ord-manut.sc-codigo   >= TT-PARAM.fi-cc-ini
                    and   ord-manut.sc-codigo   <= TT-PARAM.fi-cc-fim
                    and   ord-manut.cd-equipto  >= TT-PARAM.cd-equipto-ini
                    and   ord-manut.cd-equipto  <= TT-PARAM.cd-equipto-fim
                    and   ord-manut.ct-codigo   >= TT-PARAM.fi-ct-ini
                    and   ord-manut.ct-codigo   <= TT-PARAM.fi-ct-fim,
                    each emitente no-lock
                        where emitente.cod-emitente = ordem-compra.cod-emitente,
                        each it-requisicao no-lock
                            where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao 
                            and   it-requisicao.sequencia     = req-ord-produc.sequencia
                            and   it-requisicao.it-codigo    >= TT-PARAM.fi-item-ini
                            and   it-requisicao.it-codigo    <= TT-PARAM.fi-item-fim,
                            each prazo-compra no-lock
                                where prazo-compra.numero-ordem  = ordem-compra.numero-ordem
                                and   prazo-compra.data-entrega >= TT-PARAM.dt-ent-ini
                                and   prazo-compra.data-entrega <= TT-PARAM.dt-ent-fim
                                and   prazo-compra.situacao     >= TT-PARAM.fi-sit-ini
                                and   prazo-compra.situacao     <= TT-PARAM.fi-sit-fim
                                break by ord-manut.cd-equipto
                                      by emitente.nome-abrev
                                      by req-ord-produc.nr-requisicao
                                      by it-requisicao.nr-requisicao
                                      by ordem-compra.num-pedido
                                      by it-requisicao.it-codigo
                                      by item.it-codigo:

        RUN pi-acompanhar IN h-acomp (INPUT "Ordem: " + STRING (ordem-compra.numero-ordem) + " Item: " + it-requisicao.it-codigo).

        CREATE tt-movto.
        ASSIGN tt-movto.tt-cod-eqpto         = ord-manut.cd-equipto
               tt-movto.tt-nr-ord-produ      = req-ord-produc.nr-ord-produ 
               tt-movto.tt-it-codigo         = item.it-codigo              
               tt-movto.tt-qt-requisitada    = it-requisicao.qt-requisitada
               tt-movto.tt-nr-requisicao     = ordem-compra.nr-requisicao  
               tt-movto.tt-numero-ordem      = ordem-compra.numero-ordem   
               tt-movto.tt-num-pedido        = ordem-compra.num-pedido     
               tt-movto.tt-preco-fornec      = ordem-compra.preco-fornec   
               tt-movto.tt-data-entrega      = prazo-compra.data-entrega   
               tt-movto.tt-situacao          = prazo-compra.situacao
               tt-movto.tt-ct-codigo         = ord-manut.ct-desp
               tt-movto.tt-cc-codigo         = ord-manut.sc-desp
               tt-movto.tt-num-ord-inv       = ordem-compra.num-ord-inv    
               tt-movto.tt-desc-item         = item.desc-item
               tt-movto.tt-cod-emit          = ordem-compra.cod-emitente
               tt-movto.tt-quant-receb       = prazo-compra.quant-receb
               .
    END.
    
    RUN pi-imprime (INPUT "Manuten‡Æo").

END.

run pi-finalizar in h-acomp.

/* ===> Procedures <=== */

PROCEDURE pi-imprime: 

    DEFINE INPUT PARAMETER p-desc AS CHAR FORMAT "X(20)" NO-UNDO.

    DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
    DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
    DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
    DEFINE VARIABLE chChart                 AS COM-HANDLE.
    DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
    DEFINE VARIABLE iCount                  AS INTEGER.
    DEFINE VARIABLE iIndex                  AS INTEGER.
    DEFINE VARIABLE iTotalNumberOfOrders    AS INTEGER.
    DEFINE VARIABLE iMonth                  AS INTEGER.
    DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
    DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
    DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 3.
    DEFINE VARIABLE cColumn                 AS CHARACTER.
    DEFINE VARIABLE cRange                  AS CHARACTER.

    DEF VAR COUNT  AS INT.
    DEFINE VARIABLE c-situacao AS CHARACTER   NO-UNDO.


    /************************************************************/
    /*          Abre planilha Excel                             */
    CREATE "Excel.Application" chExcelApplication.
    chworkbook  = chexcelapplication:workbooks:add.
    chworksheet = chexcelapplication:sheets:item(1).
    chExcelApplication:Visible = true.
    /************************************************************/

        /*************** Imprime altera»„es **************************/
        /*************** Cabe¯alho ***********************************/ 
        chWorkSheet:Range("a1"):Value = "Relat¢rio de Materiais Flutuantes (" + p-desc + ") -  "    + string(TODAY) .
                        /*Value  " + string(fi-mes-ini,"99") + " / " + string(fi-ano-ini)*/ .
        chWorkSheet:Range("a1"):Font:Size = 12.
        chWorkSheet:Range("a1"):Font:Bold = TRUE.
        chWorkSheet:Range("a3"):Value = "Equipamento". 
        chWorkSheet:Range("b3"):Value = "Ordem Manut.".
        chWorkSheet:Range("c3"):Value = "Qtde Solicitada".     
        chWorkSheet:Range("d3"):Value = "Item       ".  
        chWorkSheet:Range("e3"):Value = "Descri‡Æo  ". 
        chWorkSheet:Range("f3"):Value = "Requisi‡Æo".     
        chWorkSheet:Range("g3"):Value = "Nr. Ordem ".  
        chWorkSheet:Range("h3"):Value = "Situa‡Æo ".
        chWorkSheet:Range("i3"):Value = "Nr. Pedido".     
        chWorkSheet:Range("j3"):Value = "Pre‡o Fornec.".  
        chWorkSheet:Range("k3"):Value = "Data Entrega".     
        chWorkSheet:Range("l3"):Value = "Conta Contÿbil".     
        chWorkSheet:Range("m3"):Value = "Centro de Custo ".
        chWorkSheet:Range("n3"):Value = "Fornecedor ".  


        chWorkSheet:Range("a3:n3"):Font:Bold = TRUE.
        chWorkSheet:Columns("a"):ColumnWidth = 15.
        chWorkSheet:Columns("b"):ColumnWidth = 15.
        chWorkSheet:Columns("C"):ColumnWidth = 15.
        chWorkSheet:Columns("d"):ColumnWidth = 15.
        chWorkSheet:Columns("e"):ColumnWidth = 15.
        chWorkSheet:Columns("f"):ColumnWidth = 15.
        chWorkSheet:Columns("g"):ColumnWidth = 15.
        chWorkSheet:Columns("h"):ColumnWidth = 15.
        chWorkSheet:Columns("i"):ColumnWidth = 15.
        chWorkSheet:Columns("j"):ColumnWidth = 15.
        chWorkSheet:Columns("k"):ColumnWidth = 15.
        chWorkSheet:Columns("l"):ColumnWidth = 15.
        chWorkSheet:Columns("m"):ColumnWidth = 15.
        chWorkSheet:Columns("n"):ColumnWidth = 15.

        ASSIGN i-linha = 5.

         FOR EACH tt-movto:

             RUN pi-acompanhar IN h-acomp (INPUT "Gerando Excel: " + STRING (tt-movto.tt-cod-eqpto) + " Item: " + tt-movto.tt-it-codigo).

            ASSIGN c-situacao = "".
            IF tt-movto.tt-situacao = 1  THEN
               ASSIGN c-situacao = "NÆo Confirmada".
            ELSE
            IF tt-movto.tt-situacao = 2  THEN
               ASSIGN c-situacao = "Confirmada".
            ELSE
            IF tt-movto.tt-situacao = 3  THEN
               ASSIGN c-situacao = "Cotada".
            ELSE
            IF tt-movto.tt-situacao = 4  THEN
               ASSIGN c-situacao = "Eliminada".
            ELSE
            IF tt-movto.tt-situacao = 5  THEN
               ASSIGN c-situacao = "Em Cota‡Æo".
            ELSE
            IF tt-movto.tt-situacao = 6  THEN
               ASSIGN c-situacao = "Recebida".

            ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-cod-eqpto   
                   chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-nr-ord-produ     
                   chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-qt-requisitada  
                   chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-it-codigo
                   chExcelApplication:range( "e" + STRING(i-linha) ):value = tt-desc-item
                   chExcelApplication:range( "f" + STRING(i-linha) ):value = tt-nr-requisicao 
                   chExcelApplication:range( "g" + STRING(i-linha) ):value = tt-numero-ordem 
                   chExcelApplication:range( "h" + STRING(i-linha) ):value = c-situacao
                   chExcelApplication:range( "i" + STRING(i-linha) ):value = tt-num-pedido 
                   chExcelApplication:range( "j" + STRING(i-linha) ):value = tt-preco-fornec
                   chExcelApplication:range( "k" + STRING(i-linha) ):value = tt-data-entrega
                   chExcelApplication:range( "l" + STRING(i-linha) ):value = tt-ct-codigo 
                   chExcelApplication:range( "m" + STRING(i-linha) ):value = tt-cc-codigo
                   chExcelApplication:range( "n" + STRING(i-linha) ):value = tt-cod-emit.
                   .

            ASSIGN i-linha = i-linha + 1.

        END.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet. 

END.



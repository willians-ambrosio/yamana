/****************************************************************************
** Programa: esp/escc004rp.p
** Objetivo: Exporta‡Æo de Ordens de Compra
** Autor   : DSC
** Data    : Junho/2016
** Versao  : 2.06.00.006 - Inicial
*****************************************************************************/
{utp/ut-glob.i}
{esp/escc004tt.i}

/*--------------------------------------------------------------------------*/
define input parameter raw-param as raw no-undo.
define input parameter table for tt-raw-digita.

create tt-param. 
raw-transfer raw-param to tt-param.
find first tt-param no-lock no-error.

define variable c-situacao              as character format "x(15)"     no-undo.

define variable v_num_linha             as integer initial 4            no-undo.
define variable c-cd-equipto            like ord-manut.cd-equipto       no-undo.
define variable de-qtde-abe             like prazo-compra.quant-saldo   no-undo.

define variable h-acomp                 as handle                       no-undo.

DEFINE VARIABLE de-pre-unit-for AS DECIMAL FORMAT ">>>>>>>,>>>,>>9.99999" NO-UNDO.
DEFINE VARIABLE l-entrou AS LOGICAL NO-UNDO.


define variable chExcel                 as com-handle                   no-undo.
define variable chWBook                 as com-handle                   no-undo.
define variable chWSheet                as com-handle                   no-undo.
define variable chChart                 as com-handle                   no-undo.
define variable chWSheetRange           as com-handle                   no-undo.
define variable iCount                  as integer                      no-undo.
define variable iIndex                  as integer                      no-undo.
define variable iTotalNumberOfOrders    as integer                      no-undo.
define variable iMonth                  as integer                      no-undo.
define variable dAnnualQuota            as decimal                      no-undo.
define variable dTotalSalesAmount       as decimal                      no-undo.
define variable iColumn                 as integer initial 3            no-undo.
define variable cColumn                 as character                    no-undo.
define variable cRange                  as character                    no-undo.
define variable count                   as integer                      no-undo.

DEFINE STREAM st-csv.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp('Processando').

IF tt-param.i-execucao = 2 THEN
   RUN pi-impressao.
ELSE
   IF tt-param.l-csv THEN
      RUN pi-impressao-csv.
   ELSE
      RUN pi-impressao.

run pi-finalizar in h-acomp.

return 'OK':U.

PROCEDURE pi-impressao:
   
   
   
   CREATE "Excel.Application" chExcel.
   chWBook  = chExcel:workbooks:add().
   chWSheet = chExcel:Activesheet.
   chExcel:Visible = FALSE.
   
   /*************** Cabe»alho ***********************************/ 
   chWSheet:Range("a1"):Value = "Hist¢rico de Compras  -  "    + string(today) .
                   /*Value  " + string(fi-mes-ini,"99") + " / " + string(fi-ano-ini)*/ .
   chWSheet:Range("a1"):Font:Size = 12.
   chWSheet:Range("a1"):Font:Bold = TRUE.
   chWSheet:Range("a3"):Value = "Ordem".
   chWSheet:Range("b3"):Value = "Contrato".
   chWSheet:Range("c3"):Value = "Fam¡lia".             
   chWSheet:Range("d3"):Value = "Fam Comercial".
   chWSheet:Range("e3"):Value = "Item ".                  
   chWSheet:Range("f3"):Value = "Descri‡Æo ".             
   chWSheet:Range("g3"):Value = "Dt Requisi‡Æo".          
   chWSheet:Range("h3"):Value = "Requisi‡Æo".             
   chWSheet:Range("i3"):Value = "Requisitante".           
   chWSheet:Range("j3"):Value = "Dt Aprov Requis".        
   chWSheet:Range("k3"):Value = "Dt Ordem".               
   chWSheet:Range("l3"):Value = "Dt Pedido".              
   chWSheet:Range("m3"):Value = "Pedido  ".               
   chWSheet:Range("n3"):Value = "Ped Emerg.".             
   chWSheet:Range("o3"):Value = "Prazo Entrega".          
   chWSheet:Range("p3"):Value = "Dt Nota Fiscal".         
   chWSheet:Range("q3"):Value = "Dt Rec. F¡sico  ".       
   chWSheet:Range("r3"):Value = "Comprador   ".           
   chWSheet:Range("s3"):Value = "Qtde".                   
   chWSheet:Range("t3"):Value = "Valor ".                 
   chWSheet:Range("u3"):Value = "Fornecedor   ".          
   chWSheet:Range("v3"):Value = "Cond Pagto".             
   chWSheet:Range("w3"):Value = "Grupo de Estoque".       
   chWSheet:Range("x3"):Value = "Unid Medida".            
   chWSheet:Range("y3"):Value = "Tipo de Material".       
   chWSheet:Range("z3"):Value = "Situa‡Æo do Item".       
   chWSheet:Range("aa3"):Value ="Tipo Controle do Item".  
   chWSheet:Range("ab3"):Value = "Situa‡Æo".              
   chWSheet:Range("ac3"):Value = "Departamento".          
   chWSheet:Range("ad3"):Value = "Nro Ordem Investimento". 
   
   
   chWSheet:Range("a3:ad3"):Font:Bold = TRUE.
   chWSheet:Columns("a" ):ColumnWidth = 12.
   chWSheet:Columns("b" ):ColumnWidth = 15.
   chWSheet:Columns("C" ):ColumnWidth = 15.
   chWSheet:Columns("d" ):ColumnWidth = 15.
   chWSheet:Columns("e" ):ColumnWidth = 15.
   chWSheet:Columns("f" ):ColumnWidth = 15.
   chWSheet:Columns("g" ):ColumnWidth = 10.
   chWSheet:Columns("h" ):ColumnWidth = 10.
   chWSheet:Columns("i" ):ColumnWidth = 10.
   chWSheet:Columns("j" ):ColumnWidth = 10.
   chWSheet:Columns("k" ):ColumnWidth = 10.
   chWSheet:Columns("l" ):ColumnWidth = 10.
   chWSheet:Columns("m" ):ColumnWidth = 10.
   chWSheet:Columns("n" ):ColumnWidth = 10.
   chWSheet:Columns("o" ):ColumnWidth = 10.
   chWSheet:Columns("p" ):ColumnWidth = 10.
   chWSheet:Columns("q" ):ColumnWidth = 10.
   chWSheet:Columns("r" ):ColumnWidth = 10.
   chWSheet:Columns("s" ):ColumnWidth = 10.                
   chWSheet:Columns("t" ):ColumnWidth = 10.   
   chWSheet:Columns("u" ):ColumnWidth = 10.
   chWSheet:Columns("v" ):ColumnWidth = 10.
   chWSheet:Columns("w" ):ColumnWidth = 15.
   chWSheet:Columns("x" ):ColumnWidth = 15.
   chWSheet:Columns("y" ):ColumnWidth = 15.
   chWSheet:Columns("z" ):ColumnWidth = 15.
   chWSheet:Columns("aa"):ColumnWidth = 15.
   chWSheet:Columns("ab"):ColumnWidth = 15.
   chWSheet:Columns("ac"):ColumnWidth = 15.
   chWSheet:Columns("ad"):ColumnWidth = 15.

   for each ordem-compra no-lock where
            ordem-compra.dat-ordem    >= tt-param.dat-ordem-ini    and 
            ordem-compra.dat-ordem    <= tt-param.dat-ordem-fim    and
            ordem-compra.cod-estabel  >= tt-param.cod-estabel-ini  and
            ordem-compra.cod-estabel  <= tt-param.cod-estabel-fim  and
            ordem-compra.it-codigo    >= tt-param.it-codigo-ini    and
            ordem-compra.it-codigo    <= tt-param.it-codigo-fim    and
            ordem-compra.cod-comprado >= tt-param.cod-comprado-ini and
            ordem-compra.cod-comprado <= tt-param.cod-comprado-fim and
            ordem-compra.nr-contrato  >= tt-param.nr-contrato-ini  and
            ordem-compra.nr-contrato  <= tt-param.nr-contrato-fim,
      first item no-lock of ordem-compra where
            item.fm-codigo >= tt-param.fm-codigo-ini               and
            item.fm-codigo <= tt-param.fm-codigo-fim,
      first item-uni-estab no-lock of ordem-compra,
      first emitente no-lock of ordem-compra,
       each prazo-compra no-lock of ordem-compra
       /*break by emitente.nome-abrev
                     by ordem-compra.num-pedido
                     by item.it-codigo*/ :
   
       run pi-acompanhar in h-acomp (input "Processando Ordem " + string(ordem-compra.numero-ordem)).
   
       find first cond-pagto no-lock where
                  cond-pagto.cod-cond-pag = ordem-compra.cod-cond-pag no-error.
   
       chExcel:range("A" + STRING(v_num_linha)):value         = ordem-compra.numero-ordem.
       chExcel:range("b" + STRING(v_num_linha)):VALUE         = ordem-compra.nr-contrato.
       chExcel:Range("c" + STRING(v_num_linha)):NumberFormat  = "0000".
       chExcel:range("c" + STRING(v_num_linha)):value         = item.fm-codigo.
       chExcel:range("d" + STRING(v_num_linha)):value         = item.fm-cod-com.
       chExcel:Range("c" + STRING(v_num_linha)):NumberFormat  = "00000000".
       chExcel:range("e" + STRING(v_num_linha)):VALUE         = item.it-codigo.
       chExcel:range("f" + STRING(v_num_linha)):value         = item.desc-item.
       
       find first requisicao no-lock where
                  requisicao.nr-requisicao = ordem-compra.nr-requisicao no-error.
       if avail requisicao then do:
           chExcel:range("g" + STRING(v_num_linha)):VALUE      = requisicao.dt-requisicao.
           chExcel:range("i" + STRING(v_num_linha)):value      = requisicao.nome-abrev.
           
       end.
   
       chExcel:Range("h"  + STRING(v_num_linha)):VALUE         = ordem-compra.nr-requisicao.
   
       find first doc-pend-aprov no-lock where
                  doc-pend-aprov.nr-requisicao = ordem-compra.nr-requisicao no-error.
       if avail doc-pend-aprov then
           chExcel:range("j"  + STRING(v_num_linha)):value     = doc-pend-aprov.dt-aprova.
   
       chExcel:range("k"  + STRING(v_num_linha)):VALUE         = ordem-compra.dat-ordem.
       chExcel:range("l"  + STRING(v_num_linha)):value         = ordem-compra.data-pedido.
       chExcel:range("m"  + STRING(v_num_linha)):VALUE         = ordem-compra.num-pedido.
   
       find first pedido-compr no-lock where
                  pedido-compr.num-pedido = ordem-compra.num-pedido no-error.
       if avail pedido-compr then do:
          if pedido-compr.emergencial = yes then
              chExcel:range("n"  + STRING(v_num_linha)):value  = "Sim". 
          else 
              chExcel:range("n"  + STRING(v_num_linha)):value  = "NÆo". 
       end.
       else 
          chExcel:range("n"  + STRING(v_num_linha)):value  = "NÆo". 
   
       /* prazo de entrega */
       find first cotacao-item no-lock use-index cotacao where
                  cotacao-item.numero-ordem = ordem-compra.numero-ordem and
                  cotacao-item.it-codigo    = ordem-compra.it-codigo    and
                  cotacao-item.preco-unit   > 0                         no-error.
       if avail cotacao-item then
           assign chExcel:range("o"  + STRING(v_num_linha)):value         = cotacao-item.prazo-entreg.
       else
           assign chExcel:range("o"  + STRING(v_num_linha)):value         = 0.
   
       /* data do recebimento fisico */
       find first it-doc-fisico no-lock where
                  it-doc-fisico.it-codigo    = ordem-compra.it-codigo    and
                  it-doc-fisico.num-pedido   = ordem-compra.num-pedido   and
                  it-doc-fisico.numero-ordem = ordem-compra.numero-ordem no-error.
       if avail it-doc-fisico then do:
   
           find first doc-fisico of it-doc-fisico no-lock no-error.
           if avail doc-fisico then do:
   
               chExcel:range("p"  + STRING(v_num_linha)):VALUE         = doc-fisico.dt-emissao.
               chExcel:range("q"  + STRING(v_num_linha)):value         = doc-fisico.dt-trans.
   
           end.
       end.
       else do:
   
           find first item-doc-est no-lock where
                      item-doc-est.it-codigo    = ordem-compra.it-codigo    and
                      item-doc-est.num-pedido   = ordem-compra.num-pedido   and
                      item-doc-est.numero-ordem = ordem-compra.numero-ordem no-error.
           if avail item-doc-est then do:
               find first docum-est of item-doc-est no-lock no-error.
   
               if avail docum-est then do:
   
                   chExcel:range("p"  + STRING(v_num_linha)):VALUE         = docum-est.dt-emissao.
                   chExcel:range("q"  + STRING(v_num_linha)):value         = docum-est.dt-trans.
   
               end.
           end.
       end.

       find first cotacao-item no-lock use-index cotacao where
                  cotacao-item.numero-ordem = ordem-compra.numero-ordem and
                  cotacao-item.it-codigo    = ordem-compra.it-codigo    and
                  cotacao-item.preco-unit   > 0                         no-error.
       if avail cotacao-item then
           ASSIGN de-pre-unit-for = cotacao-item.pre-unit-for.
       else
           ASSIGN de-pre-unit-for = ordem-compra.pre-unit-for.
   
       ASSIGN chExcel:range("r"  + STRING(v_num_linha)):VALUE = ordem-compra.cod-comprado
              chExcel:range("s"  + STRING(v_num_linha)):VALUE = prazo-compra.quantidade
              chExcel:range("t"  + STRING(v_num_linha)):VALUE = de-pre-unit-for
              chExcel:range("u"  + STRING(v_num_linha)):VALUE = ordem-compra.cod-emitente
              chExcel:range("v"  + STRING(v_num_linha)):VALUE = STRING (ordem-compra.cod-cond-pag) + " - " + (IF AVAILABLE cond-pagto THEN cond-pagto.descricao ELSE "NAO ENCONTRADO")
              chExcel:range("w"  + STRING(v_num_linha)):VALUE = item.ge-codigo
              chExcel:range("x"  + STRING(v_num_linha)):VALUE = item.un
              chExcel:range("y"  + STRING(v_num_linha)):VALUE = IF item.ind-serv-mat           <> 0 THEN {ininc/i08in172.i 04 item.ind-serv-mat}            ELSE ""
              chExcel:range("z"  + STRING(v_num_linha)):VALUE = IF item-uni-estab.cod-obsoleto <> 0 THEN {ininc/i17in172.i 04 item-uni-estab.cod-obsoleto}  ELSE ""
              chExcel:range("aa" + STRING(v_num_linha)):VALUE = IF item.tipo-contr             <> 0 THEN {ininc/i09in122.i 04 item.tipo-contr}              ELSE ""
              chExcel:range("ab" + STRING(v_num_linha)):VALUE = IF ordem-compra.situacao       <> 0 THEN {ininc/i02in274.i 04 ordem-compra.situacao}        ELSE "".

       for first es-it-depto no-lock where
                 es-it-depto.it-codigo = item.it-codigo,
           first es-depto no-lock where
                 es-depto.codigo = es-it-depto.cod-depto:
           
           chExcel:range("ac" + STRING(v_num_linha)):VALUE     = es-depto.descricao.
   
       end.
   
       chExcel:range("ad" + STRING(v_num_linha)):VALUE         = ordem-compra.num-ord-inv.
   
       assign v_num_linha = v_num_linha + 1.
       
   end.
                          
   IF tt-param.i-execucao = 2 THEN DO:
      DEFINE VARIABLE c-arquivo AS CHARACTER NO-UNDO.
   
      ASSIGN tt-param.arquivo =  "d:\temp\rpw\escc004_" + REPLACE( STRING(TODAY,"99/99/9999"),"/","") + REPLACE( STRING(TIME,"hh:mm:ss"),":","") + ".xlsx".
   END.
   
   
   /* DEFINE STREAM st.                                                  */
   /* OUTPUT STREAM st TO VALUE("d:\temp\sergio.txt").                   */
   /*                                                                    */
   /* PUT STREAM st SESSION:TEMP-DIRECTORY + "novo" FORMAT "x(100)" SKIP */
   /*     SESSION:BATCH-MODE SKIP                                        */
   /*     tt-param.arquivo.                                              */
   /*                                                                    */
   /* OUTPUT STREAM st CLOSE.                                            */
   
   case tt-param.destino:
   
       when 1 then do:
       end.
       when 2 then do:
   
           release object chWSheet.
   
           chWBook:close(yes,replace(replace(replace(tt-param.arquivo,'TXT','XLSX'),'LST','XLSX'),'/','~\')).
   
           release object chWBook.
   
           chExcel:quit().
   
           release object chExcel.
   
       end.
   
       when 3 then do:
   
           chExcel:Visible = true.
   
           release object chWSheet.
           release object chWBook.
           release object chExcel.
   
       end.
   
   end case.
END PROCEDURE.


procedure pi-impressao-csv:
   DEFINE VARIABLE c-narrativa       AS CHARACTER         NO-UNDO.
   DEFINE VARIABLE c-arquivo         AS CHARACTER         NO-UNDO.
   DEFINE VARIABLE ch-excel          AS COMPONENT-HANDLE  NO-UNDO.
   DEFINE VARIABLE i-cont-linha      AS INTEGER           NO-UNDO.
   DEFINE VARIABLE de-saldo-contrato AS DECIMAL           NO-UNDO.
   DEFINE VARIABLE d-saldo-contrato  LIKE contrato-for.dec-2           NO-UNDO.

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + "_" + REPLACE( STRING(TODAY,"99/99/9999"),"/","") + REPLACE( STRING(TIME,"hh:mm:ss"),":","")  + ".csv".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-csv TO VALUE(c-arquivo) NO-CONVERT NO-MAP.
   
   /*************** Cabe»alho ***********************************/
   PUT STREAM st-csv
       "Ordem;"
       "Contrato;"
       "Fam¡lia;"             
       "Fam Comercial;"
       "Item ;"               
       "Descri‡Æo ;"          
       "Dt Requisi‡Æo;"       
       "Requisi‡Æo;"          
       "Requisitante;"        
       "Dt Aprov Requis;"     
       "Dt Ordem;"            
       "Dt Pedido;"           
       "Pedido  ;"            
       "Ped Emerg.;"          
       "Prazo Entrega;"       
       "Dt Nota Fiscal;"      
       "Dt Rec. F¡sico  ;"    
       "Comprador   ;"     
       "Qtde;"                
       "Valor ;"              
       "Fornecedor   ;"        
       "Cond Pagto;"
       "Grupo de Estoque;"
       "Unid Medida;"
       "Tipo de Material;"
       "Situa‡Æo do Item;"
       "Tipo Controle do Item;"
       "Situa‡Æo;"
       "Departamento;"
       "Nro Ordem Investimento;" SKIP.
   
   for each ordem-compra no-lock where
            ordem-compra.dat-ordem    >= tt-param.dat-ordem-ini    and 
            ordem-compra.dat-ordem    <= tt-param.dat-ordem-fim    and
            ordem-compra.cod-estabel  >= tt-param.cod-estabel-ini  and
            ordem-compra.cod-estabel  <= tt-param.cod-estabel-fim  and
            ordem-compra.it-codigo    >= tt-param.it-codigo-ini    and
            ordem-compra.it-codigo    <= tt-param.it-codigo-fim    and
            ordem-compra.cod-comprado >= tt-param.cod-comprado-ini and
            ordem-compra.cod-comprado <= tt-param.cod-comprado-fim and
            ordem-compra.nr-contrato  >= tt-param.nr-contrato-ini  and
            ordem-compra.nr-contrato  <= tt-param.nr-contrato-fim,
      first item no-lock of ordem-compra where
            item.fm-codigo >= tt-param.fm-codigo-ini               and
            item.fm-codigo <= tt-param.fm-codigo-fim,
      first item-uni-estab no-lock of ordem-compra,
      first emitente no-lock of ordem-compra,
       each prazo-compra no-lock of ordem-compra
       /*break by emitente.nome-abrev
                     by ordem-compra.num-pedido
                     by item.it-codigo*/ :
   
       run pi-acompanhar in h-acomp (input "Processando Ordem " + string(ordem-compra.numero-ordem)).
   
       find first cond-pagto no-lock where
                  cond-pagto.cod-cond-pag = ordem-compra.cod-cond-pag no-error.

       PUT STREAM st-csv
           ordem-compra.numero-ordem ";"
           ordem-compra.nr-contrato  ";"
           item.fm-codigo            ";"
           item.fm-cod-com           ";"
           item.it-codigo            ";"
           item.desc-item            ";".  
       
       find first requisicao no-lock where
                  requisicao.nr-requisicao = ordem-compra.nr-requisicao no-error.
       if avail requisicao then do:
           PUT STREAM st-csv
               requisicao.dt-requisicao ";".
           
       end.
       ELSE 
           PUT STREAM st-csv
               ";".
   
       PUT STREAM st-csv
           ordem-compra.nr-requisicao ";".

       if avail requisicao then do:
           PUT STREAM st-csv
               requisicao.nome-abrev    ";".
           
       end.
       ELSE 
           PUT STREAM st-csv
               ";".
   
       find first doc-pend-aprov no-lock where
                  doc-pend-aprov.nr-requisicao = ordem-compra.nr-requisicao no-error.
       if avail doc-pend-aprov then
           PUT STREAM st-csv
               doc-pend-aprov.dt-aprova ";".
       ELSE
           PUT STREAM st-csv
               ";".

       PUT STREAM st-csv
           ordem-compra.dat-ordem   ";"
           ordem-compra.data-pedido ";"
           ordem-compra.num-pedido  ";".
   
       find first pedido-compr no-lock where
                  pedido-compr.num-pedido = ordem-compra.num-pedido no-error.
       if avail pedido-compr then do:
          if pedido-compr.emergencial = yes then
              PUT STREAM st-csv "Sim" ";". 
          else 
              PUT STREAM st-csv "NÆo" ";". 
       end.
       ELSE 
          PUT STREAM st-csv "NÆo" ";".
   
       /* prazo de entrega */
       find first cotacao-item no-lock use-index cotacao where
                  cotacao-item.numero-ordem = ordem-compra.numero-ordem and
                  cotacao-item.it-codigo    = ordem-compra.it-codigo    and
                  cotacao-item.preco-unit   > 0                         no-error.
       if avail cotacao-item then
           PUT STREAM st-csv cotacao-item.prazo-entreg ";".
       else
           PUT STREAM st-csv 0 ";".
   
       /* data do recebimento fisico */
       find first it-doc-fisico no-lock where
                  it-doc-fisico.it-codigo    = ordem-compra.it-codigo    and
                  it-doc-fisico.num-pedido   = ordem-compra.num-pedido   and
                  it-doc-fisico.numero-ordem = ordem-compra.numero-ordem no-error.
       if avail it-doc-fisico then do:
   
           find first doc-fisico of it-doc-fisico no-lock no-error.
           if avail doc-fisico then do:
               PUT STREAM st-csv
                   doc-fisico.dt-emissao ";"
                   doc-fisico.dt-trans   ";".
           end.
           ELSE
               PUT STREAM st-csv
                   ";"
                   ";".
       end.
       else do:
   
           find first item-doc-est no-lock where
                      item-doc-est.it-codigo    = ordem-compra.it-codigo    and
                      item-doc-est.num-pedido   = ordem-compra.num-pedido   and
                      item-doc-est.numero-ordem = ordem-compra.numero-ordem no-error.
           if avail item-doc-est then do:
               find first docum-est of item-doc-est no-lock no-error.
   
               if avail docum-est then do:
                   PUT STREAM st-csv
                       docum-est.dt-emissao ";"
                       docum-est.dt-trans   ";".
               end.
               ELSE
                   PUT STREAM st-csv
                       ";"
                       ";".
           end.
           ELSE
              PUT STREAM st-csv
                  ";"
                  ";".
       end.

       find first cotacao-item no-lock use-index cotacao where
                  cotacao-item.numero-ordem = ordem-compra.numero-ordem and
                  cotacao-item.it-codigo    = ordem-compra.it-codigo    and
                  cotacao-item.preco-unit   > 0                         no-error.
       if avail cotacao-item then
           ASSIGN de-pre-unit-for = cotacao-item.pre-unit-for.
       else
           ASSIGN de-pre-unit-for = ordem-compra.pre-unit-for.
   
       PUT STREAM st-csv                                                                                                           
           ordem-compra.cod-comprado                                                                                               ";"
           prazo-compra.quantidade FORMAT ">>>>>>,>>>,>>9.9999"                                                                    ";"
           de-pre-unit-for                                                                                                         ";"
           ordem-compra.cod-emitente                                                                                               ";"
           STRING (ordem-compra.cod-cond-pag) + " - " + (IF AVAILABLE cond-pagto THEN cond-pagto.descricao ELSE "NAO ENCONTRADO") FORMAT "X(35)" ";"
           item.ge-codigo                                                                                                          ";"
           item.un                                                                                                                 ";"
           IF item.ind-serv-mat           <> 0 THEN {ininc/i08in172.i 04 item.ind-serv-mat}            ELSE ""  FORMAT "X(15)"                   ";"
           IF item-uni-estab.cod-obsoleto <> 0 THEN {ininc/i17in172.i 04 item-uni-estab.cod-obsoleto}  ELSE ""  FORMAT "X(15)"                   ";"
           IF item.tipo-contr             <> 0 THEN {ininc/i09in122.i 04 item.tipo-contr}              ELSE ""  FORMAT "X(15)"                   ";"
           IF ordem-compra.situacao       <> 0 THEN {ininc/i02in274.i 04 ordem-compra.situacao}        ELSE ""  FORMAT "X(25)"                   ";".

       ASSIGN l-entrou = NO.
   
       for first es-it-depto no-lock where
                 es-it-depto.it-codigo = item.it-codigo,
           first es-depto no-lock where
                 es-depto.codigo = es-it-depto.cod-depto:
           
           PUT STREAM st-csv
               es-depto.descricao ";".

           ASSIGN l-entrou = YES.
   
       end.

       IF l-entrou = NO THEN 
          PUT STREAM st-csv ";".
   
       PUT STREAM st-csv
           ordem-compra.num-ord-inv ";".

       PUT STREAM st-csv
           SKIP.
   end.
                          
   OUTPUT STREAM st-csv CLOSE.

   IF i-num-ped-exec-rpw = 0 THEN
      DOS SILENT START excel.exe VALUE(c-arquivo).

END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/

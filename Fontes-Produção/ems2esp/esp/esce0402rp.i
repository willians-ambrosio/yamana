/*=================== XML Variables ====================*/
define variable iLineCount       as integer   no-undo.
define variable iColCount        as integer   no-undo.
define variable iColumns         as integer   no-undo.
                                              
define variable cStyle           as character no-undo.
define variable cStyleDtTrans    as character no-undo.
define variable cStyleQuantidade as character no-undo.
define variable cStyleMatMobGgf  as character no-undo.
                                              
define variable cNumericFormat   as character no-undo.

/*=================== XML Functions ====================*/
function fnSkipCol  returns char (input pNumberColsToSkip as int)  forward.
function fnSkipLine returns char (input pNumberLinesToSkip as int) forward.

/*=================== XML Procedures ===================*/
procedure OpenWorkBook:

    put unformatted
        '<?xml version="1.0"?>'                                                       skip
        '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"'              skip
        '          xmlns:o="urn:schemas-microsoft-com:office:office"'                 skip
        '          xmlns:x="urn:schemas-microsoft-com:office:excel"'                  skip
        '          xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"'           skip
        '          xmlns:html="http://www.w3.org/TR/REC-html40">'                     skip
        '   <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">'     skip
        '       <Author>' c-seg-usuario '</Author>'                                   skip
        '       <LastAuthor>' c-seg-usuario '</LastAuthor>'                           skip
        '       <LastPrinted>' '2005-10-18T09:43:42Z' '</LastPrinted>'                skip
        '       <Created>' '2005-10-18T09:41:43Z' '</Created>'                        skip
        '       <Company>' 'Datasul' '</Company>'                                     skip
        '       <Version>11.5606</Version>'                                           skip
        '   </DocumentProperties>'                                                    skip
        '   <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">' skip
        '      <DownloadComponents/>'                                                 skip
        '      <LocationOfComponents HRef="file:///~\~\"/>'                             skip
        '   </OfficeDocumentSettings>'                                                skip.

end procedure.

procedure CloseWorkBook:

    put unformatted
        '</Workbook>'.

end procedure.

procedure OpenExcelWorkBook:

    put unformatted
        '<ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">' skip
        '   <WindowHeight>2970</WindowHeight>'                           skip
        '   <WindowWidth>11340</WindowWidth>'                            skip
        '   <WindowTopX>360</WindowTopX>'                                skip
        '   <WindowTopY>105</WindowTopY>'                                skip
        '   <ProtectStructure>False</ProtectStructure>'                  skip
        '   <ProtectWindows>False</ProtectWindows>'                      skip.

end procedure.

procedure CloseExcelWorkBook:

    put unformatted
        '</ExcelWorkbook>' skip.
end procedure.

procedure OpenStyles:
    define variable cLinha as character no-undo.

    input from value(search('esp\esce0402rp.i1')).
    repeat:
        import unformat cLinha.
        put unformat cLinha skip.
    end.
    input close.

end procedure.
            
procedure CloseStyles:

    put unformatted
        '</Styles>' skip.
end procedure.

procedure OpenWorkSheet:
    
    def input param c-program-name as char no-undo.

    put unformatted
        '<Worksheet ss:Name="' c-program-name '">' skip.

end procedure.

procedure CloseWorkSheet:

    put unformatted
        '</Worksheet>' skip.

end procedure.

procedure OpenWorksheetOptions:
    put unformatted
        '<WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">'                    skip
        '   <Zoom>80</Zoom>'                                                                   skip
        '   <DoNotDisplayGridlines/>'                                                          skip
        '   <PageSetup>'                                                                       skip
        '      <Header x:Margin="0.4" x:Data="&amp;LCabecalho"/>'                              skip
        '      <Footer x:Margin="0.4"/>'                                                       skip
        '      <PageMargins x:Bottom="0.984251969" x:Left="0.78740157499999996"'               skip
        '                   x:Right="0.78740157499999996" x:Top="0.984251969"/>'               skip
        '   </PageSetup>'                                                                      skip
        ' <Selected/>'                                                                         skip.

end procedure.

procedure CloseWorksheetOptions:

    put unformatted
        '</WorksheetOptions>' skip.

end procedure.
	
procedure OpenTable:

    def input param pColumns as int no-undo.

    put unformatted
        '   <Table ss:ExpandedColumnCount="' pColumns '" ss:ExpandedRowCount="65500" x:FullColumns="1" x:FullRows="1">' skip.

end procedure.

procedure CloseTable:

    put unformatted
        '   </Table>' skip.

end procedure.

procedure setColumnWidth:

    def input param cWidth as char no-undo.

    put unformatted
        '       <Column ss:AutoFitWidth="0" ss:Width="' trim(cWidth) '"/>' skip.

end procedure.

procedure OpenLine:

    assign iLineCount = iLineCount + 1.

    put unformatted
        '      <Row ss:Index="' iLineCount '">' skip.

end procedure.
	
procedure CloseLine:

    /* reinicia a contagem das colunas sempre que a TAG da linha ‚ fechada */
    assign iColCount = 0.

    put unformatted
        '      </Row>' skip.

end procedure.
	
procedure PrintDataString:

    def input param pData  as char no-undo.
    def input param pStyle as char no-undo.

    assign iColCount = iColCount + 1.

    if  pStyle <> "" then
        put unformatted
            '         <Cell ss:Index="' iColCount '" ss:StyleID="' pStyle '"><Data ss:Type="String">' pData '</Data></Cell>' skip.
    else
        put unformatted
            '         <Cell ss:Index="' iColCount '"><Data ss:Type="String">' pData '</Data></Cell>' skip.

end procedure.

procedure PrintDataStringEsp:

    def input param iNumberCol as int  no-undo.
    def input param pData  as char no-undo.
    def input param pStyle as char no-undo.

    assign iColCount = iColCount + iNumberCol.

    if  pStyle <> "" then
        put unformatted
            '         <Cell ss:Index="' iNumberCol '" ss:StyleID="' pStyle '"><Data ss:Type="String">' pData '</Data></Cell>' skip.
    else
        put unformatted
            '         <Cell ss:Index="' iNumberCol '"><Data ss:Type="String">' pData '</Data></Cell>' skip.

end procedure.
	
procedure PrintDataNumber:

    def input param pData  as char no-undo.
    def input param pStyle as char no-undo.

    assign iColCount = iColCount + 1.

    if  pStyle <> "" then
        put unformatted
            '         <Cell ss:Index="' iColCount '" ss:StyleID="' pStyle '"><Data ss:Type="Number">' pData '</Data></Cell>' skip.
    else
        put unformatted
            '         <Cell ss:Index="' iColCount '"><Data ss:Type="Number">' pData '</Data></Cell>' skip.

end procedure.

procedure PrintDataNumberEsp:

    def input param pData  as char no-undo.
    def input param pStyle as char no-undo.

    assign iColCount = iColCount + 1.

    if  pStyle <> "" then
        put unformatted
            '         <Cell ss:Index="' iColCount '" ss:StyleID="' pStyle '" ss:Formula="=' pData '"><Data ss:Type="Number">0</Data></Cell>' skip.
    else
        put unformatted
            '         <Cell ss:Index="' iColCount '"><Data ss:Type="Number">' pData '</Data></Cell>' skip.

end procedure.

procedure PrintDataDate:

    def input param pData  as date no-undo.
    def input param pStyle as char no-undo.


    def var cYear          as char no-undo.
    def var cMonth         as char no-undo.
    def var cDay           as char no-undo.

    /* conversÆo das datas para padrÆo XML */
    assign cYear  = string(year(pData))
           cMonth = string(month(pData),"99")
           cDay   = string(day(pData),"99").

    assign iColCount = iColCount + 1.

    if  pStyle <> "" then
        put unformatted
            '         <Cell ss:Index="' iColCount '" ss:StyleID="' pStyle '"><Data ss:Type="DateTime">' cYear '-' cMonth '-' cDay 'T00:00:00.000</Data></Cell>' skip.
    else
        put unformatted
            '         <Cell ss:Index="' iColCount '"><Data ss:Type="DateTime">' cYear '-' cMonth '-' cDay 'T00:00:00.000</Data></Cell>' skip.

end procedure.


procedure MergeColumns:

    def input param iNumberCols as int  no-undo. /* n£mero de c‚lulas da coluna a serem mescladas a partir da c‚lula corrente */
    def input param cData       as char no-undo.
    def input param cStyle      as char no-undo.

    assign iColCount = iColCount + 1. /* pr¢xima c‚lula da coluna */

    put unformatted
        '         <Cell ss:MergeAcross="' iNumberCols '" ss:StyleID="' cStyle '"><Data ss:Type="String">' cData '</Data></Cell>' skip.

    /* ap¢s mesclar as c‚lulas atualiza a coluna corrente, retirando a c‚lula (coluna) acrescenta acima */
    assign iColCount = iColCount + iNumberCols - 1.

end procedure.

procedure MergeAcross:

    def input param iNumberCols as int  no-undo. /* n£mero de c‚lulas da coluna a serem mescladas a partir da c‚lula corrente */
    def input param iNumberDown as int  no-undo.
    def input param cData       as char no-undo.
    def input param cStyle      as char no-undo.

    assign iColCount = iColCount + 1. /* pr¢xima c‚lula da coluna */

    put unformatted
        '         <Cell ss:MergeAcross="' iNumberCols '" ss:MergeDown="' iNumberDown '" ss:StyleID="' cStyle '"><Data ss:Type="String">' cData '</Data></Cell>' skip.

    /* ap¢s mesclar as c‚lulas atualiza a coluna corrente, retirando a c‚lula (coluna) acrescenta acima */
    assign iColCount = iColCount + iNumberCols - 1.

end procedure.
              
function fnSkipCol returns char (input pNumberColsToSkip as int).
    
    def var iCol as int no-undo.

    do iCol = 1 to pNumberColsToSkip:
        assign iColCount = iColCount + 1.
    end.
    
    return "OK".

end function.

function fnSkipLine returns char (input pNumberLinesToSkip as int).
    
    def var iLine as int no-undo.

    do iLine = 1 to pNumberLinesToSkip:
        assign iLineCount = iLineCount + 1.
    end.

    return "OK".

end function.

procedure piConvertXMLToXLS:

    /*=================================
      ConversÆo do arquivo XML para XLS
      =================================*/


    def var v-arquivo as char       no-undo.
    def var ch-excel  as com-handle no-undo.   
    
    if  iLineCount > 65500 then do: /* o Excel ‚ limitado a 65536 linhas */
        run utp/ut-msgs.p(input "show",
                          input 17006,
                          input "O relat¢rio Excel atingiu o limite de 65.500 linhas" + "~~" +
                                "Por limita‡äes do Microsoft Excel o relat¢rio ‚ limitado a " +
                                "65.500 linhas. Deste modo, dever  ser informada " +
                                "uma faixa de sele‡Æo que limite a impressÆo dos dados.").

        /* elimina arquivo XML */
        os-delete value(tt-param.arquivo).
    end.
    else do:

        /* elimina arquivo XLS */
        assign v-arquivo = replace(tt-param.arquivo, ".xml", ".xls").
        os-delete value(v-arquivo).


        create "Excel.Application" ch-excel.
        assign file-info:file-name = tt-param.arquivo. 

        ch-excel:WorkBooks:open(file-info:file-name,true). 
        ch-excel:sheets:item(1).

        assign file-info:file-name = v-arquivo.

        /* ConversÆo do arquivo .XML para .XLS */
        /* -4143: constante de conversÆo de arquivo .XML para .XLS */
        ch-excel:ActiveWorkbook:SaveAs(file-info:file-name,-4143,"","",no,no,no). 

        ch-excel:ActiveWorkbook:close.

        ch-excel:visible = yes.
        ch-excel:Workbooks:open(v-arquivo).

        release object ch-excel.

        /* elimina arquivo XML */
        os-delete value(tt-param.arquivo).

    end.
   
end procedure.

/*================ Internals Procedures ================*/
procedure piExplodeEstrutura :
    define input  parameter i-item as character no-undo.
    define variable old-item       as character no-undo.

    for each estrutura where estrutura.it-codigo = i-item no-lock :  
        if estrutura.it-codigo <> old-item then
            assign i-niv-mais-bai = i-niv-mais-bai + 1.

        find first item 
            where item.it-codigo = estrutura.es-codigo
            no-lock no-error.

        create tt-estrutura.
        assign i-num-sequen              = i-num-sequen + 1
               tt-estrutura.it-codigo    = estrutura.es-codigo
               tt-estrutura.niv-mais-bai = i-niv-mais-bai
               tt-estrutura.num-sequen   = i-num-sequen
               tt-estrutura.cod-estabel  = item.cod-estabel
               tt-estrutura.inform-compl = item.inform-compl
               tt-estrutura.codigo-refer = item.codigo-refer
               tt-estrutura.un           = item.un
               old-item                  = estrutura.it-codigo.

        run piExplodeEstrutura(input estrutura.es-codigo).
    end.
end procedure.

procedure piSaldoIni :
    define input  parameter  c-cod-item  like item.it-codigo             no-undo.
    define input  parameter  c-estabelec like estabelec.cod-estabel      no-undo.
    define input  parameter  da-data-ini like param-estoq.mensal-ate     no-undo.
    define input  parameter  da-data-fin like param-estoq.mensal-ate     no-undo.

    define buffer b-tt-estrutura for tt-estrutura.

    find first b-tt-estrutura
        where b-tt-estrutura.it-codigo = c-cod-item
        exclusive-lock no-error.
    if avail b-tt-estrutura then do :
        /* Busca Saldo Atual em Quantidade */
        for each saldo-estoq fields (saldo-estoq.qtidade-atu)
            where saldo-estoq.it-codigo    = c-cod-item 
              and saldo-estoq.cod-estabel  = c-estabelec
              no-lock :       
              assign b-tt-estrutura.qtidade-ini = b-tt-estrutura.qtidade-ini + saldo-estoq.qtidade-atu
                     b-tt-estrutura.qtidade-fin = b-tt-estrutura.qtidade-fin + saldo-estoq.qtidade-atu.
        end.
        
        find first saldo-estoq
            where saldo-estoq.it-codigo    = c-cod-item
              and saldo-estoq.cod-estabel  = c-estabelec
              no-lock no-error.
        
        if not avail saldo-estoq then do:
           assign b-tt-estrutura.qtidade-ini        = 0 
                  b-tt-estrutura.qtidade-fin        = 0
                  b-tt-estrutura.sald-cash-ini[1]   = 0
                  b-tt-estrutura.sald-cash-ini[2]   = 0.
        end.    
        else do:
            for each item-estab fields (cod-estabel
                                        item-estab.sald-ini-ggf-m
                                        item-estab.sald-atu-ggf-m
                                        item-estab.sald-ini-mob-m
                                        item-estab.sald-atu-mob-m)
                where item-estab.it-codigo   = c-cod-item
                  and item-estab.cod-estabel = c-estabelec
                  no-lock with 1 down:

                assign b-tt-estrutura.sald-cash-ini[1]   = b-tt-estrutura.sald-cash-ini[1]   + item-estab.sald-ini-ggf-m[1]
                       b-tt-estrutura.sald-cash-ini[2]   = b-tt-estrutura.sald-cash-ini[2]   + item-estab.sald-ini-ggf-m[2]
                       b-tt-estrutura.sald-nocash-ini[1] = b-tt-estrutura.sald-nocash-ini[1] + item-estab.sald-ini-mob-m[1]
                       b-tt-estrutura.sald-nocash-ini[2] = b-tt-estrutura.sald-nocash-ini[2] + item-estab.sald-ini-mob-m[2].
        
                for each movto-estoq fields (movto-estoq.tipo-trans
                                             movto-estoq.dt-trans
                                             movto-estoq.quantidade
                                             movto-estoq.valor-ggf-m
                                             movto-estoq.valor-mob-m) use-index item-data
                    where movto-estoq.cod-estabel  = item-estab.cod-estabel
                      and movto-estoq.it-codigo    = c-cod-item
                      and movto-estoq.dt-trans    >= da-data-ini
                      no-lock:
        
                    if movto-estoq.tipo-trans = 1 then do: 
                        if movto-estoq.dt-trans > da-data-fin then
                            assign b-tt-estrutura.qtidade-ini = b-tt-estrutura.qtidade-ini - movto-estoq.quantidade.
                        else
                            assign b-tt-estrutura.qtidade-ini = b-tt-estrutura.qtidade-ini - movto-estoq.quantidade.
                    end.
                    else do:
                        if movto-estoq.dt-trans > da-data-fin then
                           assign b-tt-estrutura.qtidade-ini = b-tt-estrutura.qtidade-ini + movto-estoq.quantidade.
                        else
                           assign b-tt-estrutura.qtidade-ini = b-tt-estrutura.qtidade-ini + movto-estoq.quantidade.
                    end.
                end.
            end.
        end.
    end.
end procedure.

procedure piMovtoItem :
    define input  parameter  c-cod-item  like item.it-codigo             no-undo.
    define input  parameter  da-data-ini like param-estoq.mensal-ate     no-undo.
    define input  parameter  da-data-fin like param-estoq.mensal-ate     no-undo.

    define buffer b-tt-estrutura for tt-estrutura.
    define buffer b1-tt-estrutura for tt-estrutura.

    find first b-tt-estrutura
        where b-tt-estrutura.it-codigo = c-cod-item exclusive-lock no-error.
    if avail b-tt-estrutura then do :

        for each ord-prod 
            where ord-prod.it-codigo = b-tt-estrutura.it-codigo no-lock :

            assign b-tt-estrutura.qtidade-movto = b-tt-estrutura.qtidade-movto + ord-prod.qt-produzida.

            /*Valores de CASH do Periodo*/
            for each movto-ggf fields (movto-ggf.dt-trans
                                       movto-ggf.it-codigo 
                                       movto-ggf.tipo-trans
                                       movto-ggf.nr-ord-produ
                                       movto-ggf.valor-ggf-1-m movto-ggf.valor-ggf-2-m movto-ggf.valor-ggf-3-m movto-ggf.valor-ggf-4-m movto-ggf.valor-ggf-5-m movto-ggf.valor-ggf-6-m)
                where movto-ggf.nr-ord-produ = ord-prod.nr-ord-produ
                and   movto-ggf.dt-trans >= da-data-ini
                and   movto-ggf.dt-trans <= da-data-fin use-index data no-lock :
    
                if movto-ggf.tipo-trans = 1 then
                    assign b-tt-estrutura.sald-cash-movto[1] = b-tt-estrutura.sald-cash-movto[1] + (movto-ggf.valor-ggf-1-m[1] + movto-ggf.valor-ggf-2-m[1] + movto-ggf.valor-ggf-3-m[1]
                                                                                                 +  movto-ggf.valor-ggf-4-m[1] + movto-ggf.valor-ggf-5-m[1] + movto-ggf.valor-ggf-6-m[1])
                           b-tt-estrutura.sald-cash-movto[2] = b-tt-estrutura.sald-cash-movto[2] + (movto-ggf.valor-ggf-1-m[2] + movto-ggf.valor-ggf-2-m[2] + movto-ggf.valor-ggf-3-m[2]
                                                                                                 +  movto-ggf.valor-ggf-4-m[2] + movto-ggf.valor-ggf-5-m[2] + movto-ggf.valor-ggf-6-m[2]).
                else
                    assign b-tt-estrutura.sald-cash-movto[1] = b-tt-estrutura.sald-cash-movto[1] - (movto-ggf.valor-ggf-1-m[1] + movto-ggf.valor-ggf-2-m[1] + movto-ggf.valor-ggf-3-m[1]
                                                                                                 +  movto-ggf.valor-ggf-4-m[1] + movto-ggf.valor-ggf-5-m[1] + movto-ggf.valor-ggf-6-m[1])
                           b-tt-estrutura.sald-cash-movto[2] = b-tt-estrutura.sald-cash-movto[2] - (movto-ggf.valor-ggf-1-m[2] + movto-ggf.valor-ggf-2-m[2] + movto-ggf.valor-ggf-3-m[2]
                                                                                                 +  movto-ggf.valor-ggf-4-m[2] + movto-ggf.valor-ggf-5-m[2] + movto-ggf.valor-ggf-6-m[2]).
            end.

            /*Valores de NO CASH do Periodo*/
            for each movto-dir fields (movto-dir.tipo-trans
                                       movto-dir.valor-mob-m
                                       movto-dir.nr-ord-produ
                                       movto-dir.dt-trans)
                where movto-dir.nr-ord-produ = ord-prod.nr-ord-produ
                and   movto-dir.dt-trans >= da-data-ini
                and   movto-dir.dt-trans <= da-data-fin use-index data no-lock :

                if movto-dir.tipo-trans = 1 then
                    assign b-tt-estrutura.sald-nocash-movto[1] = b-tt-estrutura.sald-nocash-movto[1] + movto-dir.valor-mob-m[1]
                           b-tt-estrutura.sald-nocash-movto[2] = b-tt-estrutura.sald-nocash-movto[2] + movto-dir.valor-mob-m[2].
                else
                    assign b-tt-estrutura.sald-nocash-movto[1] = b-tt-estrutura.sald-nocash-movto[1] - movto-dir.valor-mob-m[1]
                           b-tt-estrutura.sald-nocash-movto[2] = b-tt-estrutura.sald-nocash-movto[2] - movto-dir.valor-mob-m[2].
            end.
            /*Valores FROM*/
            for each movto-mat fields(movto-mat.esp-docto
                                     movto-mat.nr-ord-produ
                                     movto-mat.dt-trans
                                     movto-mat.quantidade    
                                     movto-mat.valor-ggf-m
                                     movto-mat.valor-mob-m)
               where movto-mat.nr-ord-produ = ord-prod.nr-ord-produ
               and   movto-mat.dt-trans    >= da-data-ini
               and   movto-mat.dt-trans    <= da-data-fin use-index data no-lock :
    
               case movto-mat.esp-docto :
                   when 28 then /* REQ - Requisicao            */
                       assign b-tt-estrutura.qtidade-from        = b-tt-estrutura.qtidade-from        + movto-mat.quantidade
                              b-tt-estrutura.sald-cash-from[1]   = b-tt-estrutura.sald-cash-from[1]   + movto-mat.valor-ggf-m[1]
                              b-tt-estrutura.sald-cash-from[2]   = b-tt-estrutura.sald-cash-from[2]   + movto-mat.valor-ggf-m[2]
                              b-tt-estrutura.sald-nocash-from[1] = b-tt-estrutura.sald-nocash-from[1] + movto-mat.valor-mob-m[1]
                              b-tt-estrutura.sald-nocash-from[2] = b-tt-estrutura.sald-nocash-from[2] + movto-mat.valor-mob-m[2].
                   when 31 then /* RRQ - Retorno de Requisicao */
                       assign b-tt-estrutura.qtidade-from        = b-tt-estrutura.qtidade-from        - movto-mat.quantidade
                              b-tt-estrutura.sald-cash-from[1]   = b-tt-estrutura.sald-cash-from[1]   - movto-mat.valor-ggf-m[1]
                              b-tt-estrutura.sald-cash-from[2]   = b-tt-estrutura.sald-cash-from[2]   - movto-mat.valor-ggf-m[2]
                              b-tt-estrutura.sald-nocash-from[1] = b-tt-estrutura.sald-nocash-from[1] - movto-mat.valor-mob-m[1]
                              b-tt-estrutura.sald-nocash-from[2] = b-tt-estrutura.sald-nocash-from[2] - movto-mat.valor-mob-m[2].
               end case.
            end.

        end.

        if b-tt-estrutura.codigo-refer = 'xxx' then
            assign b-tt-estrutura.qtidade-from = b-tt-estrutura.qtidade-movto.
            
        IF tt-estrutura.it-codigo <> tt-param.it-codigo THEN DO :    

            for each movto-estoq fields(movto-estoq.esp-docto
                                        movto-estoq.nr-ord-produ
                                        movto-estoq.dt-trans
                                        movto-estoq.quantidade    
                                        movto-estoq.valor-ggf-m
                                        movto-estoq.valor-mob-m)
                where movto-estoq.it-codigo    = b-tt-estrutura.it-codigo
                and   movto-estoq.nr-ord-produ > 0
                and   movto-estoq.dt-trans    >= da-data-ini
                and   movto-estoq.dt-trans    <= da-data-fin use-index item-data no-lock :
    
                case movto-estoq.esp-docto :
                    when 28 then /* REQ - Requisicao            */
                        assign b-tt-estrutura.qtidade-to        = b-tt-estrutura.qtidade-to        - movto-estoq.quantidade
                               b-tt-estrutura.sald-cash-to[1]   = b-tt-estrutura.sald-cash-to[1]   - movto-estoq.valor-ggf-m[1]
                               b-tt-estrutura.sald-cash-to[2]   = b-tt-estrutura.sald-cash-to[2]   - movto-estoq.valor-ggf-m[2]
                               b-tt-estrutura.sald-nocash-to[1] = b-tt-estrutura.sald-nocash-to[1] - movto-estoq.valor-mob-m[1]
                               b-tt-estrutura.sald-nocash-to[2] = b-tt-estrutura.sald-nocash-to[2] - movto-estoq.valor-mob-m[2].
                    when 31 then /* RRQ - Retorno de Requisicao */
                        assign b-tt-estrutura.qtidade-to        = b-tt-estrutura.qtidade-to        + movto-estoq.quantidade
                               b-tt-estrutura.sald-cash-to[1]   = b-tt-estrutura.sald-cash-to[1]   + movto-estoq.valor-ggf-m[1]
                               b-tt-estrutura.sald-cash-to[2]   = b-tt-estrutura.sald-cash-to[2]   + movto-estoq.valor-ggf-m[2]
                               b-tt-estrutura.sald-nocash-to[1] = b-tt-estrutura.sald-nocash-to[1] + movto-estoq.valor-mob-m[1]
                               b-tt-estrutura.sald-nocash-to[2] = b-tt-estrutura.sald-nocash-to[2] + movto-estoq.valor-mob-m[2].
                end case.
            end.
            
        END.

    end.
end procedure.

PROCEDURE piMovtoCPV :
    DEFINE INPUT  PARAMETER  c-cod-item  LIKE item.it-codigo             NO-UNDO.
    DEFINE INPUT  PARAMETER  c-estabelec LIKE estabelec.cod-estabel      NO-UNDO.
    DEFINE INPUT  PARAMETER  da-data-ini LIKE param-estoq.mensal-ate     NO-UNDO.
    DEFINE INPUT  PARAMETER  da-data-fin LIKE param-estoq.mensal-ate     NO-UNDO.

    DEFINE BUFFER b-tt-estrutura FOR tt-estrutura.
    DEFINE VARIABLE c-esp-docto LIKE movto-estoq.esp-docto NO-UNDO.
    
    IF c-cod-item = "97120040" THEN
        ASSIGN c-esp-docto = 21. /* NFE */
    ELSE     
        ASSIGN c-esp-docto = 22. /* NFS */
    
    FIND FIRST b-tt-estrutura WHERE 
               b-tt-estrutura.cod-estabel = c-estabelec AND
               b-tt-estrutura.it-codigo   = c-cod-item EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL b-tt-estrutura THEN DO :
    
        FOR EACH movto-estoq FIELDS (movto-estoq.tipo-trans
                                     movto-estoq.dt-trans
                                     movto-estoq.quantidade
                                     movto-estoq.valor-ggf-m
                                     movto-estoq.valor-mob-m) USE-INDEX item-data
            WHERE movto-estoq.cod-estabel  = c-estabelec
              AND movto-estoq.it-codigo    = c-cod-item
              AND movto-estoq.dt-trans    >= da-data-ini
              AND movto-estoq.dt-trans    <= da-data-fin
              AND movto-estoq.esp-docto    = c-esp-docto
              NO-LOCK :             

              ASSIGN b-tt-estrutura.qtidade-to        = b-tt-estrutura.qtidade-to        - movto-estoq.quantidade
                     b-tt-estrutura.sald-cash-to[1]   = b-tt-estrutura.sald-cash-to[1]   - movto-estoq.valor-ggf-m[1]
                     b-tt-estrutura.sald-cash-to[2]   = b-tt-estrutura.sald-cash-to[2]   - movto-estoq.valor-ggf-m[2]
                     b-tt-estrutura.sald-nocash-to[1] = b-tt-estrutura.sald-nocash-to[1] - movto-estoq.valor-mob-m[1]
                     b-tt-estrutura.sald-nocash-to[2] = b-tt-estrutura.sald-nocash-to[2] - movto-estoq.valor-mob-m[2].
        END.

    END.

END PROCEDURE.


function fnTotalGeral returns char (input TotGeral as character).

    define variable c-Formula  as character no-undo.
    define variable c-Operador as character no-undo.
    define variable i-Count    as integer   no-undo.

    do i-Count = 1 to num-entries(TotGeral) :
        if i-Count > 1 then
            assign c-Operador = '+'.

        assign c-Formula = c-Formula + 
                           c-Operador + 
                           'R[' + 
                           string(integer(entry(i-Count,TotGeral)) - iLineCount) 
                           + ']C'.
    end.
    
    return c-Formula.
end function.

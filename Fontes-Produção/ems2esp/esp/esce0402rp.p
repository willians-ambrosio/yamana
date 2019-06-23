/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.
/************************************************************************
**
**  i-prgvrs.i - Programa para criacao do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alterado para possuir a definiá∆o dos prÇprocessadores logo no in°cio do programa*/
/**** Alteraá∆o efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de vers∆o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR */
/*Esta include est† sendo liberada vazia para o EMS 2
 para n∆o ocorrer erros de compilaá∆o*/
 


/* Fim */


/* Fim */
    

/* Fim */
    
.    
/* Fim */

 
/*Fim alteraá∆o 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.001[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.001"
       c-prg-obj = "esce0402rp".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/*{include/i-ctrlrp.i {1}}*/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/

/*Fim alteraá∆o 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "esce0402rp"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.

    /*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/
    
        /*FO 1329.898 - tech1139 - 01/08/2006 */
        PUT "esce0402rp" AT 1 "2.00.00.001" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
        /*FO 1329.898 - tech1139 - 01/08/2006 */
    
    /*Fim alteraá∆o 08/09/2006*/
                                                  
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


        
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                        
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/

/* alteraá∆o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari†vel acima foi definida */ 

/* fim da alateraá∆o */

/* Alteraá∆o realizada por tech38629 - 19/07/2006 - Definiá∆o do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de criaá∆o: 19/07/2006                                  */
/* Descriá∆o: Define o prÇ-processador que indica a utilizaá∆o  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da alteraá∆o */

   /*** 010001 ***/
/********************************************************************************
***
*** ESCE0402RP - Gerador de Arquivo Excel XML para XLS
***
********************************************************************************/
{utp/ut-glob.i}
 
/* Parameters Definitions ---                                           */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as character format "x(35)":U
    field usuario          as character format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field it-codigo        like item.it-codigo
    field data-ini         like param-estoq.mensal-ate 
    field data-fim         like param-estoq.mensal-ate 
    field mes-nome         as character
    field nome-empresa     like estabelec.nome.

define temp-table tt-estrutura no-undo
    field it-codigo    like item.it-codigo
    field inform-compl like item.inform-compl
    field codigo-refer like item.codigo-refer
    field cod-estabel  like item.cod-estabel
    field niv-mais-bai like item.niv-mais-bai
    field un           like item.un
    field num-sequen   as integer column-label 'Seq'
    
    field qtidade-ajust     like saldo-estoq.qtidade-ini column-label 'Qtde Ajust'

    field qtidade-ini       like saldo-estoq.qtidade-ini column-label 'Qtde Inic'
    field qtidade-from      like saldo-estoq.qtidade-ini column-label 'Qtde From'
    field qtidade-movto     like saldo-estoq.qtidade-ini column-label 'Qtde Movto'
    field qtidade-to        like saldo-estoq.qtidade-ini column-label 'Qtde To'
    field qtidade-fin       like saldo-estoq.qtidade-fin column-label 'Qtde Fin'
                           
    field sald-cash-ini     like movto-estoq.valor-mat-m column-label 'Vl Cash Inic' 
    field sald-cash-from    like movto-estoq.valor-mat-m column-label 'Vl Cash From' 
    field sald-cash-movto   like movto-estoq.valor-mat-m column-label 'Vl Cash Movto'
    field sald-cash-to      like movto-estoq.valor-mat-m column-label 'Vl Cash To'   
    field sald-cash-fin     like movto-estoq.valor-mat-m column-label 'Vl Cash Fin'  

    field sald-nocash-ini   like movto-estoq.valor-mat-m column-label 'Vl No Cash Inic' 
    field sald-nocash-from  like movto-estoq.valor-mat-m column-label 'Vl No Cash From' 
    field sald-nocash-movto like movto-estoq.valor-mat-m column-label 'Vl No Cash Movto'
    field sald-nocash-to    like movto-estoq.valor-mat-m column-label 'Vl No Cash To'   
    field sald-nocash-fin   like movto-estoq.valor-mat-m column-label 'Vl No Cash Fin'  

    index id is primary unique num-sequen it-codigo ascending.
 
define input parameter table for tt-param.

/* Definiá∆o de Vari†veis Locais ---                                    */
define variable h-acomp           as handle    no-undo.

define variable i-niv-mais-bai    as integer   no-undo.
define variable i-num-sequen      as integer   no-undo.
define variable de-qtidade-to     like saldo-estoq.qtidade-ini.
define variable de-sald-cash-to   like movto-estoq.valor-mat-m.
define variable de-sald-nocash-to like movto-estoq.valor-mat-m.
define variable i-nmb-old         as integer   no-undo.
define variable AccumLineCash     as character no-undo.
define variable AccumLineNoCash   as character no-undo.
define variable AccumLineTotal    as character no-undo.
define variable un-medida-old     as character no-undo.
define variable de-qtidade-ajust  like saldo-estoq.qtidade-ini.

/* XML Definitions ---                                                  */
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

    /* reinicia a contagem das colunas sempre que a TAG da linha Ç fechada */
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

    /* convers∆o das datas para padr∆o XML */
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

    def input param iNumberCols as int  no-undo. /* n£mero de cÇlulas da coluna a serem mescladas a partir da cÇlula corrente */
    def input param cData       as char no-undo.
    def input param cStyle      as char no-undo.

    assign iColCount = iColCount + 1. /* pr¢xima cÇlula da coluna */

    put unformatted
        '         <Cell ss:MergeAcross="' iNumberCols '" ss:StyleID="' cStyle '"><Data ss:Type="String">' cData '</Data></Cell>' skip.

    /* ap¢s mesclar as cÇlulas atualiza a coluna corrente, retirando a cÇlula (coluna) acrescenta acima */
    assign iColCount = iColCount + iNumberCols - 1.

end procedure.

procedure MergeAcross:

    def input param iNumberCols as int  no-undo. /* n£mero de cÇlulas da coluna a serem mescladas a partir da cÇlula corrente */
    def input param iNumberDown as int  no-undo.
    def input param cData       as char no-undo.
    def input param cStyle      as char no-undo.

    assign iColCount = iColCount + 1. /* pr¢xima cÇlula da coluna */

    put unformatted
        '         <Cell ss:MergeAcross="' iNumberCols '" ss:MergeDown="' iNumberDown '" ss:StyleID="' cStyle '"><Data ss:Type="String">' cData '</Data></Cell>' skip.

    /* ap¢s mesclar as cÇlulas atualiza a coluna corrente, retirando a cÇlula (coluna) acrescenta acima */
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
      Convers∆o do arquivo XML para XLS
      =================================*/


    def var v-arquivo as char       no-undo.
    def var ch-excel  as com-handle no-undo.   
    
    if  iLineCount > 65500 then do: /* o Excel Ç limitado a 65536 linhas */
        run utp/ut-msgs.p(input "show",
                          input 17006,
                          input "O relat¢rio Excel atingiu o limite de 65.500 linhas" + "~~" +
                                "Por limitaá‰es do Microsoft Excel o relat¢rio Ç limitado a " +
                                "65.500 linhas. Deste modo, dever† ser informada " +
                                "uma faixa de seleá∆o que limite a impress∆o dos dados.").

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

        /* Convers∆o do arquivo .XML para .XLS */
        /* -4143: constante de convers∆o de arquivo .XML para .XLS */
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
  

run utp/ut-acomp.p persistent set h-acomp.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Exportando...",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
        
run pi-inicializar in h-acomp (input return-value).

find first tt-param no-lock no-error.
if not avail tt-param then do :
    run pi-finalizar in h-acomp.
    return "NOK":U.
end.

/* Alteraá∆o da Extens∆o do Arquivo ---                                 */
assign tt-param.arquivo = replace(tt-param.arquivo, ".tmp", ".xml")
       tt-param.arquivo = lower(tt-param.arquivo).

/* Altera ParÉmetro da Sess∆o para Impress∆o de Dados Decimais ---      */
assign cNumericFormat = session:numeric-format
       session:numeric-format = "AMERICAN".

/* Abertura do Arquivo para Impress∆o do XML ---                        */
output to value(tt-param.arquivo) convert target "utf-8":U.

    run piExecute.

output close. 

/*=================================
  Convers∆o do arquivo XML para XLS
  =================================*/

run piConvertXMLToXLS.

/* Retorna os ParÉmetros da Sess∆o */
assign session:numeric-format = cNumericFormat.

run pi-finalizar in h-acomp.

return "OK":U.

procedure piExecute :

    /* Determina o N£mero de Colunas da Planilha */
    assign iColumns = 20.
    
        run OpenWorkBook.
    run OpenExcelWorkBook.
    run CloseExcelWorkBook .
        run OpenStyles.
        run CloseStyles.
        run OpenWorkSheet(input tt-param.mes-nome). /* Nome da Planilha */
    run OpenTable(input iColumns).

    /* Definiá∆o da Largura das Colunas */
    run setColumnWidth(input   "132.00").
    run setColumnWidth(input   "048.75").
    run setColumnWidth(input   "063.75").
    run setColumnWidth(input   "055.50").
    run setColumnWidth(input   "057.75").
    run setColumnWidth(input   "069.00").
    run setColumnWidth(input   "055.50").
    run setColumnWidth(input   "057.75").
    run setColumnWidth(input   "063.75").
    run setColumnWidth(input   "055.50").
    run setColumnWidth(input   "057.75").
    run setColumnWidth(input   "072.75").
    run setColumnWidth(input   "055.50").
    run setColumnWidth(input   "061.50").
    run setColumnWidth(input   "056.25").
    run setColumnWidth(input   "069.00").
    run setColumnWidth(input   "055.50").
    run setColumnWidth(input   "057.75").
    run setColumnWidth(input   "087.00").

    run OpenLine.
        run MergeColumns(input 17, tt-param.nome-empresa, "s199").
    run CloseLine.

    run OpenLine.
        run MergeColumns(input 17, "GOLD STOCK CONTROL", "s200"). 
    run CloseLine.
    
    run OpenLine.
        run PrintDataString(input "", "s31").
    run CloseLine.
    
    run OpenLine.
        run MergeAcross (input 1,2,""                   , "m18227260").
        run MergeColumns(input 2, "Initial Stock"       , "m18227442").
        run MergeColumns(input 2, "Transferred ( from )", "m18227452").
        run MergeColumns(input 2, "Moviment"            , "m18227482").
        run MergeColumns(input 2, "Transferred ( to )"  , "m18227108").
        run MergeColumns(input 0, "Adjust"              , "s30").
        run MergeColumns(input 2, "Final Stock"         , "m18227462").
    run CloseLine.
                                                              
    run OpenLine.
        run PrintDataStringEsp(input 3,input "Values" , "s30").
        run PrintDataString   (input ""               , "s32").
        run PrintDataString   (input "Ore"            , "s33").
        run PrintDataString   (input "Values"         , "s30").
        run PrintDataString   (input ""               , "s32").
        run PrintDataString   (input "Ore"            , "s33").
        run PrintDataString   (input "Values"         , "s30").
        run PrintDataString   (input ""               , "s32").
        run PrintDataString   (input "Ore"            , "s33").
        run PrintDataString   (input "Values"         , "s30").
        run PrintDataString   (input ""               , "s32").
        run PrintDataString   (input "Ore"            , "s33").
        run PrintDataString   (input "Ore"            , "s33").
        run PrintDataString   (input "Values"         , "s32").
        run PrintDataString   (input ""               , "s34").
        run PrintDataString   (input "Ore"            , "s33").
    run CloseLine.

    run OpenLine.
        run PrintDataStringEsp(input 3,input "Reais-R$" , "s32").
        run PrintDataString   (input "Dolar-US$"        , "s34").
        run PrintDataString   (input "Ton"              , "s33").
        run PrintDataString   (input "Reais-R$"         , "s32").
        run PrintDataString   (input "Dolar-US$"        , "s34").
        run PrintDataString   (input "Ton"              , "s33").
        run PrintDataString   (input "Reais-R$"         , "s32").
        run PrintDataString   (input "Dolar-US$"        , "s34").
        run PrintDataString   (input "Ton"              , "s33").
        run PrintDataString   (input "Reais-R$"         , "s32").
        run PrintDataString   (input "Dolar-US$"        , "s34").
        run PrintDataString   (input "Ton"              , "s33").
        run PrintDataString   (input "Ton"              , "s33"). 
        run PrintDataString   (input "Reais-R$"         , "s32").
        run PrintDataString   (input "Dolar-US$"        , "s34").
        run PrintDataString   (input "Ton"              , "s33").
    run CloseLine.
                                                              
    find first item 
        where item.it-codigo = tt-param.it-codigo
        no-lock no-error.

    create tt-estrutura.
    assign i-niv-mais-bai            = 0 
           i-num-sequen              = 1
           tt-estrutura.it-codigo    = item.it-codigo
           tt-estrutura.cod-estabel  = item.cod-estabel
           tt-estrutura.inform-compl = item.inform-compl
           tt-estrutura.codigo-refer = item.codigo-refer
           tt-estrutura.un           = item.un
           tt-estrutura.niv-mais-bai = i-niv-mais-bai
           tt-estrutura.num-sequen   = i-num-sequen.
    
    run piExplodeEstrutura(input tt-param.it-codigo).

    find last tt-estrutura no-lock no-error.
    if avail tt-estrutura then
        assign un-medida-old = tt-estrutura.un.


    /*Logica Por Item */
    for each tt-estrutura
        by tt-estrutura.num-sequen descending.

        run piSaldoIni(input tt-estrutura.it-codigo,     
                       input tt-estrutura.cod-estabel,                       
                       input tt-param.data-ini,                
                       input tt-param.data-fim).               
    
        if i-nmb-old <> tt-estrutura.niv-mais-bai then
            assign tt-estrutura.qtidade-from      = de-qtidade-to
                   tt-estrutura.sald-cash-to[1]   = de-sald-cash-to[1]  
                   tt-estrutura.sald-nocash-to[1] = de-sald-nocash-to[1]
                   i-nmb-old                      = tt-estrutura.niv-mais-bai
                   de-qtidade-to                  = 0.
    
        run piMovtoItem(input tt-estrutura.it-codigo,
                        input tt-param.data-ini,
                        input tt-param.data-fim).                        
                                                
        IF tt-estrutura.it-codigo = tt-param.it-codigo THEN
            run piMovtoCPV(input tt-estrutura.it-codigo,
                           input tt-estrutura.cod-estabel,
                           input tt-param.data-ini,
                           input tt-param.data-fim).

        if  length(trim(tt-estrutura.inform-compl)) = 0 then do :
            assign de-qtidade-ajust = tt-estrutura.qtidade-movto.
            next.
        end.

        if de-qtidade-ajust > 0 then
            assign tt-estrutura.qtidade-ajust = tt-estrutura.qtidade-movto - de-qtidade-ajust
                   de-qtidade-ajust           = 0.

        if tt-estrutura.un <> un-medida-old then do :
            assign tt-estrutura.qtidade-from = 0
                   un-medida-old             = tt-estrutura.un. 
                                                      
            run OpenLine.
                run PrintDataString(input "", "s31"). 
            run CloseLine.

            run OpenLine.
                run MergeAcross (input 1,2,""                   , "m18227270").
                run MergeColumns(input 2, "Initial Stock"       , "m18227118").
                run MergeColumns(input 2, "Transferred ( from )", "m18227128").
                run MergeColumns(input 2, "Moviment"            , "m18227138").
                run MergeColumns(input 2, "Transferred ( to )"  , "m18227148").
                run MergeColumns(input 0, "Adjust"              , "s30").
                run MergeColumns(input 2, "Final Stock"         , "m18226956").
            run CloseLine.

            run OpenLine.
                run PrintDataStringEsp(input 3,input "Values" , "s30").
                run PrintDataString   (input ""               , "s32").
                run PrintDataString   (input "Ore"            , "s33").
                run PrintDataString   (input "Values"         , "s30").
                run PrintDataString   (input ""               , "s32").
                run PrintDataString   (input "Ore"            , "s33").
                run PrintDataString   (input "Values"         , "s30").
                run PrintDataString   (input ""               , "s32").
                run PrintDataString   (input "Ore"            , "s33").
                run PrintDataString   (input "Values"         , "s30").
                run PrintDataString   (input ""               , "s32").
                run PrintDataString   (input "Ore"            , "s33").
                run PrintDataString   (input "Ore"            , "s33").
                run PrintDataString   (input "Values"         , "s32").
                run PrintDataString   (input ""               , "s34").
                run PrintDataString   (input "Ore"            , "s33").
            run CloseLine.
        
            run OpenLine.
                run PrintDataStringEsp(input 3,input "Reais-R$" , "s32").
                run PrintDataString   (input "Dolar-US$"        , "s34").
                run PrintDataString   (input "Gramma"           , "s33").
                run PrintDataString   (input "Reais-R$"         , "s32").
                run PrintDataString   (input "Dolar-US$"        , "s34").
                run PrintDataString   (input "Gramma"           , "s33").
                run PrintDataString   (input "Reais-R$"         , "s32").
                run PrintDataString   (input "Dolar-US$"        , "s34").
                run PrintDataString   (input "Gramma"           , "s33").
                run PrintDataString   (input "Reais-R$"         , "s32").
                run PrintDataString   (input "Dolar-US$"        , "s34").
                run PrintDataString   (input "Gramma"           , "s33").
                run PrintDataString   (input "Gramma"           , "s33"). 
                run PrintDataString   (input "Reais-R$"         , "s32").
                run PrintDataString   (input "Dolar-US$"        , "s34").
                run PrintDataString   (input "Gramma"           , "s33").
            run CloseLine.                              
        end.             

        assign tt-estrutura.qtidade-fin        = tt-estrutura.qtidade-ini        + tt-estrutura.qtidade-movto       + tt-estrutura.qtidade-to           + tt-estrutura.qtidade-ajust
               tt-estrutura.sald-cash-fin[1]   = tt-estrutura.sald-cash-ini[1]   + tt-estrutura.sald-cash-from[1]   + tt-estrutura.sald-cash-movto[1]   + tt-estrutura.sald-cash-to[1]  
               tt-estrutura.sald-cash-fin[2]   = tt-estrutura.sald-cash-ini[2]   + tt-estrutura.sald-cash-from[2]   + tt-estrutura.sald-cash-movto[2]   + tt-estrutura.sald-cash-to[2]  
               tt-estrutura.sald-nocash-fin[1] = tt-estrutura.sald-nocash-ini[1] + tt-estrutura.sald-nocash-from[1] + tt-estrutura.sald-nocash-movto[1] + tt-estrutura.sald-nocash-to[1]
               tt-estrutura.sald-nocash-fin[2] = tt-estrutura.sald-nocash-ini[2] + tt-estrutura.sald-nocash-from[2] + tt-estrutura.sald-nocash-movto[2] + tt-estrutura.sald-nocash-to[2].

        run OpenLine.
            run PrintDataString(input "", "s31"). 
        run CloseLine.
                                              
        run OpenLine.
            run MergeAcross       (input 0,2,tt-estrutura.inform-compl  , "m18226966"). 
            run PrintDataStringEsp(input 2,input "Cash"                 , "s75"). 
            run PrintDataNumber   (input tt-estrutura.sald-cash-ini[1]  , "s113").       
            run PrintDataNumber   (input tt-estrutura.sald-cash-ini[2]  , "s114").       
            run PrintDataNumber   (input tt-estrutura.qtidade-ini       , "s96").  
            run PrintDataNumber   (input tt-estrutura.sald-cash-from[1] , "s116").       
            run PrintDataNumber   (input tt-estrutura.sald-cash-from[2] , "s117").       
            run PrintDataNumber   (input tt-estrutura.qtidade-from      , "s115"). 
            run PrintDataNumber   (input tt-estrutura.sald-cash-movto[1], "s118").       
            run PrintDataNumber   (input tt-estrutura.sald-cash-movto[2], "s41").       
            run PrintDataNumber   (input tt-estrutura.qtidade-movto     , "s105").
            run PrintDataNumber   (input tt-estrutura.sald-cash-to[1]   , "s120").      
            run PrintDataNumber   (input tt-estrutura.sald-cash-to[2]   , "s119").      
            run PrintDataNumber   (input tt-estrutura.qtidade-to        , "s106"). 
            run PrintDataNumber   (input tt-estrutura.qtidade-ajus      , "s107"). 
            run PrintDataNumber   (input tt-estrutura.sald-cash-fin[1]  , "s98").      
            run PrintDataNumber   (input tt-estrutura.sald-cash-fin[2]  , "s121").      
            run PrintDataNumber   (input tt-estrutura.qtidade-fin       , "s89").

            if AccumLineCash = '' then
                assign AccumLineCash = string(iLineCount).
            else
                assign AccumLineCash = AccumLineCash + ',' + string(iLineCount).
        run CloseLine.          
        
        run OpenLine.
            run PrintDataStringEsp(input 2,input "No Cash"                , "s125").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-ini[1]  , "s83").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-ini[2]  , "s48").
            run PrintDataNumber   (input tt-estrutura.qtidade-ini         , "s104").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-from[1] , "s122").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-from[2] , "s123").
            run PrintDataNumber   (input tt-estrutura.qtidade-from        , "s124").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-movto[1], "s47").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-movto[2], "s48").
            run PrintDataNumber   (input tt-estrutura.qtidade-movto       , "s49").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-to[1]   , "s52").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-to[2]   , "s86").
            run PrintDataNumber   (input tt-estrutura.qtidade-to          , "s93").
            run PrintDataNumber   (input tt-estrutura.qtidade-ajus        , "s95"). 
            run PrintDataNumber   (input tt-estrutura.sald-nocash-fin[1]  , "s47").
            run PrintDataNumber   (input tt-estrutura.sald-nocash-fin[2]  , "s48").
            run PrintDataNumber   (input tt-estrutura.qtidade-fin         , "s49").

            if AccumLineNoCash = '' then
                assign AccumLineNoCash = string(iLineCount).
            else
                assign AccumLineNoCash = AccumLineNoCash + ',' + string(iLineCount).
        run CloseLine.          
        
        run OpenLine.
            run PrintDataStringEsp(input 2,input "Total"           , "s53").
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s100").   
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s102").   
            run PrintDataNumber   (input tt-estrutura.qtidade-ini  , "s101").            
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s103").   
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s108").   
            run PrintDataNumber   (input tt-estrutura.qtidade-from , "s109").           
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s99").   
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s102").   
            run PrintDataNumber   (input tt-estrutura.qtidade-movto, "s101").          
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s110").   
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s111").   
            run PrintDataNumber   (input tt-estrutura.qtidade-to   , "s112").             
            run PrintDataNumberEsp(input "R[-2]C"                  , "s61").  
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s99").   
            run PrintDataNumberEsp(input "R[-2]C+R[-1]C"           , "s102").   
            run PrintDataNumber   (input tt-estrutura.qtidade-fin  , "s101").   

            if AccumLineTotal = '' then
                assign AccumLineTotal = string(iLineCount).
            else
                assign AccumLineTotal = AccumLineTotal + ',' + string(iLineCount).
        run CloseLine.                                                                 
    end.

    run OpenLine.
        run MergeAcross  (input 0,3,'TOTAL GERAL', "m18227412"). 
    run CloseLine.

    run OpenLine.
        run PrintDataStringEsp(input 2,input "Cash" , "s69").
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s126" ).  
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s41" ). 
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s89" ).  
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s147" ). 
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s117" ).  
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s42" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s118" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s41" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s105").   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s120" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s85" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s92" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s107" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s126" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s41" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineCash)   , "s97" ).   
    run CloseLine.

    run OpenLine.
        run PrintDataStringEsp(input 2,input "No Cash" , "s70"). 
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s47" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s48" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s49" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s155" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s123" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s145" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s83" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s48" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s48" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s52" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s86" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s93" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s140" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s47" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s48" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineNoCash) , "s48" ).
    run CloseLine.

    run OpenLine.
        run PrintDataStringEsp(input 2,input "Total" , "s71").
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s54" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s55" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s56" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s141" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s82" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s57" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s54" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s55" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s56" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s58" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s87" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s60" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s72" ).
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s54" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s55" ).   
        run PrintDataNumberEsp(input fnTotalGeral(AccumLineTotal)  , "s55" ).   
    run CloseLine.                                              
    /* Fim Logica */

    /*=======================
      Fim Impress∆o dos Dados
      =======================*/
    run CloseTable.
        run OpenWorksheetOptions.
        run CloseWorksheetOptions.
        run CloseWorkSheet.
        run CloseWorkBook.   
end procedure.

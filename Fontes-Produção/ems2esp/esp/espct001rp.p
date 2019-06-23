/*****************************************************************************
**       Programa: ESPCT001rp.p
**       Data....: 25/09/09
**       Autor...: DATASUL S.A.
**       Objetivo: Relatorio Contratos e Saldos
*******************************************************************************/

/****************** Definição de Tabelas Temporárias do Relatório **********************/

define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field ind-sit-contrat as integer
    field c-cod-estabel-ini like contrato-for.cod-estabel
    field c-cod-estabel-fim like contrato-for.cod-estabel
    field i-nr-contrato-ini like contrato-for.nr-contrato
    field i-nr-contrato-fim like contrato-for.nr-contrato
    field da-dt-ini-validade-ini like contrato-for.dt-ini-validade
    field da-dt-ini-validade-fim like contrato-for.dt-ini-validade
    field da-dt-ter-validade-ini like contrato-for.dt-ter-validade
    field da-dt-ter-validade-fim like contrato-for.dt-ter-validade.

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini like contrato-for.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim like contrato-for.cod-estabel format "x(3)" initial "ZZZ" no-undo.
def new shared var i-nr-contrato-ini like contrato-for.nr-contrato format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-nr-contrato-fim like contrato-for.nr-contrato format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var da-dt-ini-validade-ini like contrato-for.dt-ini-validade format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-ini-validade-fim like contrato-for.dt-ini-validade format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dt-ter-validade-ini like contrato-for.dt-ter-validade format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-ter-validade-fim like contrato-for.dt-ter-validade format "99/99/9999" initial "12/31/9999" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

def var ems2cademp-contr-ext-natureza as character no-undo.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.

def var valid-val-medicao   as dec initial 0.
def var valid-dec-2         as dec initial 0.
def var valid-sld-val-receb as dec initial 0.
def var valid-sld-val       as dec initial 0.

define variable chExcel       as component-handle no-undo.
define variable chWBook       as component-handle no-undo.
define variable chWSheet      as component-handle no-undo.
define variable chWSheet1     as component-handle no-undo.

define VARIABLE c-path-excel  as char format "x(256)" no-undo.

DEF VAR i-linha            AS INT INIT 3 NO-UNDO.

&scoped-define xlTop                 -4160
&scoped-define xlCenter              -4108
&scoped-define xlRight               -4152
&scoped-define xlLeft                -4131
&scoped-define xlEdgeLeft             7
&scoped-define xlThin                 2
&scoped-define xlPortrait             1
&scoped-define xlpaperA4              9 
&SCOPED-DEFINE xlDecimalSeparator     3

create tt-param.
raw-transfer raw-param to tt-param.

os-delete value(tt-param.arquivo).

find first tt-param no-error.
IF tt-param.destino = 3 or
   tt-param.destino = 1 THEN do:
    assign tt-param.arquivo = string(SESSION:TEMP-DIRECTORY + "ESPCT001" + STRING(time) + ".xls").
end.

ASSIGN tt-param.arquivo = replace(tt-param.arquivo, "lst", "xls")
       tt-param.arquivo = replace(tt-param.arquivo, "txt", "xls")
       tt-param.arquivo = replace(tt-param.arquivo, "csv", "xls")
       tt-param.arquivo = replace(tt-param.arquivo, "~/", "~\").

create 'Excel.Application' chExcel.
ASSIGN c-path-excel = chExcel:Path.

chExcel:SheetsInNewWorkbook = 1.
chWBook  = chExcel:Workbooks:Add().
chWSheet = chWBook:Sheets:Item(1).

chExcel:Windows:Item(1):DisplayGridlines = False.

if trim(session:printer-name) <> "" then do:
    chWSheet:PageSetup:Orientation    = {&xlPortrait}.
    chWSheet:PageSetup:PaperSize      = {&xlPaperA4}.
    chWSheet:PageSetup:LeftMargin = 2.
    chWSheet:PageSetup:TopMargin  = 2.
    ChWSheet:PageSetup:Zoom           = false.
    ChWSheet:PageSetup:FitToPagesWide = 1.
    ChWSheet:PageSetup:FitToPagesTall = 32767.
end.

chExcel:ActiveWindow:Zoom = 100.
chWSheet:cells:font:SIZE  = 9.
chExcel:visible           = FALSE.

chWSheet:Rows("1:4"):RowHeight   = 15.
chWSheet:Rows("5"):RowHeight     = 15.
chWSheet:Rows("6"):RowHeight     = 15.
chWSheet:Rows("7"):RowHeight     = 15.
chWSheet:Rows("8"):RowHeight     = 15.
chWSheet:Rows("9:256"):RowHeight = 15.

assign ind-sit-contrat = tt-param.ind-sit-contrat
       c-cod-estabel-ini = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim = tt-param.c-cod-estabel-fim
       i-nr-contrato-ini = tt-param.i-nr-contrato-ini
       i-nr-contrato-fim = tt-param.i-nr-contrato-fim
       da-dt-ini-validade-ini = tt-param.da-dt-ini-validade-ini
       da-dt-ini-validade-fim = tt-param.da-dt-ini-validade-fim
       da-dt-ter-validade-ini = tt-param.da-dt-ter-validade-ini
       da-dt-ter-validade-fim = tt-param.da-dt-ter-validade-fim.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Desc. Contrato").

for each contrato-for NO-LOCK where 
         contrato-for.cod-estabel >= c-cod-estabel-ini and 
         contrato-for.cod-estabel <= c-cod-estabel-fim and
         contrato-for.dt-ini-validade >= da-dt-ini-validade-ini and 
         contrato-for.dt-ini-validade <= da-dt-ini-validade-fim and
         contrato-for.dt-ter-validade >= da-dt-ter-validade-ini and 
         contrato-for.dt-ter-validade <= da-dt-ter-validade-fim and
         contrato-for.nr-contrato >= i-nr-contrato-ini and 
         contrato-for.nr-contrato <= i-nr-contrato-fim,
    FIRST emitente NO-LOCK where 
          emitente.cod-emitente = contrato-for.cod-emitente,
    each item-contrat NO-LOCK where 
         item-contrat.nr-contrato = contrato-for.nr-contrato,
    FIRST item NO-LOCK where 
          item.it-codigo = item-contrat.it-codigo,
    each medicao-contrat NO-LOCK where 
         medicao-contrat.nr-contrato  = contrato-for.nr-contrato  AND
         medicao-contrat.num-seq-item = item-contrat.num-seq-item
    BREAK BY contrato-for.nr-contrato
          BY medicao-contrat.dat-medicao:

    run pi-acompanhar in h-acomp(input string(contrato-for.des-cont)).

    assign ems2cademp-contr-ext-natureza = entry(contrato-for.natureza,"Compra,Servi‡o,Beneficiamento").

    IF FIRST(contrato-for.nr-contrato) THEN DO:

        chWSheet:Range("A1:R1"):MergeCells          = TRUE.
        chWSheet:Range("A1:R1"):VALUE               = "Relat¢rio de Contratos e Saldos".
        chWSheet:Range("A1:R1"):FONT:SIZE           = 11.
        chWSheet:Range("A1:R1"):FONT:BOLD           = TRUE.
        chWSheet:Range("A1:R1"):HorizontalAlignment = {&xlCenter}.
        chWSheet:Range("A1:R1"):VerticalAlignment   = {&xlCenter}.
        chWSheet:Range("A1:R1"):interior:colorindex = 5.
        chWSheet:Range("A1:R1"):FONT:ColorIndex     = 2.
        chWSheet:Rows(1):RowHeight                  = 20.     
        chWSheet:Range("A1:R1"):Borders({&xlEdgeLeftTopBottomRight}):Weight = {&xlThin}.

        chWSheet:Range("A2:R2"):MergeCells          = TRUE.

        chWSheet:Range("A3:R3"):FONT:BOLD  = TRUE.                                                                 
        chWSheet:Range("A3:R3"):Borders({&xlEdgeLeftTopBottomRight}):Weight = {&xlThin}.

        ASSIGN chWSheet:Range("A3"):VALUE = "Est"                         
               chWSheet:Range("B3"):VALUE = "Natureza"                    
               chWSheet:Range("C3"):VALUE = "Fornecedor"              
               chWSheet:Range("D3"):VALUE = "Nome Fornecedor"             
               chWSheet:Range("E3"):VALUE = "Contrato Ativo ou Inativo"   
               chWSheet:Range("F3"):VALUE = "Nr Contrato"                 
               chWSheet:Range("G3"):VALUE = "Desc. Contrato"               
               chWSheet:Range("H3"):VALUE = "In¡cio"                      
               chWSheet:Range("I3"):VALUE = "T‚rmino"                     
               chWSheet:Range("J3"):VALUE = "Limite Valor Contrato"       
               chWSheet:Range("K3"):VALUE = "Valor Movimentado"           
               chWSheet:Range("L3"):VALUE = "Vl Medi‡Æo"                  
               chWSheet:Range("M3"):VALUE = "Medi‡Æo em"                  
               chWSheet:Range("N3"):VALUE = "Tipo Controle"               
               chWSheet:Range("O3"):VALUE = "Narrativa do contrato"       
               chWSheet:Range("P3"):VALUE = "Moeda"                       
               chWSheet:Range("Q3"):VALUE = "Gestor T‚cnico"              
               chWSheet:Range("R3"):VALUE = "Saldo Valor Recebido".

        chWSheet:Range("A:R"):EntireColumn:AutoFit.

    END.
    
    assign valid-val-medicao   = 0
           valid-dec-2         = 0 
           valid-sld-val-receb = 0
           valid-sld-val       = 0
           i-linha             = i-linha + 1.

    if medicao-contrat.val-medicao <> ? THEN
        assign valid-val-medicao = medicao-contrat.val-medicao.
    if contrato-for.dec-2 <> ? THEN
        assign valid-dec-2 = contrato-for.dec-2.
    if contrato-for.sld-val-receb <> ? THEN
        ASSIGN valid-sld-val-receb = contrato-for.sld-val-receb.
    if contrato-for.sld-val <> ? THEN
        assign valid-sld-val = contrato-for.sld-val.

    chWSheet:Range("H" + STRING(i-linha)):NumberFormat = "0" + string(chExcel:Application:International({&xlDecimalSeparator})) + "00".
    chWSheet:Range("J" + STRING(i-linha)):NumberFormat = "0" + string(chExcel:Application:International({&xlDecimalSeparator})) + "00".
    chWSheet:Range("K" + STRING(i-linha)):NumberFormat = "0" + string(chExcel:Application:International({&xlDecimalSeparator})) + "00".
    chWSheet:Range("L" + STRING(i-linha)):NumberFormat = "0" + string(chExcel:Application:International({&xlDecimalSeparator})) + "00".
    chWSheet:Range("R" + STRING(i-linha)):NumberFormat = "0" + string(chExcel:Application:International({&xlDecimalSeparator})) + "00".

    chWSheet:Range("H" + STRING(i-linha)):numberformat = "@".
    chWSheet:Range("I" + STRING(i-linha)):numberformat = "@".
    chWSheet:Range("M" + STRING(i-linha)):numberformat = "@".

    ASSIGN chWSheet:Range("A"  + STRING(i-linha)):VALUE = contrato-for.cod-estabel.
           chWSheet:Range("B"  + STRING(i-linha)):VALUE = ems2cademp-contr-ext-natureza.
           chWSheet:Range("C"  + STRING(i-linha)):VALUE = contrato-for.cod-emitente.
           chWSheet:Range("D"  + STRING(i-linha)):VALUE = emitente.nome-emit.
           chWSheet:Range("E"  + STRING(i-linha)):VALUE = trim(string(contrato-for.log-libera,"Sim/NÆo")).
           chWSheet:Range("F"  + STRING(i-linha)):VALUE = contrato-for.nr-contrato.
           chWSheet:Range("G"  + STRING(i-linha)):VALUE = trim(contrato-for.des-contrat).
           chWSheet:Range("H"  + STRING(i-linha)):VALUE = string(day(contrato-for.dt-ini-validade)) + "/" + string(MONTH(contrato-for.dt-ini-validade)) + "/" + string(YEAR(contrato-for.dt-ini-validade)).
           chWSheet:Range("I"  + STRING(i-linha)):VALUE = string(day(contrato-for.dt-ter-validade)) + "/" + string(MONTH(contrato-for.dt-ter-validade)) + "/" + string(YEAR(contrato-for.dt-ter-validade)).
           chWSheet:Range("J"  + STRING(i-linha)):VALUE = valid-dec-2.
           chWSheet:Range("K"  + STRING(i-linha)):VALUE = valid-sld-val.
           chWSheet:Range("L"  + STRING(i-linha)):VALUE = valid-val-medicao.
           chWSheet:Range("M"  + STRING(i-linha)):VALUE = string(day(medicao-contrat.dat-medicao)) + "/" + string(MONTH(medicao-contrat.dat-medicao)) + "/" + string(YEAR(medicao-contrat.dat-medicao)).
           chWSheet:Range("N"  + STRING(i-linha)):VALUE = item-contrat.ind-tipo-control.
           chWSheet:Range("O"  + STRING(i-linha)):VALUE = contrato-for.narrat-contrat.
           chWSheet:Range("P"  + STRING(i-linha)):VALUE = contrato-for.moeda.
           chWSheet:Range("Q"  + STRING(i-linha)):VALUE = contrato-for.gestor-tecnico.
           chWSheet:Range("R"  + STRING(i-linha)):VALUE = valid-sld-val-receb.
    
end.

chWSheet:Range("H:I"):EntireColumn:AutoFit.

case tt-param.destino:

    when 1 then do:
        chExcel:visible = TRUE.
    end.

    when 2 then do:      
        if SEARCH(tt-param.arquivo) <> ? then 
            os-delete value(tt-param.arquivo) .
        chWBook:SaveAs(tt-param.arquivo, {&xlNormal}, "", "", False, False, False).
        chWBook:Close().
        chExcel:Quit().
    end.

    when 3 then do:
        chExcel:visible = TRUE.
    end.

end case.

if valid-handle(chWSheet) then do:
    release object chWSheet. 
end.

if valid-handle(chWBook) then do:
    release object chWBook.
end.

if valid-handle(chExcel) then do:
    release object chExcel.
end.   

assign chWSheet = ?
       chWBook  = ?
       chExcel  = ?.

IF VALID-HANDLE(h-acomp) THEN 
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */

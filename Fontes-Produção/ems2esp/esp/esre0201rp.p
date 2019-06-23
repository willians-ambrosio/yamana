/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESRE0201RP 2.06.00.001}  /*** 010025 ***/

{include/i_fnctrad.i}
/******************************************************************************
**
**  Programa: ESRE0201RP.P
**
**  Data....: 05/11/2013
**
**  Objetivo: Listagem dos Parametros dos Usuarios do Recebimento
**
**  Versao..: 

******************************************************************************/
{utp/ut-glob.i}

def new shared var h-acomp          as handle                 no-undo.

DEFINE VARIABLE l-valido            AS LOGICAL  FORMAT "SIM/NAO"   NO-UNDO.
DEFINE VARIABLE cArquivo            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chExcelApplication  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkItem          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i-linha             AS INTEGER NO-UNDO.


run utp/ut-acomp.p persistent set h-acomp. 

{cdp/cdcfgmat.i}

define temp-table tt-param
    field destino            as integer
    field arquivo            as char
    field usuario            as char format "x(12)"
    field data-exec          as date
    field hora-exec          as integer
    field classifica         as integer
    field desc-classifica    as char format "x(40)"
    field c-usu-ini          as char format "x(15)"
    field c-usu-fim          as char format "x(15)".

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

def temp-table tt-raw-digita                   
    field raw-digita as raw.
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.    

{include/i-rpvar.i} 

find first param-global no-lock no-error.
if  avail param-global then
    assign c-empresa  = param-global.grupo.

assign c-programa     = "ESRE0201"
       c-versao       = "2.06.00"
       c-revisao      = "001"
       c-sistema      = return-value.

run pi-inicializar in h-acomp (input "Processando...").

CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = NO.
chExcelApplication:workbooks:ADD().
chExcelApplication:SHEETS(1):Select.

ASSIGN i-linha = 1.

ASSIGN chExcelApplication:range("A" + STRING(i-linha) ):value = "Usuario"
       chExcelApplication:range("B" + STRING(i-linha) ):value = "Nome"
       chExcelApplication:range("C" + STRING(i-linha) ):value = "Ativo"
       chExcelApplication:range("D" + STRING(i-linha) ):value = "OF Aut"
       chExcelApplication:range("E" + STRING(i-linha) ):value = "AP Auto"
       chExcelApplication:range("F" + STRING(i-linha) ):value = "Rec Var"
       chExcelApplication:range("G" + STRING(i-linha) ):value = "Bx Estoque"
       chExcelApplication:range("H" + STRING(i-linha) ):value = "AutoReabre PD"
       chExcelApplication:range("I" + STRING(i-linha) ):value = "Consiste Pre‡o Unit"
       chExcelApplication:range("J" + STRING(i-linha) ):value = "Atu Err"
       chExcelApplication:range("K" + STRING(i-linha) ):value = "Atu Adv"
       chExcelApplication:range("L" + STRING(i-linha) ):value = "Erros Dupl"
       chExcelApplication:range("M" + STRING(i-linha) ):value = "Atualiza UF"
       chExcelApplication:range("N" + STRING(i-linha) ):value = "Ped Out Fornec"
       chExcelApplication:range("O" + STRING(i-linha) ):value = "Recebe sem Pedido"
       chExcelApplication:range("P" + STRING(i-linha) ):value = "Variac EmissÆo"
       chExcelApplication:range("Q" + STRING(i-linha) ):value = "Var Atual"
       .

ASSIGN i-linha = i-linha + 1.

for each param-re no-lock where
         param-re.usuario >= tt-param.c-usu-ini and
         param-re.usuario <= tt-param.c-usu-fim:

    run pi-acompanhar in h-acomp (input "Usu rio : " + param-re.usuario).

    FIND FIRST usuar_mestre NO-LOCK
        WHERE usuar_mestre.cod_usuario = param-re.usuario NO-ERROR.
    IF AVAILABLE usuar_mestre THEN DO:
        IF TODAY >= usuar_mestre.dat_inic_valid AND
           TODAY <= usuar_mestre.dat_fim_valid THEN
            ASSIGN l-valido = YES.
        ELSE 
            ASSIGN l-valido = NO.
    END.
    ELSE
        ASSIGN l-valido = NO.

    ASSIGN chExcelApplication:range("A" + STRING(i-linha) ):value = param-re.usuario     
           chExcelApplication:range("B" + STRING(i-linha) ):value = param-re.nome        
           chExcelApplication:range("C" + STRING(i-linha) ):value = IF l-valido             THEN "SIM" ELSE "NAO"
           chExcelApplication:range("D" + STRING(i-linha) ):value = IF param-re.of-on-line  THEN "SIM" ELSE "NAO"
           chExcelApplication:range("E" + STRING(i-linha) ):value = IF param-re.ap-on-line  THEN "SIM" ELSE "NAO"
           chExcelApplication:range("F" + STRING(i-linha) ):value = IF param-re.aceita-var  THEN "SIM" ELSE "NAO"
           chExcelApplication:range("G" + STRING(i-linha) ):value = IF param-re.baixa-estoq THEN "SIM" ELSE "NAO"
           chExcelApplication:range("H" + STRING(i-linha) ):value = IF param-re.reabre-pd   THEN "SIM" ELSE "NAO"
           chExcelApplication:range("I" + STRING(i-linha) ):value = IF param-re.preco-unit  THEN "SIM" ELSE "NAO"
           chExcelApplication:range("J" + STRING(i-linha) ):value = IF param-re.atu-erro    THEN "SIM" ELSE "NAO"
           chExcelApplication:range("K" + STRING(i-linha) ):value = IF param-re.atu-advert  THEN "SIM" ELSE "NAO"
           chExcelApplication:range("L" + STRING(i-linha) ):value = IF param-re.erro-dupli  THEN "SIM" ELSE "NAO"
           chExcelApplication:range("M" + STRING(i-linha) ):value = IF param-re.alt-uf      THEN "SIM" ELSE "NAO"
           chExcelApplication:range("N" + STRING(i-linha) ):value = IF param-re.rec-out-for THEN "SIM" ELSE "NAO"
           chExcelApplication:range("O" + STRING(i-linha) ):value = IF param-re.sem-pedido  THEN "SIM" ELSE "NAO"
           chExcelApplication:range("P" + STRING(i-linha) ):value = param-re.var-emis
           chExcelApplication:range("Q" + STRING(i-linha) ):value = param-re.var-atual   
           .

    ASSIGN i-linha = i-linha + 1.

end.

run pi-finalizar in h-acomp.

chExcelApplication:Columns("A:Q"):EntireColumn:AutoFit.

CASE tt-param.destino:
    WHEN 1 THEN DO: /* Impressora */
        chExcelApplication:ActiveSheet:PrintOut.
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:QUIT().
    END.
    
    WHEN 2 THEN DO: /* Arquivo    */
        ASSIGN cArquivo = REPLACE (tt-param.arquivo,"/","\").
        ASSIGN cArquivo = REPLACE (cArquivo,"tmp","csv").
        ASSIGN cArquivo = REPLACE (cArquivo,"lst","csv").
        ASSIGN cArquivo = REPLACE (cArquivo,"txt","csv").
        ASSIGN cArquivo = REPLACE (cArquivo,"xls","csv").

        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:Workbooks:Item(1):SaveAs(cArquivo,6,,,,,).
        chExcelApplication:QUIT().
    END.
    WHEN 3 THEN DO: /* Terminal   */
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:Visible = TRUE.
    END.
END CASE.

RELEASE OBJECT chExcelApplication.

return "OK".


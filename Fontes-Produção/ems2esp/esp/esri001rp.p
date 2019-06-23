/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESRI001rp 2.06.00.001}  /*** 010010 ***/
 /*****************************************************************************
** 
**   Programa: ESRI001rp
** 
**   Data....: 07/11/2012
** 
**   Autor...: Bruno Bertulli (DSC)
**
**   Objetivo: 
**
******************************************************************************/

/*Defini‡Æo temp-tables tt-param*/

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

{esp\ESRI001.i}

DEFINE VARIABLE chExcelApplication AS COM-HANDLE    NO-UNDO.
DEFINE VARIABLE chWorkBook         AS COM-HANDLE    NO-UNDO.
DEFINE VARIABLE chWorkSheet        AS COM-HANDLE    NO-UNDO.

DEFINE VARIABLE i-linha  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iGrupo   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iImposto AS INTEGER     NO-UNDO.

def temp-table tt-raw-digita
field raw-digita as raw.    
   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.               

{include/i-rpvar.i}

find param-global no-lock no-error.          
          
assign c-programa = "ESRI001rp"
       c-versao   = "1.00"
       c-revisao  = "000"
       c-sistema  = "Especifico"
       c-empresa  = grupo.
 
/*
{include/i-rpcab.i}
{include/i-rpout.i}
*/

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (INPUT "Processando...").

/* ---> Cria objeto excel <--- */
CREATE "Excel.Application" chExcelApplication.

/* ---> Adiciona o modelo do documento <--- */
chWorkbook  = chExcelApplication:Workbooks:ADD().
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

ASSIGN i-linha = 1.

ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = "Estabelec".
ASSIGN chExcelApplication:Range("B" + STRING (i-linha)):VALUE = "Cod Identificador Bem".
ASSIGN chExcelApplication:Range("C" + STRING (i-linha)):VALUE = "Data Inicio Cr‚dito".
ASSIGN chExcelApplication:Range("D" + STRING (i-linha)):VALUE = "Data Entrada".
ASSIGN chExcelApplication:Range("E" + STRING (i-linha)):VALUE = "Quantidade".
ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = "Unidade".
ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = "Valor Cont bil".
ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = "Ficha".
ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = "S‚rie".
ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = "Nr Docto".
ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = "C¢digo Emitente".
ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = "Nat Opera‡Æo".
ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = "Sequˆncia".
ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = "Grupo 1".
ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = "Dt Grp 1".
ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = "Imp 1 Grp 1".
ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = "Imp 2 Grp 1".
ASSIGN chExcelApplication:Range("R" + STRING (i-linha)):VALUE = "Imp 3 Grp 1".
ASSIGN chExcelApplication:Range("S" + STRING (i-linha)):VALUE = "Grupo 2".
ASSIGN chExcelApplication:Range("T" + STRING (i-linha)):VALUE = "Dt Grp 2".
ASSIGN chExcelApplication:Range("U" + STRING (i-linha)):VALUE = "Imp 1 Grp 2".
ASSIGN chExcelApplication:Range("V" + STRING (i-linha)):VALUE = "Grupo 3".
ASSIGN chExcelApplication:Range("W" + STRING (i-linha)):VALUE = "Dt Grp 3".
ASSIGN chExcelApplication:Range("X" + STRING (i-linha)):VALUE = "Imp 1 Grp 3".

ASSIGN chExcelApplication:Range("A1:AZ1"):Font:FontStyle = "Negrito".

/*
ASSIGN chExcelApplication:Columns("A:D"):NumberFormat = "@".
ASSIGN chExcelApplication:Columns("E:M"):NumberFormat = "0,00".
ASSIGN chExcelApplication:Columns("A"):ColumnWidth = 7. 
*/

ASSIGN i-linha = i-linha + 1.

ASSIGN tt-param.cod-identific-bem-ini = ""
       tt-param.cod-identific-bem-fim = "ZZZZZZZZZZZZ"
       .

FOR EACH ri-bem NO-LOCK
    WHERE ri-bem.cod-identific-bem >= tt-param.cod-identific-bem-ini
    AND   ri-bem.cod-identific-bem <= tt-param.cod-identific-bem-fim
    AND   ri-bem.dat-entrada       >= tt-param.dat-entrada-ini
    AND   ri-bem.dat-entrada       <= tt-param.dat-entrada-fim
    :
    RUN pi-acompanhar IN h-acomp (INPUT "Bem " + ri-bem.cod-identific-bem).

    ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = ri-bem.cod-estabel.
    ASSIGN chExcelApplication:Range("B" + STRING (i-linha)):VALUE = ri-bem.cod-identific-bem.
    ASSIGN chExcelApplication:Range("C" + STRING (i-linha)):VALUE = STRING (ri-bem.dat-inic-cred,"99/99/9999").
    ASSIGN chExcelApplication:Range("D" + STRING (i-linha)):VALUE = STRING (ri-bem.dat-entrada,"99/99/9999").
    ASSIGN chExcelApplication:Range("E" + STRING (i-linha)):VALUE = ri-bem.quantidade.
    ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = ri-bem.un.
    ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = ri-bem.val-contabil.
    ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = (IF ri-bem.log-ficha = YES THEN "SIM" ELSE "NAO").
    ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = ri-bem.serie.
    ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = ri-bem.nr-nota-fis.
    ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = ri-bem.cod-emitente.
    ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = ri-bem.nat-operacao.
    ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = ri-bem.nr-seq-docto.

    ASSIGN iGrupo = 1.
    FOR EACH ri-bem-grupo OF ri-bem NO-LOCK:
        IF iGrupo = 1 THEN DO:
            FIND FIRST ri-grupos OF ri-bem-grupo NO-LOCK NO-ERROR.
            ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = STRING (ri-bem-grupo.cod-grupo) + "-" + ri-grupos.desc-grupo.
            ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = STRING (ri-bem-grupo.data-1,"99/99/9999").

            ASSIGN iImposto = 1.
            FOR EACH ri-valor-bem OF ri-bem-grupo NO-LOCK:
                IF iImposto = 1 THEN DO:
                    FIND FIRST ri-imposto OF ri-valor-bem NO-LOCK NO-ERROR.
                    ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = STRING (ri-imposto.cod-imp) + "-" + ri-imposto.desc-imposto.
                END.
                IF iImposto = 2 THEN DO:
                    FIND FIRST ri-imposto OF ri-valor-bem NO-LOCK NO-ERROR.
                    ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = STRING (ri-imposto.cod-imp) + "-" + ri-imposto.desc-imposto.
                END.
                IF iImposto = 3 THEN DO:
                    FIND FIRST ri-imposto OF ri-valor-bem NO-LOCK NO-ERROR.
                    ASSIGN chExcelApplication:Range("R" + STRING (i-linha)):VALUE = STRING (ri-imposto.cod-imp) + "-" + ri-imposto.desc-imposto. 
                END.

                ASSIGN iImposto = iImposto + 1.
            END.
        END.
        IF iGrupo = 2 THEN DO:
            FIND FIRST ri-grupos OF ri-bem-grupo NO-LOCK NO-ERROR.
            ASSIGN chExcelApplication:Range("S" + STRING (i-linha)):VALUE = STRING (ri-bem-grupo.cod-grupo) + "-" + ri-grupos.desc-grupo.
            ASSIGN chExcelApplication:Range("T" + STRING (i-linha)):VALUE = STRING (ri-bem-grupo.data-1,"99/99/9999").

            ASSIGN iImposto = 1.
            FOR EACH ri-valor-bem OF ri-bem-grupo NO-LOCK:
                IF iImposto = 1 THEN DO:
                    FIND FIRST ri-imposto OF ri-valor-bem NO-LOCK NO-ERROR.
                    ASSIGN chExcelApplication:Range("U" + STRING (i-linha)):VALUE = STRING (ri-imposto.cod-imp) + "-" + ri-imposto.desc-imposto.
                END.
                ASSIGN iImposto = iImposto + 1.
            END.
        END.
        IF iGrupo = 3 THEN DO:
            FIND FIRST ri-grupos OF ri-bem-grupo NO-LOCK NO-ERROR.
            ASSIGN chExcelApplication:Range("V" + STRING (i-linha)):VALUE = STRING (ri-bem-grupo.cod-grupo) + "-" + ri-grupos.desc-grupo.
            ASSIGN chExcelApplication:Range("W" + STRING (i-linha)):VALUE = STRING (ri-bem-grupo.data-1,"99/99/9999").

            ASSIGN iImposto = 1.
            FOR EACH ri-valor-bem OF ri-bem-grupo NO-LOCK:
                IF iImposto = 1 THEN DO:
                    FIND FIRST ri-imposto OF ri-valor-bem NO-LOCK NO-ERROR.
                    ASSIGN chExcelApplication:Range("X" + STRING (i-linha)):VALUE = STRING (ri-imposto.cod-imp) + "-" + ri-imposto.desc-imposto.
                END.
                ASSIGN iImposto = iImposto + 1.
            END.
        END.
        ASSIGN iGrupo = iGrupo + 1.
    END.

    ASSIGN i-linha = i-linha + 1.
END.

run pi-finalizar in h-acomp. 

chWorkSheet:Columns("A:AZ"):EntireColumn:AutoFit.

chExcelApplication:VISIBLE = TRUE.

RELEASE object chExcelApplication.      
RELEASE object chWorkbook.
RELEASE object chWorksheet.


/*
{include/i-rpclo.i}
*/

return "OK":U.


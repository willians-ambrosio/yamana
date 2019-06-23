/*****************************************************************************
**  Programa: esra-load-003.p
**     Autor: 
**      Data: 
** Descricao: Gerar Excel para validacao
** Alteracao: 
******************************************************************************/

{dsc/ra/include/esra-load-001.i}

DEFINE INPUT PARAMETER TABLE FOR tt-planilha. 



DEFINE VARIABLE excelApp  AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i-linha   AS INTEGER    NO-UNDO.


create "excel.application" excelapp.

excelapp:visible = TRUE.
excelapp:workbooks:add().
excelapp:worksheets:item(1):select.


excelapp:Range("A:A"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("D:D"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("E:E"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("G:G"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("H:H"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("I:I"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("O:O"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("Q:Q"):SELECT().
excelapp:SELECTION:NumberFormat = "@".

excelapp:Range("R:R"):SELECT().
excelapp:SELECTION:NumberFormat = "@".



excelapp:range("A1"):value = "XML".
excelapp:range("K1"):value = "DATASUL".
excelapp:range("T1"):value = "STATUS".



excelapp:Range("A1:J1"):SELECT().
excelapp:Selection:MergeCells          = TRUE.
excelapp:Selection:HorizontalAlignment = 3.

RUN pi-borda-grossa.
 
excelapp:Range("K1:S1"):SELECT().
excelapp:Selection:MergeCells          = TRUE.
excelapp:Selection:HorizontalAlignment = 3.
RUN pi-borda-grossa.

excelapp:Range("T1:W1"):SELECT().
excelapp:Selection:MergeCells          = TRUE.
excelapp:Selection:HorizontalAlignment = 3.
RUN pi-borda-grossa.



/*XML*/
excelapp:range("A2"):value = "Chave de Acesso".
excelapp:range("B2"):value = "Serie".
excelapp:range("C2"):value = "nNF".
excelapp:range("D2"):value = "CNPJ Emit".
excelapp:range("E2"):value = "xNome".
excelapp:range("F2"):value = "nItem".
excelapp:range("G2"):value = "cProd".
excelapp:range("H2"):value = "xProd".
excelapp:range("I2"):value = "CFOP".
excelapp:range("J2"):value = "vProd".

/*Datasul*/
excelapp:range("K2"):value = "Estab".
excelapp:range("L2"):value = "Serie".
excelapp:range("M2"):value = "Nota".
excelapp:range("N2"):value = "Cod Emit".
excelapp:range("O2"):value = "Nome Abrev".
excelapp:range("P2"):value = "Seq".
excelapp:range("Q2"):value = "Item".
excelapp:range("R2"):value = "Natureza".
excelapp:range("S2"):value = "Preco Total".

/**Status*/
excelapp:range("T2"):value = "Sit Sequencia".
excelapp:range("U2"):value = "Sit Preco".
excelapp:range("V2"):value = "Variaca Preco".
excelapp:range("W2"):value = "Yamana".

/* excelapp:Range("A2:W30"):SELECT(). */
/* RUN pi-borda-fina.                 */

excelapp:Range("A2:J2"):SELECT().
RUN pi-borda-grossa.
 
excelapp:Range("K2:S2"):SELECT().
RUN pi-borda-grossa.

excelapp:Range("T2:W2"):SELECT().
RUN pi-borda-grossa.






ASSIGN i-linha = 3.

FOR EACH tt-planilha:
    
    /*XML*/

    
    
    excelapp:range("A" + STRING(i-linha)):VALUE = tt-planilha.chave-acesso.
    excelapp:range("B" + STRING(i-linha)):VALUE = tt-planilha.ide-serie.
    excelapp:range("C" + STRING(i-linha)):VALUE = tt-planilha.ide-nnf.
    excelapp:range("D" + STRING(i-linha)):VALUE = tt-planilha.cnpj.
    excelapp:range("E" + STRING(i-linha)):VALUE = tt-planilha.xnome.
    excelapp:range("F" + STRING(i-linha)):VALUE = tt-planilha.det-nitem.
    excelapp:range("G" + STRING(i-linha)):VALUE = tt-planilha.det-cprod.
    excelapp:range("H" + STRING(i-linha)):VALUE = tt-planilha.det-xprod.
    excelapp:range("I" + STRING(i-linha)):VALUE = tt-planilha.det-cfop.
    excelapp:range("J" + STRING(i-linha)):VALUE = tt-planilha.det-vprod.

    /*Datasul*/
    excelapp:range("K" + STRING(i-linha)):VALUE = tt-planilha.cod-estabel.
    excelapp:range("L" + STRING(i-linha)):VALUE = tt-planilha.serie.
    excelapp:range("M" + STRING(i-linha)):VALUE = tt-planilha.nr-docto.
    excelapp:range("N" + STRING(i-linha)):VALUE = tt-planilha.cod-emitente.
    excelapp:range("O" + STRING(i-linha)):VALUE = tt-planilha.nome-abrev.
    excelapp:range("P" + STRING(i-linha)):VALUE = tt-planilha.seq.
    excelapp:range("Q" + STRING(i-linha)):VALUE = tt-planilha.it-codigo.
    excelapp:range("R" + STRING(i-linha)):VALUE = tt-planilha.nat-operacao.
    excelapp:range("S" + STRING(i-linha)):VALUE = tt-planilha.preco-tot.

    /*Status*/
    excelapp:range("T" + STRING(i-linha)):VALUE = tt-planilha.sit-seq.
    excelapp:range("U" + STRING(i-linha)):VALUE = tt-planilha.sit-preco.
    excelapp:range("V" + STRING(i-linha)):VALUE = tt-planilha.var-preco.

    ASSIGN i-linha = i-linha + 1.



END.










PROCEDURE pi-borda-fina:
    excelapp:Selection:Borders(1):LineStyle  = 1. 
    excelapp:Selection:Borders(1):Weight     = 2. 
    excelapp:Selection:Borders(3):LineStyle  = 1. 
    excelapp:Selection:Borders(3):Weight     = 2. 
    excelapp:Selection:Borders(4):LineStyle  = 1. 
    excelapp:Selection:Borders(4):Weight     = 2. 
    excelapp:Selection:Borders(7):LineStyle  = 1.    
    excelapp:Selection:Borders(7):Weight     = 3.
    excelapp:Selection:Borders(8):LineStyle  = 1.    
    excelapp:Selection:Borders(8):Weight     = 3.
    excelapp:Selection:Borders(9):LineStyle  = 1.    
    excelapp:Selection:Borders(9):Weight     = 3.
    excelapp:Selection:Borders(10):LineStyle = 1.    
    excelapp:Selection:Borders(10):Weight    = 3.


END PROCEDURE.


PROCEDURE pi-borda-grossa:

    excelapp:Selection:Borders(1):LineStyle  = 1. 
    excelapp:Selection:Borders(1):Weight     = 2. 
    excelapp:Selection:Borders(3):LineStyle  = 1. 
    excelapp:Selection:Borders(3):Weight     = 2. 
    excelapp:Selection:Borders(4):LineStyle  = 1. 
    excelapp:Selection:Borders(4):Weight     = 2. 
    excelapp:Selection:Borders(7):LineStyle  = 1.    
    excelapp:Selection:Borders(7):Weight     = 3.
    excelapp:Selection:Borders(8):LineStyle  = 1.    
    excelapp:Selection:Borders(8):Weight     = 3.
    excelapp:Selection:Borders(9):LineStyle  = 1.    
    excelapp:Selection:Borders(9):Weight     = 3.
    excelapp:Selection:Borders(10):LineStyle = 1.    
    excelapp:Selection:Borders(10):Weight    = 3.


END PROCEDURE.




/*****************************************************************************
******************************************************************************
**       PROGRAMA: escc007rp
**       DATA....: Abril/2012
**       OBJETIVO: Listagem Itens x Fornecedores
**       EMPRESA:  CSX Solution - Thiago Coutinho
******************************************************************************
******************************************************************************/
def temp-table tt-raw-digita no-undo
    field raw-digita as raw.

/* recebimento de parƒmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/* defini‡Æo das temp-tables para recebimento de parƒmetros */
define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    FIELD c-cod-estabel-ini     LIKE estabelec.cod-estabel
    FIELD c-cod-estabel-fim     LIKE estabelec.cod-estabel
    FIELD d-data-emis-ini       LIKE ordem-compra.data-emissao
    FIELD d-data-emis-fim       LIKE ordem-compra.data-emissao
    FIELD c-it-codigo-ini       LIKE ITEM.it-codigo
    FIELD c-it-codigo-fim       LIKE ITEM.it-codigo
    FIELD c-ge-codigo-ini       LIKE ITEM.ge-codigo
    FIELD c-ge-codigo-fim       LIKE ITEM.ge-codigo
    FIELD i-cod-emitente-ini    LIKE emitente.cod-emitente
    FIELD i-cod-emitente-fim    LIKE emitente.cod-emitente
    .

DEFINE VARIABLE h-acomp      AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-line-count AS INTEGER INIT 4  NO-UNDO.
DEFINE VARIABLE c-arq-excel  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE chExcel2     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWBook2     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWSheet2    AS COM-HANDLE      NO-UNDO.

DEF VAR chworkbook AS COM-HANDLE NO-UNDO.
DEF VAR c-caminho AS CHAR NO-UNDO.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i "Itens_x_Fornecedores.:" *}
run pi-inicializar in h-acomp (input return-value).

RUN criaPlanilha.
RUN setColumnFormat.
RUN printColumnLabel.
RUN printDados.
RUN finalizaPlanilha.
   
run pi-finalizar in h-acomp.

/******************************** PROCEDURES  EXCEL *******************************************/
PROCEDURE printDados:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Imprime os dados na planilha excel
------------------------------------------------------------------------------*/

IF CAN-FIND(FIRST tt-param) THEN DO:
    
    FOR EACH ordem-compra NO-LOCK
       WHERE ordem-compra.cod-estabel  >= tt-param.c-cod-estabel-ini
         AND ordem-compra.cod-estabel  <= tt-param.c-cod-estabel-fim
         AND ordem-compra.data-emissao >= tt-param.d-data-emis-ini
         AND ordem-compra.data-emissao <= tt-param.d-data-emis-fim
         AND ordem-compra.it-codigo    >= tt-param.c-it-codigo-ini
         AND ordem-compra.it-codigo    <= tt-param.c-it-codigo-fim
         AND ordem-compra.cod-emitente >= tt-param.i-cod-emitente-ini
         AND ordem-compra.cod-emitente <= tt-param.i-cod-emitente-fim :

        FIND FIRST emitente
             WHERE emitente.cod-emitente = ordem-compra.cod-emitente
             NO-LOCK NO-ERROR.

        FIND FIRST ITEM
             WHERE ITEM.it-codigo = ordem-compra.it-codigo
             NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN DO:

            IF ITEM.ge-codigo >= tt-param.c-ge-codigo-ini AND
               ITEM.ge-codigo <= tt-param.c-ge-codigo-fim THEN DO:

                RUN pi-acompanhar in h-acomp (input "Ordem..:" + STRING(ordem-compra.numero-ordem)).

                chWSheet2:Range("C1:E1"):select.
                chExcel2:SELECTION:Merge.

                ASSIGN
                    chWSheet2:Range("C1:E1"):VALUE = String("Listagem Itens x Fornecedores - ") + string(TODAY, "99/99/9999")
                    chWSheet2:Range("C1:E1"):FONT:Bold = TRUE
                    chWSheet2:Range("C1:E1"):FONT:SIZE = 14.

                ASSIGN
                    chWSheet2:Range("A" + string(i-line-count)):VALUE = STRING(ordem-compra.cod-emitente)
                    chWSheet2:Range("B" + string(i-line-count)):VALUE = IF AVAIL emitente THEN emitente.nome-abrev ELSE ""
                    chWSheet2:Range("C" + string(i-line-count)):VALUE = ITEM.it-codigo
                    chWSheet2:Range("D" + string(i-line-count)):VALUE = ITEM.desc-item
                    chWSheet2:Range("E" + string(i-line-count)):VALUE = ITEM.ge-codigo
                    chWSheet2:Range("F" + string(i-line-count)):VALUE = ordem-compra.numero-ordem
                    chWSheet2:Range("G" + string(i-line-count)):VALUE = ordem-compra.num-pedido
                    chWSheet2:Range("H" + STRING(i-line-count)):HorizontalAlignment = -4152 
                    chWSheet2:Range("H" + string(i-line-count)):VALUE = STRING(ordem-compra.data-emissao, "99/99/9999")
                    chWSheet2:Range("I" + string(i-line-count)):VALUE = ordem-compra.cod-estabel
                    .

                ASSIGN
                    i-line-count = i-line-count + 1.

            END.       
        END.
    END. /** FOR EACH ordem-compra NO-LOCK **/

END.

END PROCEDURE.

PROCEDURE criaPlanilha:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Cria‡Æo da Planilha Excel
------------------------------------------------------------------------------*/

    CREATE 'Excel.Application' chExcel2.
    chWBook2 = chExcel2:Workbooks:Add().
    chWSheet2 = chWBook2:Sheets:Item(1).

END PROCEDURE. /*- criaPlanilha -*/


PROCEDURE setColumnFormat:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Defini‡Æo das larguras das colunas do aquivo
------------------------------------------------------------------------------*/
    
    ASSIGN  chExcel2:COLUMNS("A:A"):ColumnWidth = 10.00
            chExcel2:COLUMNS("B:B"):ColumnWidth = 20.00
            chExcel2:COLUMNS("C:C"):ColumnWidth = 20.00
            chExcel2:COLUMNS("D:D"):ColumnWidth = 20.00
            chExcel2:COLUMNS("E:E"):ColumnWidth = 10.00
            chExcel2:COLUMNS("F:F"):ColumnWidth = 20.00
            chExcel2:COLUMNS("G:G"):ColumnWidth = 10.00
            chExcel2:COLUMNS("H:H"):ColumnWidth = 10.00
            chExcel2:COLUMNS("I:I"):ColumnWidth = 10.00.
    
 END PROCEDURE. /*- setColumnFormat -*/ 

 PROCEDURE printColumnLabel: 
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Gera label e cor do cabe‡alho das colunas dos Projetos de Investimento
------------------------------------------------------------------------------*/

  /*Linha 1*/
    ASSIGN 
         chWSheet2:Range("A3") = "Codigo Fornecedor"
         chWSheet2:Range("B3") = "Nome Abreviado"
         chWSheet2:Range("C3") = "Item"
         chWSheet2:Range("D3") = "Descricao Item"
         chWSheet2:Range("E3") = "Grupo Estq"
         chWSheet2:Range("F3") = "Ordem Compra"
         chWSheet2:Range("G3") = "Pedido"
         chWSheet2:Range("H3") = "Data Emissao"
         chWSheet2:Range("I3") = "Estabelecimento"
         .
    
         chWSheet2:Range("A3:I3"):SELECT.
         chWSheet2:Range("A3:I3"):FONT:Bold = TRUE.
         chWSheet2:Range("A3:I3"):FONT:SIZE = 12.


END PROCEDURE. /*- printColumnLabel -*/

PROCEDURE pi-gera-borda-browser:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Gera borda para o cabecalho 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAM c-selecao AS CHAR NO-UNDO.

    chWSheet2:Range(c-selecao):SELECT. 
    ASSIGN chExcel2:Selection:Borders(5):LineStyle   = -4142 
           chExcel2:Selection:Borders(6):LineStyle   = -4142
           chExcel2:Selection:Borders(7):LineStyle   = 1
           chExcel2:Selection:Borders(7):Weight      = 2
           chExcel2:Selection:Borders(7):ColorIndex  = -4105
           chExcel2:Selection:Borders(8):LineStyle   = 1
           chExcel2:Selection:Borders(8):Weight      = 2
           chExcel2:Selection:Borders(8):ColorIndex  = -4105
           chExcel2:Selection:Borders(9):LineStyle   = 1
           chExcel2:Selection:Borders(9):Weight      = 2
           chExcel2:Selection:Borders(9):ColorIndex  = -4105
           chExcel2:Selection:Borders(10):LineStyle  = 1
           chExcel2:Selection:Borders(10):Weight     = 2
           chExcel2:Selection:Borders(10):ColorIndex = -4105
           chExcel2:Selection:Borders(11):LineStyle  = 1
           chExcel2:Selection:Borders(12):LineStyle  = -4142.

END PROCEDURE. /*- pi-gera-borda-browser -*/

PROCEDURE finalizaPlanilha:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Finaliza Planilha Excel
------------------------------------------------------------------------------*/

    ASSIGN c-caminho = session:TEMP-DIRECTORY + 'ESCC007_' + STRING(TIME) + ".xls".

    /* ajusta o tamanho das colunas */
    chWSheet2:Range("A:I"):EntireColumn:AutoFit.

    chExcel2:VISIBLE = YES.

    if search(c-caminho) <> ? then
      os-delete value(c-caminho).

    chExcel2:ActiveWorkbook:SaveAs(c-caminho,,,,,,).

/*     chExcel2:ActiveWorkbook:CLOSE.   */
/*     chExcel2:APPLICATION:QUIT.       */
/*     chExcel2:QUIT().                 */

    IF VALID-HANDLE(chExcel2) THEN
        RELEASE OBJECT chExcel2.

    IF VALID-HANDLE(chWBook2) THEN
        RELEASE OBJECT chWBook2.

    IF VALID-HANDLE(chWSheet2) THEN
        RELEASE OBJECT chWSheet2.
    
END PROCEDURE.

 /*- finalizaPlanilha -*/



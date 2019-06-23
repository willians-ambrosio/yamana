DEFINE INPUT  PARAMETER ip-arquivo-entrada AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ip-arquivo-saida   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-ccin NO-UNDO
       FIELDS sequencia AS INTEGER
       FIELDS coluna-a  AS CHARACTER     FORMAT "X(0004)" /*   | Caracter  */
       FIELDS coluna-b  AS CHARACTER     FORMAT "X(0001)" /*   | Caracter  */
    .

DEFINE TEMP-TABLE tt-cc00 NO-UNDO
       FIELDS sequencia AS INTEGER
       FIELDS coluna-a  AS CHARACTER     FORMAT "X(0004)" /*   | Caracter  */
       FIELDS coluna-b  AS CHARACTER     FORMAT "X(0016)" /*   | Caracter  */
       FIELDS coluna-c  AS CHARACTER     FORMAT "X(0001)" /*   | Logico    */
       FIELDS coluna-d  AS CHARACTER     FORMAT "X(0002)" /*   | Inteiro   */
       FIELDS coluna-e  AS CHARACTER     FORMAT "X(0001)" /*   | Inteiro   */
       FIELDS coluna-f  AS CHARACTER     FORMAT "X(0012)" /*   | Caracter  */
       FIELDS coluna-g  AS CHARACTER     FORMAT "X(0001)" /*   | Inteiro   */
       FIELDS coluna-h  AS CHARACTER     FORMAT "X(0005)" /*   | Decimal   */
       FIELDS coluna-i  AS CHARACTER     FORMAT "X(0005)" /*   | Decimal   */
       FIELDS coluna-j  AS CHARACTER     FORMAT "X(0003)" /*   | Inteiro   */
       FIELDS coluna-k  AS CHARACTER     FORMAT "X(0003)" /*   | Inteiro   */
       FIELDS coluna-l  AS CHARACTER     FORMAT "X(0005)" /*   | Inteiro   */
       FIELDS coluna-m  AS CHARACTER     FORMAT "X(0001)" /*   | Inteiro   */
       FIELDS coluna-n  AS CHARACTER     FORMAT "X(0013)" /*   | Decimal   */
       FIELDS coluna-o  AS CHARACTER     FORMAT "X(0012)" /*   | Caracter  */
       FIELDS coluna-p  AS CHARACTER     FORMAT "X(0001)" /*   | Inteiro   */
       FIELDS coluna-q  AS CHARACTER     FORMAT "X(0015)" /*   | Decimal   */
       FIELDS coluna-r  AS CHARACTER     FORMAT "X(0015)" /*   | Decimal   */
       FIELDS coluna-s  AS CHARACTER     FORMAT "X(0012)" /*   | Decimal   */
       FIELDS coluna-t  AS CHARACTER     FORMAT "X(0015)" /*   | Decimal   */
       FIELDS coluna-u  AS CHARACTER     FORMAT "X(0013)" /*   | Decimal   */
       FIELDS coluna-v  AS CHARACTER     FORMAT "X(0015)" /*   | Decimal   */
       FIELDS coluna-w  AS CHARACTER     FORMAT "X(0013)" /*   | Decimal   */
       FIELDS coluna-x  AS CHARACTER     FORMAT "X(0013)" /*   | Decimal   */
       FIELDS coluna-y  AS CHARACTER     FORMAT "X(0008)" /*   | Data      */
       FIELDS coluna-z  AS CHARACTER     FORMAT "X(0008)" /*   | Data      */
       FIELDS coluna-aa AS CHARACTER     FORMAT "X(0008)" /*   | Data      */
       FIELDS coluna-ab AS CHARACTER     FORMAT "X(0009)" /*   | Inteiro   */
       FIELDS coluna-ac AS CHARACTER     FORMAT "X(0005)" /*   | Caracter  */
       FIELDS coluna-ad AS CHARACTER     FORMAT "X(0005)" /*   | Caracter  */
       FIELDS coluna-ae AS CHARACTER     FORMAT "X(0005)" /*   | Caracter  */
       FIELDS coluna-af AS CHARACTER     FORMAT "X(0005)" /*   | Caracter  */
       FIELDS coluna-ag AS CHARACTER     FORMAT "X(0013)" /*   | Caracter  */
    .

DEFINE TEMP-TABLE tt-cc01 NO-UNDO
       FIELDS sequencia AS INTEGER      
       FIELDS coluna-a  AS CHARACTER     FORMAT "X(0004)"  /*  | Caracter  */
       FIELDS coluna-b  AS CHARACTER     FORMAT "X(0016)"  /*  | Caracter  */
       FIELDS coluna-c  AS CHARACTER     FORMAT "X(0032)"  /*  | Caracter  */
       FIELDS coluna-d  AS CHARACTER     FORMAT "X(0013)"  /*  | Decimal   */
       FIELDS coluna-e  AS CHARACTER     FORMAT "X(0002)"  /*  | Inteiro   */
       FIELDS coluna-f  AS CHARACTER     FORMAT "X(0001)"  /*  | L¢gico    */
       FIELDS coluna-g  AS CHARACTER     FORMAT "X(0002)"  /*  | Inteiro   */
       FIELDS coluna-h  AS CHARACTER     FORMAT "X(0012)"  /*  | Caracter  */
       FIELDS coluna-i  AS CHARACTER     FORMAT "X(0001)"  /*  | Inteiro   */
       FIELDS coluna-j  AS CHARACTER     FORMAT "X(0015)"  /*  | Decimal   */
       FIELDS coluna-k  AS CHARACTER     FORMAT "X(0015)"  /*  | Decimal   */
       FIELDS coluna-l  AS CHARACTER     FORMAT "X(0013)"  /*  | Decimal   */
       FIELDS coluna-m  AS CHARACTER     FORMAT "X(0013)"  /*  | Decimal   */
       FIELDS coluna-n  AS CHARACTER     FORMAT "X(0012)"  /*  | Caracter  */
       FIELDS coluna-o  AS CHARACTER     FORMAT "X(0003)"  /*  | Inteiro   */
       FIELDS coluna-p  AS CHARACTER     FORMAT "X(0008)"  /*  | Data      */
       FIELDS coluna-q  AS CHARACTER     FORMAT "X(0040)"  /*  | Caracter  */
       FIELDS coluna-r  AS CHARACTER     FORMAT "X(2000)"  /*  | Caracter  */
       FIELDS coluna-s  AS CHARACTER     FORMAT "X(0009)"  /*  | Inteiro   */
       FIELDS coluna-t  AS CHARACTER     FORMAT "X(0004)"  /*  | Decimal   */
       FIELDS coluna-u  AS CHARACTER     FORMAT "X(0040)"  /*  | Caracter  */
    .

DEFINE TEMP-TABLE tt-pd00 NO-UNDO
       FIELDS sequencia AS INTEGER
       FIELDS coluna-a  AS CHARACTER     FORMAT "X(0004)"  /*   | Caracter */ 
       FIELDS coluna-b  AS CHARACTER     FORMAT "X(0005)"  /*   | Caracter */ 
       FIELDS coluna-c  AS CHARACTER     FORMAT "X(0005)"  /*   | Caracter */ 
       FIELDS coluna-d  AS CHARACTER     FORMAT "X(0004)"  /*   | Inteiro  */ 
       FIELDS coluna-e  AS CHARACTER     FORMAT "X(0009)"  /*   | Inteiro  */ 
    .

DEFINE TEMP-TABLE tt-ic00 NO-UNDO
       FIELDS sequencia AS INTEGER
       FIELDS coluna-a  AS CHARACTER     FORMAT "X(0004)"  /*  | Caracter  */
       FIELDS coluna-b  AS CHARACTER     FORMAT "X(0016)"  /*  | Caracter  */
       FIELDS coluna-c  AS CHARACTER     FORMAT "X(0011)"  /*  | Decimal   */
       FIELDS coluna-d  AS CHARACTER     FORMAT "X(0011)"  /*  | Decimal   */
       FIELDS coluna-e  AS CHARACTER     FORMAT "X(0011)"  /*  | Decimal   */
       FIELDS coluna-f  AS CHARACTER     FORMAT "X(0012)"  /*  | Decimal   */
       FIELDS coluna-g  AS CHARACTER     FORMAT "X(0002)"  /*  | Inteiro   */
       FIELDS coluna-h  AS CHARACTER     FORMAT "X(0001)"  /*  | Logico    */
       FIELDS coluna-i  AS CHARACTER     FORMAT "X(0016)"  /*  | Caracter  */
       FIELDS coluna-j  AS CHARACTER     FORMAT "X(0013)"  /*  | Decimal   */
       FIELDS coluna-k  AS CHARACTER     FORMAT "X(0008)"  /*  | Caracter  */
       FIELDS coluna-l  AS CHARACTER     FORMAT "X(0001)"  /*  | Logico    */
       FIELDS coluna-m  AS CHARACTER     FORMAT "X(0001)"  /*  | Inteiro   */
       FIELDS coluna-n  AS CHARACTER     FORMAT "X(0002)"  /*  | Caracter  */
       FIELDS coluna-o  AS CHARACTER     FORMAT "X(0012)"  /*  | Caracter  */
       FIELDS coluna-p  AS CHARACTER     FORMAT "X(0004)"  /*  | Inteiro   */
       FIELDS coluna-q  AS CHARACTER     FORMAT "X(0003)"  /*  | Inteiro   */
       FIELDS coluna-r  AS CHARACTER     FORMAT "X(0001)"  /*  | Inteiro   */
       FIELDS coluna-s  AS CHARACTER     FORMAT "X(0015)"  /*  | Decimal   */
       FIELDS coluna-t  AS CHARACTER     FORMAT "X(0001)"  /*  | Inteiro   */
       FIELDS coluna-u  AS CHARACTER     FORMAT "X(0013)"  /*  | Decimal   */
       FIELDS coluna-v  AS CHARACTER     FORMAT "X(0015)"  /*  | Decimal   */
       FIELDS coluna-w  AS CHARACTER     FORMAT "X(0015)"  /*  | Decimal   */
       FIELDS coluna-x  AS CHARACTER     FORMAT "X(0001)"  /*  | Inteiro   */
       FIELDS coluna-y  AS CHARACTER     FORMAT "X(0015)"  /*  | Decimal   */
       FIELDS coluna-z  AS CHARACTER     FORMAT "X(0015)"  /*  | Decimal   */
       FIELDS coluna-aa AS CHARACTER     FORMAT "X(0001)"  /*  | Logico    */
       FIELDS coluna-ab AS CHARACTER     FORMAT "X(0001)"  /*  | Inteiro   */
       FIELDS coluna-ac AS CHARACTER     FORMAT "X(0001)"  /*  | Logico    */
       FIELDS coluna-ad AS CHARACTER     FORMAT "X(0001)"  /*  | Logico    */
       FIELDS coluna-ae AS CHARACTER     FORMAT "X(0005)"  /*  | Decimal   */
       FIELDS coluna-af AS CHARACTER     FORMAT "X(0005)"  /*  | Decimal   */
       FIELDS coluna-ag AS CHARACTER     FORMAT "X(0003)"  /*  | Caracter  */
       FIELDS coluna-ah AS CHARACTER     FORMAT "X(0005)"  /*  | Decimal   */
       FIELDS coluna-ai AS CHARACTER     FORMAT "X(0005)"  /*  | Decimal   */
       FIELDS coluna-aj AS CHARACTER     FORMAT "X(0005)"  /*  | Decimal   */
       FIELDS coluna-ak AS CHARACTER     FORMAT "X(0003)"  /*  | Inteiro   */
       FIELDS coluna-al AS CHARACTER     FORMAT "X(0003)"  /*  | Inteiro   */
       FIELDS coluna-am AS CHARACTER     FORMAT "X(0001)"  /*  | Logico    */
       FIELDS coluna-an AS CHARACTER     FORMAT "X(0009)"  /*  | Inteiro   */
    .
                                              
DEFINE TEMP-TABLE tt-ic01 NO-UNDO
       FIELDS sequencia AS INTEGER                
       FIELDS coluna-a  AS CHARACTER     FORMAT "X(0004)"  /*  | Caracter  */
       FIELDS coluna-b  AS CHARACTER     FORMAT "X(0016)"  /*  | Caracter  */
       FIELDS coluna-c  AS CHARACTER     FORMAT "X(0004)"  /*  | Inteiro   */
       FIELDS coluna-d  AS CHARACTER     FORMAT "X(0016)"  /*  | Decimal   */
       FIELDS coluna-e  AS CHARACTER     FORMAT "X(0001)"  /*  | Logico    */
       FIELDS coluna-f  AS CHARACTER     FORMAT "X(0013)"  /*  | Decimal   */
       FIELDS coluna-g  AS CHARACTER     FORMAT "X(0007)"  /*  | Decimal   */
       FIELDS coluna-h  AS CHARACTER     FORMAT "X(0004)"  /*  | Inteiro   */
       FIELDS coluna-i  AS CHARACTER     FORMAT "X(0008)"  /*  | Data      */
       FIELDS coluna-j  AS CHARACTER     FORMAT "X(0016)"  /*  | Decimal   */
       FIELDS coluna-k  AS CHARACTER     FORMAT "X(0012)"  /*  | Caracter  */
       FIELDS coluna-l  AS CHARACTER     FORMAT "X(0005)"  /*  | Decimal   */
       FIELDS coluna-m  AS CHARACTER     FORMAT "X(0076)"  /*  | Caracter  */
       FIELDS coluna-n  AS CHARACTER     FORMAT "X(0001)"  /*  | Inteiro   */
       FIELDS coluna-o  AS CHARACTER     FORMAT "X(0016)"  /*  | Decimal   */
       FIELDS coluna-p  AS CHARACTER     FORMAT "X(0008)"  /*  | Data      */
       FIELDS coluna-q  AS CHARACTER     FORMAT "X(0012)"  /*  | Decimal   */
       FIELDS coluna-r  AS CHARACTER     FORMAT "X(0012)"  /*  | Decimal   */
       FIELDS coluna-s  AS CHARACTER     FORMAT "X(0008)"  /*  | Inteiro   */
       FIELDS coluna-t  AS CHARACTER     FORMAT "X(2000)"  /*  | Caracter  */
       FIELDS coluna-u  AS CHARACTER     FORMAT "X(0003)"  /*  | Inteiro   */
       FIELDS coluna-v  AS CHARACTER     FORMAT "X(0009)"  /*  | Inteiro   */
    .

DEFINE TEMP-TABLE tt-mc00 NO-UNDO
       FIELDS sequencia AS INTEGER
       FIELDS coluna-a  AS CHARACTER     FORMAT "X(0004)"  /*   | Caracter  */
       FIELDS coluna-b  AS CHARACTER     FORMAT "X(0016)"  /*   | Caracter  */
       FIELDS coluna-c  AS CHARACTER     FORMAT "X(0020)"  /*   | Caracter  */
       FIELDS coluna-d  AS CHARACTER     FORMAT "X(0020)"  /*   | Caracter  */
       FIELDS coluna-e  AS CHARACTER     FORMAT "X(0005)"  /*   | Decimal   */
       FIELDS coluna-f  AS CHARACTER     FORMAT "X(0003)"  /*   | Caracter  */
    .

DEFINE TEMP-TABLE tt-mi00 NO-UNDO
       FIELDS sequencia AS INTEGER
       FIELDS coluna-a  AS CHARACTER    FORMAT "X(0004)"  /*   | Caracter  */
       FIELDS coluna-b  AS CHARACTER    FORMAT "X(0016)"  /*   | Caracter  */
       FIELDS coluna-c  AS CHARACTER    FORMAT "X(0020)"  /*   | Caracter  */
       FIELDS coluna-d  AS CHARACTER    FORMAT "X(0020)"  /*   | Caracter  */
       FIELDS coluna-e  AS CHARACTER    FORMAT "X(0005)"  /*   | Decimal   */
       FIELDS coluna-f  AS CHARACTER    FORMAT "X(0004)"  /*   | Inteiro   */
       FIELDS coluna-g  AS CHARACTER    FORMAT "X(0016)"  /*   | Caracter  */
       FIELDS coluna-h  AS CHARACTER    FORMAT "X(0003)"  /*   | Caracter  */
    .

DEFINE TEMP-TABLE tt-ac00 NO-UNDO
       FIELDS sequencia AS INTEGER
       FIELDS coluna-a  AS CHARACTER    FORMAT "X(0004)"  /*   | Caracter  */
       FIELDS coluna-b  AS CHARACTER    FORMAT "X(0002)"  /*   | Inteiro   */
       FIELDS coluna-c  AS CHARACTER    FORMAT "X(0012)"  /*   | Caracter  */
       FIELDS coluna-d  AS CHARACTER    FORMAT "X(0009)"  /*   | Inteiro   */
       FIELDS coluna-e  AS CHARACTER    FORMAT "X(0032)"  /*   | Caracter  */
       FIELDS coluna-f  AS CHARACTER    FORMAT "X(2000)"  /*   | Caracter  */
    .

DEFINE VARIABLE chExcelApp      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook      AS COM-HANDLE NO-UNDO. /* Cria objeto para referenciar a pasta de trabalho */
DEFINE VARIABLE chWorkSheet     AS COM-HANDLE NO-UNDO. /* Cria objeto para referenciar a planilha na pasta de trabalho */
DEFINE VARIABLE i-cont          AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-item          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-valor         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-estab-entrega AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-acao          AS CHARACTER  NO-UNDO.

FUNCTION fcTrataCampoInteiro RETURNS CHARACTER (INPUT ip-campo AS CHARACTER):
   IF ip-campo = "" OR ip-campo = ? THEN
      RETURN "".
   ELSE
      RETURN STRING(INTEGER(ip-campo)).
END FUNCTION.

FUNCTION fcTrataCampoData RETURNS CHARACTER (INPUT ip-campo AS CHARACTER):
   IF ip-campo = "" OR ip-campo = ? THEN
      RETURN "".
   ELSE
      RETURN REPLACE(ENTRY(1,ip-campo,""),"/","").
END FUNCTION.

FUNCTION fcTrataCampoDecimal RETURNS CHARACTER (INPUT ip-campo AS CHARACTER):
   IF ip-campo = "" OR ip-campo = ? THEN
      RETURN "".
   ELSE
      RETURN STRING(DECIMAL(ip-campo)).
END FUNCTION.

FUNCTION fcTrataCampoCaracter RETURNS CHARACTER (INPUT ip-campo AS CHARACTER):
   IF ip-campo = "" OR ip-campo = ? THEN
      RETURN "".
   ELSE
      RETURN ENTRY(1,ip-campo,',').
END FUNCTION.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Gerando arquivo").
RUN pi-arquivo.
RUN pi-inicia-excel.
RUN pi-ccin.
RUN pi-cc00.
RUN pi-cc01.
RUN pi-pd00.
RUN pi-ic00.
RUN pi-ic01.
RUN pi-mc00.
RUN pi-mi00.
RUN pi-ac00.
RUN pi-encerra-excel.
RUN pi-gera-arquivo.
RUN pi-finalizar IN h-acomp.

PROCEDURE pi-arquivo:
   ASSIGN ip-arquivo-saida = REPLACE(ip-arquivo-entrada,"xlsx","csv").

   EMPTY TEMP-TABLE tt-ccin.
   EMPTY TEMP-TABLE tt-cc00.
   EMPTY TEMP-TABLE tt-cc01.
   EMPTY TEMP-TABLE tt-pd00.
   EMPTY TEMP-TABLE tt-ic00.
   EMPTY TEMP-TABLE tt-ic01.
   EMPTY TEMP-TABLE tt-mc00.
   EMPTY TEMP-TABLE tt-mi00.
   EMPTY TEMP-TABLE tt-ac00.
END PROCEDURE.

PROCEDURE pi-inicia-excel:
   CREATE "Excel.Application" chExcelApp. /* Define objeto como uma aplica‡Æo Excel*/
    
   chExcelApp:VISIBLE = FALSE. /* Nao mostra a planilha */

   chExcelApp:APPLICATION:displayalerts = FALSE.
   
   chWorkBook = chExcelApp:Workbooks:Add(ip-arquivo-entrada). /* Adiciona uma pasta de trabalho existente */
END PROCEDURE.
       
PROCEDURE pi-encerra-excel:
    chExcelApp:APPLICATION:displayalerts = FALSE.

   chExcelApp:Workbooks:Close().

   chExcelApp:quit(). /* Fechar Excel */
    
   /* Limpar objetos da mem¢ria */
   RELEASE OBJECT chWorksheet.
   RELEASE OBJECT chWorkbook.
   RELEASE OBJECT chExcelApp.
END PROCEDURE.

PROCEDURE pi-ccin: 
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa»’o de Contratos                          Folha:  01|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Importa»’o de Contratos                                     |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 2                                                           |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                             | Formato | Conteœdo | Decimais | Obrigat½rio |           |
   |--------+---------------------------------------------------------------+---------+----------+----------+-------------+-----------|
   |    A   | Codigo do registro (CCIN)                                     |     4   | Caracter |          |     Sim     |           |
   |    B   | 1-Inclus’o Contratos / 2-Inclus’o Itens / 3-Altera Pre»o Itens|     1   | Inteiro  |          |     Sim     |           |
   +----------------------------------------------------------------------------------------------------------------------------------+
   */

   /* Aponta para a ABA CCIN */
   chWorkSheet = chExcelApp:Sheets:Item(1).

   CREATE tt-ccin.
   ASSIGN tt-ccin.sequencia = 1
          tt-ccin.coluna-a  = "CCIN"
          tt-ccin.coluna-b  = fcTrataCampoInteiro(chWorksheet:Range("A" + STRING(5)):VALUE).

   CASE INTEGER(tt-ccin.coluna-b):
        WHEN 1 THEN ASSIGN c-acao = "INCLUSAO". 
        WHEN 2 THEN ASSIGN c-acao = "ALTERACAO".
        WHEN 3 THEN ASSIGN c-acao = "ALTERACAO-PRECO".
   END CASE.
END PROCEDURE.

PROCEDURE pi-cc00: 
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa»’o de Contratos                          Folha:  02|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Importa»’o de Contratos                                     |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 31                                                          |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat½rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |    A   | Codigo do registro (CC00)                               |     4   | Caracter |          |     Sim     |                 |
   |    B   | Numero do Contrato                                      |    16   | Caracter |          |     Sim     |                 |
   |    C   | Imprime Contrato? (S/N)                                 |     1   | Logico   |          |     Sim     |                 |
   |    D   | Modalidade                                              |     2   | Inteiro  |          |     Sim     |                 |
   |    E   | Natureza (1-Compra / 2-Servico / 3-Beneficiamento)      |     1   | Inteiro  |          |     Sim     |                 |
   |    F   | Gestor Tecnico                                          |    12   | Caracter |          |     Sim     |                 |
   |    G   | Tipo do Frete (Pago/A Pagar)                            |     1   | Inteiro  |          |     Sim     |                 |
   |    H   | Variacao de Quantidade                                  |     5   | Decimal  |     2    |     Nao     |                 |
   |    I   | Variacao de Preco                                       |     5   | Decimal  |     2    |     Nao     |                 |
   |    J   | Codigo da Mensagem (Mensagem do Pedido de CC)           |     3   | Inteiro  |          |     Nao     |                 |
   |    K   | Cond. Pagto (Qdo 0 gerar condicao especif.)             |     3   | Inteiro  |          |     Sim     |                 |
   |    L   | Codigo do Transportador                                 |     5   | Inteiro  |          |     Sim     |                 |
   |    M   | Via de Transporte (Rodoviaria - Fluvial ...)            |     1   | Inteiro  |          |     Sim     |                 |
   |    N   | Preco Total                                             |    13   | Decimal  |     4    |     Sim     |                 |
   |    O   | Codigo do Comprador                                     |    12   | Caracter |          |     Sim     |                 |
   |    P   | Situacao (1-Nao Emitido / 2-Emitido / 3-Cancelado)      |     1   | Inteiro  |          |     Sim     |                 |
   |    Q   | Quantidade Total                                        |    15   | Decimal  |     4    |     Nao     |                 |
   |    R   | Quantidade em Saldo                                     |    15   | Decimal  |     4    |     Nao     |                 |
   |    S   | Saldo Valor                                             |    12   | Decimal  |     4    |     Nao     |                 |
   |    T   | Quantidade Recebida                                     |    15   | Decimal  |     4    |     Nao     |                 |
   |    U   | Valor Recebido                                          |    13   | Decimal  |     4    |     Nao     |                 |
   |    V   | Quantidade Liberada                                     |    15   | Decimal  |     4    |     Nao     |                 |
   |    W   | Valor Liberado                                          |    13   | Decimal  |     4    |     Nao     |                 |
   |    X   | Faturamento Minimo                                      |    13   | Decimal  |     4    |     Nao     |                 |
   |    Y   | Data Inicial                                            |     8   | Data     |          |     Sim     |                 |
   |    Z   | Data Final                                              |     8   | Data     |          |     Sim     |                 |
   |   AA   | Data da Emissao                                         |     8   | Data     |          |     Sim     |                 |
   |   AB   | Codigo do Fornecedor		               	              |     9   | Inteiro  |          |     Sim     |                 |            
   |   AC   | Estabelecimento                                         |     5   | Caracter |          |     Sim     |                 |
   |   AD   | Estabelecimento de Cobranca                             |     5   | Caracter |          |     Sim     |                 |
   |   AE   | Estabelecimento de Origem                               |     5   | Caracter |          |     Sim     |                 |
   |   AF   | Estabelecimento de Entrega                              |     5   | Caracter |          |     Sim     |                 |
   |   AG   | Limite Valor (dec-2)                                    |    13   | Caracter |     4    |     Sim     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+
   */

   /* Aponta para a ABA CAPA */
   chWorkSheet = chExcelApp:Sheets:Item(2).

   CREATE tt-cc00.
   ASSIGN tt-cc00.sequencia  = 1
          tt-cc00.coluna-a   = "CC00"                                                              /*  Caracter  */
          tt-cc00.coluna-b   = fcTrataCampoInteiro (chWorksheet:Range("A"  + STRING(5)):VALUE)     /*  Caracter  */
          tt-cc00.coluna-c   = IF c-acao = "INCLUSAO" THEN ("S") ELSE fcTrataCampoCaracter(chWorksheet:Range("AA" + STRING(5)):VALUE)     /*  Logico    */
          tt-cc00.coluna-d   = fcTrataCampoInteiro (chWorksheet:Range("E"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-e   = fcTrataCampoInteiro (chWorksheet:Range("S"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-f   = fcTrataCampoCaracter(chWorksheet:Range("W"  + STRING(5)):VALUE)     /*  Caracter  */
          tt-cc00.coluna-g   = fcTrataCampoInteiro (chWorksheet:Range("R"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-h   = fcTrataCampoDecimal (chWorksheet:Range("X"  + STRING(5)):VALUE)     /*  Decimal   */
          tt-cc00.coluna-i   = fcTrataCampoDecimal (chWorksheet:Range("Y"  + STRING(5)):VALUE)     /*  Decimal   */
          tt-cc00.coluna-j   = fcTrataCampoInteiro (chWorksheet:Range("L"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-k   = fcTrataCampoInteiro (chWorksheet:Range("J"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-l   = fcTrataCampoInteiro (chWorksheet:Range("P"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-m   = fcTrataCampoInteiro (chWorksheet:Range("Q"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-n   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-o   = fcTrataCampoCaracter(chWorksheet:Range("V"  + STRING(5)):VALUE)     /*  Caracter  */
          tt-cc00.coluna-p   = fcTrataCampoInteiro (chWorksheet:Range("AC" + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-q   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-r   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-s   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-t   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-u   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-v   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-w   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-x   = ""                                                                  /*  Decimal   */
          tt-cc00.coluna-y   = fcTrataCampoData    (chWorksheet:Range("G"  + STRING(5)):VALUE)     /*  Data      */
          tt-cc00.coluna-z   = fcTrataCampoData    (chWorksheet:Range("H"  + STRING(5)):VALUE)     /*  Data      */
          tt-cc00.coluna-aa  = fcTrataCampoData    (chWorksheet:Range("F"  + STRING(5)):VALUE)     /*  Data      */
          tt-cc00.coluna-ab  = fcTrataCampoInteiro (chWorksheet:Range("C"  + STRING(5)):VALUE)     /*  Inteiro   */
          tt-cc00.coluna-ac  = fcTrataCampoInteiro (chWorksheet:Range("D"  + STRING(5)):VALUE)     /*  Caracter  */
          tt-cc00.coluna-ad  = ""                                                                  /*  Caracter  */
          tt-cc00.coluna-ae  = ""                                                                  /*  Caracter  */
          tt-cc00.coluna-af  = IF c-acao = "INCLUSAO" THEN ("") ELSE fcTrataCampoInteiro(chWorksheet:Range("M"  + STRING(5)):VALUE)     /*  Caracter  */
          tt-cc00.coluna-ag  = fcTrataCampoDecimal (chWorksheet:Range("AF" + STRING(5)):VALUE)     /*  Caracter  */
       .
END PROCEDURE.

PROCEDURE pi-cc01: 
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa»’o de Contratos                          Folha:  03|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Importa»’o de Contratos                                     |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 19                                                          |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat½rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |    A   | Codigo do registro (CC01)                               |     4   | Caracter |          |     Sim     |                 |
   |    B   | Numero do Contrato                                      |    16   | Caracter |          |     Sim     |                 |
   |    C   | Descri‡Æo do Contrato                                   |    32   | Caracter |          |     Sim     |                 |
   |    D   | Valor Pago                                              |    13   | Decimal  |          |     Sim     |                 |
   |    E   | Codigo da Moeda                                         |     2   | Inteiro  |          |     Sim     |                 |
   |    F   | Contrato Ativo (S/N)                                    |     1   | L¢gico   |          |     Sim     |                 |
   |    G   | Tipo de Fornecimento                                    |     2   | Inteiro  |          |     Sim     |                 |
   |    H   | Contato (deixar em branco)                              |    12   | Caracter |          |     Nao     |                 |
   |    I   | Indicador de Controle Recebimento 1-Total da nota 2-Item|     1   | Inteiro  |          |     Sim     |                 |
   |    J   | Saldo em Quantidade do Contrato                         |    15   | Decimal  |     4    |     Nao     |                 |
   |    K   | Saldo em Quantidade Liberado para Recebimento           |    15   | Decimal  |     4    |     Nao     |                 |
   |    L   | Saldo em Valor do Contrato                              |    13   | Decimal  |     4    |     Nao     |                 |
   |    M   | Saldo em Valor Liberado para Recebimento                |    13   | Decimal  |     4    |     Nao     |                 |
   |    N   | Codigo do Projeto                                       |    12   | Caracter |          |     Nao     |                 |
   |    O   | Condicao de Faturamento                                 |     3   | Inteiro  |          |     Sim     |                 |
   |    P   | Data Ultima alteracao efetuada na clausula do contrato  |     8   | Data     |          |     Sim     |                 |
   |    Q   | Contato                                                 |    40   | Caracter |          |     Nao     |                 |
   |    R   | Narrativa                                               |  2000   | Caracter |          |     Sim     |                 |
   |    S   | Ordem Investimentos                                     |     9   | Inteiro  |          |     Nao     |                 |
   |    T   | Percentual Alerta Saldo                                 |     4   | Decimal  |     2    |     Nao     |                 |
   |    U   | E-mail Alerta                                           |    40   | Caracter |          |     Nao     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+
   */

   /* Aponta para a ABA CAPA */
   chWorkSheet = chExcelApp:Sheets:Item(2).

   CREATE tt-cc01.
   ASSIGN tt-cc01.sequencia  = 1
          tt-cc01.coluna-a   = "CC01"                                                           /*   Caracter   */                
          tt-cc01.coluna-b   = fcTrataCampoInteiro (chWorksheet:Range("A"  + STRING(5)):VALUE)  /*   Caracter   */
          tt-cc01.coluna-c   = fcTrataCampoCaracter(chWorksheet:Range("B"  + STRING(5)):VALUE)  /*   Caracter   */
          tt-cc01.coluna-d   = ""                                                               /*   Decimal    */
          tt-cc01.coluna-e   = fcTrataCampoInteiro (chWorksheet:Range("I"  + STRING(5)):VALUE)  /*   Inteiro    */
          tt-cc01.coluna-f   = fcTrataCampoCaracter(chWorksheet:Range("AD" + STRING(5)):VALUE)  /*   L¢gico     */
          tt-cc01.coluna-g   = ""                                                               /*   Inteiro    */
          tt-cc01.coluna-h   = fcTrataCampoCaracter(chWorksheet:Range("U"  + STRING(5)):VALUE)  /*   Caracter   */
          tt-cc01.coluna-i   = fcTrataCampoInteiro (chWorksheet:Range("T"  + STRING(5)):VALUE)  /*   Inteiro    */
          tt-cc01.coluna-j   = ""                                                               /*   Decimal    */
          tt-cc01.coluna-k   = ""                                                               /*   Decimal    */
          tt-cc01.coluna-l   = ""                                                               /*   Decimal    */
          tt-cc01.coluna-m   = ""                                                               /*   Decimal    */
          tt-cc01.coluna-n   = fcTrataCampoCaracter(chWorksheet:Range("Z"  + STRING(5)):VALUE)  /*   Caracter   */
          tt-cc01.coluna-o   = IF c-acao = "INCLUSAO" THEN (fcTrataCampoInteiro(chWorksheet:Range("J"  + STRING(5)):VALUE)) ELSE (fcTrataCampoInteiro(chWorksheet:Range("K"  + STRING(5)):VALUE))  /*   Inteiro    */
          tt-cc01.coluna-p   = fcTrataCampoData    (chWorksheet:Range("AB" + STRING(5)):VALUE)  /*   Data       */
          tt-cc01.coluna-q   = fcTrataCampoCaracter(chWorksheet:Range("U"  + STRING(5)):VALUE)  /*   Caracter   */
          tt-cc01.coluna-r   = fcTrataCampoCaracter(chWorksheet:Range("AH" + STRING(5)):VALUE)  /*   Caracter   */
          tt-cc01.coluna-s   = fcTrataCampoInteiro (chWorksheet:Range("AG" + STRING(5)):VALUE)  /*   Inteiro    */
          tt-cc01.coluna-t   = fcTrataCampoDecimal (chWorksheet:Range("N"  + STRING(5)):VALUE)  /*   Decimal    */
          tt-cc01.coluna-u   = fcTrataCampoCaracter(chWorksheet:Range("O"  + STRING(5)):VALUE)  /*   Caracter   */
       .
END PROCEDURE.

PROCEDURE pi-pd00: 
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa‡Æo de Contratos                          Folha:  04|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Pedido de Compra Autom tico                                 |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 19                                                          |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat¢rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |    A   | Codigo do registro (PD00)                               |     4   | Caracter |          |     Sim     |                 |
   |    B   | Estabelecimento Entrega                                 |     5   | Caracter |          |     Sim     |                 |
   |    C   | Estabelecimento Cobran‡a                                |     5   | Caracter |          |     Sim     |                 |
   |    D   | Condi‡Æo de Pagamento                                   |     4   | Inteiro  |          |     Sim     |                 |
   |    E   | Emitente Entrega                                        |     9   | Inteiro  |          |     NÆo     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+
   */

   DEFINE VARIABLE v-coluna-d      LIKE tt-pd00.coluna-d    NO-UNDO INITIAL "". 

   /* Aponta para a ABA CAPA */                                                              
   chWorkSheet = chExcelApp:Sheets:Item(2).                                                  

   ASSIGN v-coluna-d    = fcTrataCampoInteiro(chWorksheet:Range("J"  + STRING(5)):VALUE).


   blk:
   DO i-cont = 5 TO 65000:
      ASSIGN c-valor = chWorksheet:Range("B" + STRING(i-cont)):VALUE.
            
      IF c-valor <> ? THEN DO:
         RUN pi-acompanhar IN h-acomp (INPUT "PD00:" + STRING(i-cont)).
         
         /* Aponta para a ABA ESTAB */
         chWorkSheet = chExcelApp:Sheets:Item(4).
         
         CREATE tt-pd00.
         ASSIGN tt-pd00.sequencia  = i-cont
                tt-pd00.coluna-a   = "PD00"                                                                 /*  Caracter */                                   
                tt-pd00.coluna-b   = fcTrataCampoInteiro(chWorksheet:Range("B"  + STRING(i-cont)):VALUE)    /*  Caracter */ 
                tt-pd00.coluna-c   = fcTrataCampoInteiro(chWorksheet:Range("C"  + STRING(i-cont)):VALUE)    /*  Caracter */ 
                tt-pd00.coluna-d   = ""                                                                     /*  Inteiro  */ 
                tt-pd00.coluna-e   = fcTrataCampoInteiro(chWorksheet:Range("D"  + STRING(i-cont)):VALUE)    /*  Inteiro  */ 
             . 

         ASSIGN tt-pd00.coluna-d   = v-coluna-d. 
      END.
      ELSE DO:
         ASSIGN i-cont = 65001.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-ic00: 
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa»’o de Contratos                          Folha:  05|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Item do Contrato                                            |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 42                                                          |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat½rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |   A    | Codigo do registro (IC00)                               |     4   | Caracter |          |     Sim     |                 |
   |   B    | Numero do Contrato                                      |    16   | Caracter |          |     Sim     |                 |
   |   C    | Preco Unitario                                          |    11   | Decimal  |     5    |     Sim     |                 |
   |   D    | Quantidade Minima                                       |    11   | Decimal  |     4    |     Nao     |                 |
   |   E    | Saldo a receber em valor para o item                    |    11   | Decimal  |     4    |     Nao     |                 |
   |   F    | Valor do Faturamento Minimo Fornecedor para o Item      |    12   | Decimal  |     4    |     Nao     |                 |
   |   G    | Codigo da Moeda                                         |     2   | Inteiro  |          |     Sim     |                 |
   |   H    | Contrato Ativo                                          |     1   | Logico   |          |     Sim     |                 |
   |   I    | Codigo do Item                                          |    16   | Caracter |          |     Sim     |                 |
   |   J    | Valor Total do Item no Contrato                         |    13   | Decimal  |     4    |     Sim     |                 |
   |   K    | Codigo da Referˆncia                                    |     8   | Caracter |          |     Sim     |                 |
   |   L    | IPI Incluso                                             |     1   | Logico   |          |     Sim     |                 |
   |   M    | ICMS                                                    |     1   | Inteiro  |          |     Sim     |                 |
   |   N    | Unidade de Medida                                       |     2   | Caracter |          |     Sim     |                 |
   |   O    | Contato                                                 |    12   | Caracter |          |     Sim     |                 |
   |   P    | Sequencia do Item                                       |     4   | Inteiro  |          |     Sim     |                 |
   |   Q    | Frequencia                                              |     3   | Inteiro  |          |     Sim     |                 |
   |   R    | Situacao:1-Nao emitido 2-Emitido 3-Cancelado 4-Atendido |     1   | Inteiro  |          |     Sim     |                 |
   |   S    | Quantidade Total                                        |    15   | Decimal  |     4    |     Nao     |                 |
   |   T    | Unidade do Contrato                                     |     1   | Inteiro  |          |     Sim     |                 |
   |   U    | Quantidade em Saldo                                     |    13   | Decimal  |     4    |     Sim     |                 |
   |   V    | Valor Recebido                                          |    15   | Decimal  |     4    |     Nao     |                 |
   |   W    | Quantidade Recebida                                     |    15   | Decimal  |     4    |     Nao     |                 |
   |   X    | Controle: 1 - Quantidade e Preco fixos 2 - qtde aberta  |         |          |          |             |                 |
   |        | Preco fixo 3 - Limite de custo                          |     1   | Inteiro  |          |     Sim     |                 |
   |   Y    | Saldo em Qtde do Contrato Liberado para Recebimento     |    15   | Decimal  |     4    |     Nao     |                 |
   |   Z    | Saldo em Valor do Contrato Liberado                     |    15   | Decimal  |     4    |     Nao     |                 |
   |  AA    | Item Controlado por Evento  (S/N)                       |     1   | Logico   |          |     Sim     |                 |
   |  AB    | Caracteristica : 1 - Aditivo 2 - Redutor                |     1   | Inteiro  |          |     Sim     |                 |
   |  AC    | Valor Obrigat½rio ao Contrato e Sempre Pago             |     1   | Logico   |          |     Sim     |                 |
   |  AD    | Multa                                                   |     1   | Logico   |          |     Sim     |                 |
   |  AE    | Percentual Multa diario                                 |     5   | Decimal  |     2    |     Nao     |                 |
   |  AF    | Percentual Multa Limite                                 |     5   | Decimal  |     2    |     Nao     |                 |
   |  AG    | Deposito                                                |     3   | Caracter |          |     Sim     |                 |
   |  AH    | Aliquota ICMS                                           |     5   | Decimal  |     2    |     Sim     |                 |
   |  AI    | Aliquota IPI                                            |     5   | Decimal  |     2    |     Sim     |                 |
   |  AJ    | Aliquota ISS                                            |     5   | Decimal  |     2    |     Sim     |                 |
   |  AK    | Tipo de Despesa                                         |     3   | Inteiro  |          |     Sim     |                 |
   |  AL    | Condicao de Pagamento                                   |     3   | Inteiro  |          |     Sim     |                 |
   |  AM    | Frete                                                   |     1   | Logico   |          |     Sim     |                 |
   |  AN    | Codigo do Fornecedor                                    |     9   | Inteiro  |          |     Sim     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+
   */

   DEFINE VARIABLE v-coluna-b      LIKE tt-ic00.coluna-b    NO-UNDO INITIAL "". 
   DEFINE VARIABLE v-coluna-g      LIKE tt-ic00.coluna-g    NO-UNDO INITIAL "". 
   DEFINE VARIABLE v-coluna-o      LIKE tt-ic00.coluna-o    NO-UNDO INITIAL "". 
   DEFINE VARIABLE v-coluna-an     LIKE tt-ic00.coluna-an   NO-UNDO INITIAL "". 
   DEFINE VARIABLE v-coluna-al     LIKE tt-ic00.coluna-al   NO-UNDO INITIAL "". 

   DEFINE VARIABLE c-cod-estabel   LIKE item-uni-estab.cod-estabel      NO-UNDO.
   DEFINE VARIABLE c-it-codigo     LIKE ITEM.it-codigo      NO-UNDO.
   DEFINE VARIABLE c-deposito-pad  LIKE ITEM.deposito-pad   NO-UNDO.
   DEFINE VARIABLE c-un            LIKE ITEM.deposito-pad   NO-UNDO.


   /* Aponta para a ABA CAPA */                                                              
   chWorkSheet = chExcelApp:Sheets:Item(2).                                                  
                                                                                             
   ASSIGN v-coluna-b    = fcTrataCampoInteiro (chWorksheet:Range("A"  + STRING(5)):VALUE)                    
          v-coluna-g    = fcTrataCampoInteiro (chWorksheet:Range("I"  + STRING(5)):VALUE)                    
          v-coluna-o    = fcTrataCampoCaracter(chWorksheet:Range("U"  + STRING(5)):VALUE)
          v-coluna-an   = fcTrataCampoInteiro (chWorksheet:Range("C"  + STRING(5)):VALUE)
          v-coluna-al   = fcTrataCampoInteiro (chWorksheet:Range("J"  + STRING(5)):VALUE).

   ASSIGN c-cod-estabel = fcTrataCampoInteiro (chWorksheet:Range("D"  + STRING(5)):VALUE).

   blk:
   DO i-cont = 5 TO 65000:
      /* Aponta para a ABA ITENS */
      chWorkSheet = chExcelApp:Sheets:Item(5).

      ASSIGN c-valor = chWorksheet:Range("B" + STRING(i-cont)):VALUE.
            
      IF c-valor <> ? THEN DO:
         RUN pi-acompanhar IN h-acomp (INPUT "IC00:" + STRING(i-cont)).

         ASSIGN c-it-codigo = fcTrataCampoCaracter(chWorksheet:Range("B"  + STRING(i-cont)):VALUE).

         ASSIGN c-deposito-pad = fcTrataCampoCaracter(chWorksheet:Range("AG"  + STRING(i-cont)):VALUE).

         FIND FIRST ITEM 
              WHERE ITEM.it-codigo =  c-it-codigo
              NO-LOCK NO-ERROR.
         IF AVAILABLE(ITEM) THEN
            ASSIGN c-un = item.un.

         FOR EACH item-uni-estab
                  WHERE item-uni-estab.cod-estabel = c-cod-estabel AND
                        item-uni-estab.it-codigo   = c-it-codigo
                  NO-LOCK:
            ASSIGN c-deposito-pad = item-uni-estab.deposito-pad.
         END.

         CREATE tt-ic00.
         ASSIGN tt-ic00.sequencia   = i-cont
                tt-ic00.coluna-a    = "IC00"                                                                  /*  Caracter */
                tt-ic00.coluna-b    = ""                                                                      /*  Caracter */
                tt-ic00.coluna-c    = fcTrataCampoDecimal (chWorksheet:Range("V"  + STRING(i-cont)):VALUE)    /*  Decimal  */        
                tt-ic00.coluna-d    = fcTrataCampoDecimal (chWorksheet:Range("D"  + STRING(i-cont)):VALUE)    /*  Decimal  */        
                tt-ic00.coluna-e    = ""                                                                      /*  Decimal  */        
                tt-ic00.coluna-f    = fcTrataCampoDecimal (chWorksheet:Range("L"  + STRING(i-cont)):VALUE)    /*  Decimal  */
                tt-ic00.coluna-g    = ""                                                                      /*  Inteiro  */
                tt-ic00.coluna-h    = IF c-acao = "INCLUSAO" THEN ("S") ELSE (fcTrataCampoCaracter(chWorksheet:Range("H"  + STRING(i-cont)):VALUE))    /*  Logico   */        
                tt-ic00.coluna-i    = fcTrataCampoCaracter(chWorksheet:Range("B"  + STRING(i-cont)):VALUE)    /*  Caracter */        
                tt-ic00.coluna-j    = fcTrataCampoDecimal (chWorksheet:Range("AK" + STRING(i-cont)):VALUE)    /*  Decimal  */
                tt-ic00.coluna-k    = ""                                                                      /*  Caracter */        
                tt-ic00.coluna-l    = fcTrataCampoCaracter(chWorksheet:Range("W"  + STRING(i-cont)):VALUE)    /*  Logico   */        
                tt-ic00.coluna-m    = fcTrataCampoInteiro (chWorksheet:Range("Y"  + STRING(i-cont)):VALUE)    /*  Inteiro  */        
                tt-ic00.coluna-n    = c-un                                                                    /*  Caracter */        
                tt-ic00.coluna-o    = ""                                                                      /*  Caracter */
                tt-ic00.coluna-p    = fcTrataCampoInteiro (chWorksheet:Range("A"  + STRING(i-cont)):VALUE)    /*  Inteiro  */        
                tt-ic00.coluna-q    = IF c-acao = "INCLUSAO" THEN ("1") ELSE (fcTrataCampoInteiro (chWorksheet:Range("E"  + STRING(i-cont)):VALUE))    /*  Inteiro  */        
                tt-ic00.coluna-r    = fcTrataCampoInteiro (chWorksheet:Range("R"  + STRING(i-cont)):VALUE)    /*  Inteiro  */        
                tt-ic00.coluna-s    = ""                                                                      /*  Decimal  */        
                tt-ic00.coluna-t    = fcTrataCampoInteiro (chWorksheet:Range("O"  + STRING(i-cont)):VALUE)    /*  Inteiro  */        
                tt-ic00.coluna-u    = ""                                                                      /*  Decimal  */        
                tt-ic00.coluna-v    = ""                                                                      /*  Decimal  */        
                tt-ic00.coluna-w    = ""                                                                      /*  Decimal  */        
                tt-ic00.coluna-x    = fcTrataCampoInteiro (chWorksheet:Range("Q"  + STRING(i-cont)):VALUE)    /*  Inteiro  */                
                tt-ic00.coluna-y    = ""                                                                      /*  Decimal  */        
                tt-ic00.coluna-z    = ""                                                                      /*  Decimal  */        
                tt-ic00.coluna-aa   = IF c-acao = "INCLUSAO" THEN ("N") ELSE (fcTrataCampoCaracter(chWorksheet:Range("G"  + STRING(i-cont)):VALUE))    /*  Logico   */        
                tt-ic00.coluna-ab   = fcTrataCampoInteiro (chWorksheet:Range("P"  + STRING(i-cont)):VALUE)    /*  Inteiro  */        
                tt-ic00.coluna-ac   = ""                                                                      /*  Logico   */        
                tt-ic00.coluna-ad   = IF c-acao = "INCLUSAO" THEN ("N") ELSE (fcTrataCampoCaracter(chWorksheet:Range("J"  + STRING(i-cont)):VALUE))    /*  Logico   */        
                tt-ic00.coluna-ae   = fcTrataCampoDecimal (chWorksheet:Range("I"  + STRING(i-cont)):VALUE)    /*  Decimal  */        
                tt-ic00.coluna-af   = fcTrataCampoDecimal (chWorksheet:Range("K"  + STRING(i-cont)):VALUE)    /*  Decimal  */        
                tt-ic00.coluna-ag   = c-deposito-pad                                                          /*  Caracter */        
                tt-ic00.coluna-ah   = fcTrataCampoDecimal (chWorksheet:Range("Z"  + STRING(i-cont)):VALUE)    /*  Decimal  */        
                tt-ic00.coluna-ai   = fcTrataCampoDecimal (chWorksheet:Range("X"  + STRING(i-cont)):VALUE)    /*  Decimal  */        
                tt-ic00.coluna-aj   = fcTrataCampoDecimal (chWorksheet:Range("AA" + STRING(i-cont)):VALUE)    /*  Decimal  */        
                tt-ic00.coluna-ak   = fcTrataCampoInteiro (chWorksheet:Range("AB" + STRING(i-cont)):VALUE)    /*  Inteiro  */
                tt-ic00.coluna-al   = ""                                                                      /*  Inteiro  */
                tt-ic00.coluna-am   = fcTrataCampoCaracter(chWorksheet:Range("AC" + STRING(i-cont)):VALUE)    /*  Logico   */
                tt-ic00.coluna-an   = ""                                                                      /*  Inteiro  */
             .
                                                                                                    
         /* Dados da CAPA */                                                              
         ASSIGN tt-ic00.coluna-b    = v-coluna-b                     
                tt-ic00.coluna-g    = v-coluna-g                     
                tt-ic00.coluna-o    = v-coluna-o 
                tt-ic00.coluna-an   = v-coluna-an
                tt-ic00.coluna-al   = v-coluna-al.
      END.
      ELSE DO:
         ASSIGN i-cont = 65001.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-ic01:
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa»’o de Contratos                          Folha:  06|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Item do Contrato                                            |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 23                                                          |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat½rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |    A   | Codigo do registro (IC01)                               |     4   | Caracter |          |     Sim     |                 |
   |    B   | Numero do Contrato                                      |    16   | Caracter |          |     Sim     |                 |
   |    C   | Sequencia do Item                                       |     4   | Inteiro  |          |     Sim     |                 |
   |    D   | Preco Fornecedor                                        |    16   | Decimal  |     5    |     Sim     |                 |
   |    E   | Taxa Financeira (Incluso/Nao Incluso)                   |     1   | Logico   |          |     Sim     |                 |
   |    F   | Valor Frete                                             |    13   | Decimal  |     4    |     Sim     |                 |
   |    G   | Valor Taxa                                              |     7   | Decimal  |     4    |     Sim     |                 |
   |    H   | Prazo Entrega                                           |     4   | Inteiro  |          |     Sim     |                 |
   |    I   | Data Preco Unitario                                     |     8   | Data     |          |     Sim     |                 |
   |    J   | Preco Base                                              |    16   | Decimal  |     5    |     Sim     |                 |
   |    K   | Comprador                                               |    12   | Caracter |          |     Sim     |                 |
   |    L   | Percentual de Desconto                                  |     5   | Decimal  |     2    |     Sim     |                 |
   |    M   | Narrativa Compra                                        |    76   | Caracter |          |     Sim     |                 |
   |    N   | Controle (1 - Medicao 2 - Ordem 3 - Programacao)        |     1   | Inteiro  |          |     Sim     |                 |
   |    O   | Preco Unitario Final                                    |    16   | Decimal  |     5    |     Sim     |                 |
   |    P   | Data Preco Base                                         |     8   | Data     |          |     Sim     |                 |
   |    Q   | Saldo Quantidade Recebido                               |    12   | Decimal  |     4    |     Sim     |                 |
   |    R   | Saldo Valor Recebido                                    |    12   | Decimal  |     4    |     Sim     |                 |
   |    S   | Numero Ordem                                            |     8   | Inteiro  |          |     Sim     |                 |
   |    T   | Narrativa                                               |  2000   | Caracter |          |     Nao     |                 |
   |    U   | Percentual de compra                                    |     3   | Inteiro  |          |     Sim     |                 |
   |    V   | Ordem Investimentos                                     |     9   | Inteiro  |          |     Nao     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+
   */

   DEFINE VARIABLE v-coluna-b LIKE tt-ic01.coluna-b NO-UNDO INITIAL "".
   DEFINE VARIABLE v-coluna-k LIKE tt-ic01.coluna-k NO-UNDO INITIAL "".

   /* Aponta para a ABA CAPA */
   chWorkSheet = chExcelApp:Sheets:Item(2).

   ASSIGN v-coluna-b = fcTrataCampoInteiro(chWorksheet:Range("A"  + STRING(5)):VALUE)
          v-coluna-k = fcTrataCampoInteiro(chWorksheet:Range("V"  + STRING(5)):VALUE).


   blk:
   DO i-cont = 5 TO 65000:
      /* Aponta para a ABA ITENS */
      chWorkSheet = chExcelApp:Sheets:Item(5).

      ASSIGN c-valor = chWorksheet:Range("B" + STRING(i-cont)):VALUE.
            
      IF c-valor <> ? THEN DO:
         RUN pi-acompanhar IN h-acomp (INPUT "IC01:" + STRING(i-cont)).
         
         CREATE tt-ic01.
         ASSIGN tt-ic01.sequencia   = i-cont 
                tt-ic01.coluna-a    = "IC01"                                                              /*   | Caracter   */
                tt-ic01.coluna-b    = ""                                                                  /*   | Caracter   */
                tt-ic01.coluna-c    = fcTrataCampoInteiro(chWorksheet:Range("A"  + STRING(i-cont)):VALUE) /*   | Inteiro    */
                tt-ic01.coluna-d    = fcTrataCampoDecimal(chWorksheet:Range("V"  + STRING(i-cont)):VALUE) /*   | Decimal    */
                tt-ic01.coluna-e    = chWorksheet:Range("AE" + STRING(i-cont)):VALUE                      /*   | Logico     */
                tt-ic01.coluna-f    = fcTrataCampoDecimal(chWorksheet:Range("AD" + STRING(i-cont)):VALUE) /*   | Decimal    */
                tt-ic01.coluna-g    = fcTrataCampoDecimal(chWorksheet:Range("AF" + STRING(i-cont)):VALUE) /*   | Decimal    */
                tt-ic01.coluna-h    = fcTrataCampoInteiro(chWorksheet:Range("AI" + STRING(i-cont)):VALUE) /*   | Inteiro    */
                tt-ic01.coluna-i    = fcTrataCampoData(chWorksheet:Range("T"  + STRING(i-cont)):VALUE)    /*   | Data       */
                tt-ic01.coluna-j    = fcTrataCampoDecimal(chWorksheet:Range("V"  + STRING(i-cont)):VALUE) /*   | Decimal    */
                tt-ic01.coluna-k    = ""                                                                  /*   | Caracter   */
                tt-ic01.coluna-l    = fcTrataCampoDecimal(chWorksheet:Range("AH" + STRING(i-cont)):VALUE) /*   | Decimal    */
                tt-ic01.coluna-m    = fcTrataCampoCaracter(chWorksheet:Range("S" + STRING(i-cont)):VALUE) /*   | Caracter   */
                tt-ic01.coluna-n    = fcTrataCampoInteiro(chWorksheet:Range("N"  + STRING(i-cont)):VALUE) /*   | Inteiro    */
                tt-ic01.coluna-o    = ""                                                                  /*   | Decimal    */
                tt-ic01.coluna-p    = fcTrataCampoData(chWorksheet:Range("T"  + STRING(i-cont)):VALUE)    /*   | Data       */
                tt-ic01.coluna-q    = ""                                                                  /*   | Decimal    */
                tt-ic01.coluna-r    = ""                                                                  /*   | Decimal    */
                tt-ic01.coluna-s    = ""                                                                  /*   | Inteiro    */
                tt-ic01.coluna-t    = fcTrataCampoCaracter(chWorksheet:Range("S" + STRING(i-cont)):VALUE) /*   | Caracter   */
                tt-ic01.coluna-u    = fcTrataCampoInteiro(chWorksheet:Range("F"  + STRING(i-cont)):VALUE) /*   | Inteiro    */
                tt-ic01.coluna-v    = fcTrataCampoInteiro(chWorksheet:Range("M"  + STRING(i-cont)):VALUE) /*   | Inteiro    */
             .

         ASSIGN tt-ic01.coluna-b    = v-coluna-b
                tt-ic01.coluna-k    = v-coluna-k.

/*          /* Controle do tipo de contrato  */                                               */
/*          IF c-acao = "INCLUSAO" THEN DO:                                                   */
/*             ASSIGN tt-ic00.coluna-t  = "1"                                                 */
/*                    tt-ic00.coluna-ab = "1".                                                */
/*                                                                                            */
/*             IF (INTEGER(tt-ic01.coluna-n) = 2) OR (INTEGER(tt-ic01.coluna-n) = 3) THEN DO: */
/*                FOR EACH tt-ic00                                                            */
/*                         EXCLUSIVE-LOCK:                                                    */
/*                   ASSIGN tt-ic00.coluna-x  = "1".                                          */
/*                END.                                                                        */
/*             END.                                                                           */
/*             ELSE DO:                                                                       */
/*                                                                                            */
/*             END.                                                                           */
/* /*             ELSE DO:                              */                                    */
/* /*                FOR EACH tt-ic00                   */                                    */
/* /*                         EXCLUSIVE-LOCK:           */                                    */
/* /*                   ASSIGN tt-ic00.coluna-t  = "2"  */                                    */
/* /*                          tt-ic00.coluna-ab = "2"  */                                    */
/* /*                          tt-ic00.coluna-x  = "2". */                                    */
/* /*                END.                               */                                    */
/* /*             END.                                  */                                    */
/*                                                                                            */
/* /*             FOR EACH tt-cc01                               */                           */
/* /*                      EXCLUSIVE-LOCK:                       */                           */
/* /*                ASSIGN tt-cc01.coluna-i = tt-ic01.coluna-n. */                           */
/* /*             END.                                           */                           */
/*          END.                                                                              */
      END.
      ELSE DO:
         ASSIGN i-cont = 65001.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-mc00: 
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa»’o de Contratos                          Folha:  07|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Matriz de Rateio Contrato                                   |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 6                                                           |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat½rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |    A   | Codigo do registro (MC00)                               |     4   | Caracter |          |     Sim     |                 |
   |    B   | Numero do Contrato                                      |    16   | Caracter |          |     Sim     |                 |
   |    C   | Conta                                                   |    20   | Caracter |          |     Sim     |                 |
   |    D   | Centro Custo                                            |    20   | Caracter |          |     Sim     |                 |
   |    E   | Percentual de Rateio                                    |     5   | Decimal  |     2    |     Sim     |                 |
   |    F   | Unidade de Neg½cio                                      |     3   | Caracter |          |     NÆo     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+
   */

   DEFINE VARIABLE v-coluna-b LIKE tt-mc00.coluna-b NO-UNDO INITIAL "".

   /* Aponta para a ABA CAPA */
   chWorkSheet = chExcelApp:Sheets:Item(2).

   ASSIGN v-coluna-b    = fcTrataCampoInteiro(chWorksheet:Range("A"  + STRING(5)):VALUE).

   blk:
   DO i-cont = 5 TO 65000:

      /* Aponta para a ABA MC00 */
      chWorkSheet = chExcelApp:Sheets:Item(3).

      ASSIGN c-valor = chWorksheet:Range("B" + STRING(i-cont)):VALUE.
            
      IF c-valor <> ? THEN DO:
         RUN pi-acompanhar IN h-acomp (INPUT "MC00:" + STRING(i-cont)).
         
         CREATE tt-mc00.
         ASSIGN tt-mc00.sequencia   = i-cont
                tt-mc00.coluna-a    = "MC00"                                                                  /*  Caracter */
                tt-mc00.coluna-b    = ""                                                                      /*  Caracter */
                tt-mc00.coluna-c    = fcTrataCampoCaracter(chWorksheet:Range("B"  + STRING(i-cont)):VALUE)    /*  Caracter */
                tt-mc00.coluna-d    = fcTrataCampoCaracter(chWorksheet:Range("C"  + STRING(i-cont)):VALUE)    /*  Caracter */
                tt-mc00.coluna-e    = fcTrataCampoDecimal (chWorksheet:Range("D"  + STRING(i-cont)):VALUE)    /*  Decimal  */
                tt-mc00.coluna-f    = fcTrataCampoCaracter(chWorksheet:Range("E"  + STRING(i-cont)):VALUE)    /*  Caracter */
             .

         ASSIGN tt-mc00.coluna-b    = v-coluna-b.
      END.
      ELSE DO:
         ASSIGN i-cont = 65001.
      END.
   END.
END PROCEDURE.


PROCEDURE pi-mi00: 
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                 Lay-out do Arquivo de Importa»’o de Contratos                          Folha:  08|
   +----------------------------------------------------------------------------------------------------------------------------------|
   |                                                                                                                                  |
   |                                                                                                                                  |
   +----------------------------------------------------------------------------------------------------------------------------------+
   |                                                     Nome do Arquivo: Matriz de Rateio Item                                       |
   |                                                             Formato: .csv                                                        |
   |                                               Quantidade de Colunas: 9                                                           |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat½rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |    A   | Codigo do registro (MI00)                               |     4   | Caracter |          |     Sim     |                 |
   |    B   | Numero do Contrato                                      |    16   | Caracter |          |     Sim     |                 |
   |    C   | Conta                                                   |    20   | Caracter |          |     Sim     |                 |
   |    D   | Centro Custo                                            |    20   | Caracter |          |     Sim     |                 |
   |    E   | Percentual de Rateio                                    |     5   | Decimal  |     2    |     Sim     |                 |
   |    F   | Sequencia do Item                                       |     4   | Inteiro  |          |     Sim     |                 |
   |    G   | Codigo do Item                                          |    16   | Caracter |          |     Sim     |                 |
   |    H   | Unidade de Neg½cio                                      |     3   | Caracter |          |     NÆo     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+   
   */

   DEFINE VARIABLE v-coluna-b LIKE tt-mi00.coluna-b NO-UNDO INITIAL "".

   /* Aponta para a ABA CAPA */
   chWorkSheet = chExcelApp:Sheets:Item(2).

   ASSIGN v-coluna-b    = fcTrataCampoInteiro(chWorksheet:Range("A"  + STRING(5)):VALUE).

   blk:
   DO i-cont = 5 TO 65000:
      /* Aponta para a ABA MI00 */
      chWorkSheet = chExcelApp:Sheets:Item(6).

      ASSIGN c-valor = chWorksheet:Range("C" + STRING(i-cont)):VALUE.
            
      IF c-valor <> ? THEN DO:
         RUN pi-acompanhar IN h-acomp (INPUT "MI00:" + STRING(i-cont)).
         
         CREATE tt-mi00.
         ASSIGN tt-mi00.sequencia   = i-cont
                tt-mi00.coluna-a    = "MI00"                                                                          /*  Caracter  */
                tt-mi00.coluna-b    = ""                                                                              /*  Caracter  */
                tt-mi00.coluna-c    = fcTrataCampoCaracter(chWorksheet:Range("B"  + STRING(i-cont)):VALUE)            /*  Caracter  */
                tt-mi00.coluna-d    = fcTrataCampoCaracter(chWorksheet:Range("C"  + STRING(i-cont)):VALUE)            /*  Caracter  */
                tt-mi00.coluna-e    = fcTrataCampoDecimal (chWorksheet:Range("D"  + STRING(i-cont)):VALUE)            /*  Decimal   */
                tt-mi00.coluna-f    = fcTrataCampoInteiro (chWorksheet:Range("E"  + STRING(i-cont)):VALUE)            /*  Inteiro   */
                tt-mi00.coluna-g    = fcTrataCampoCaracter(chWorksheet:Range("F"  + STRING(i-cont)):VALUE)            /*  Caracter  */
                tt-mi00.coluna-h    = fcTrataCampoCaracter(chWorksheet:Range("G"  + STRING(i-cont)):VALUE)            /*  Caracter  */
             .

         ASSIGN tt-mi00.coluna-b    = v-coluna-b.
      END.
      ELSE DO:
         ASSIGN i-cont = 65001.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-ac00:
   /*
   +----------------------------------------------------------------------------------------------------------------------------------+
   |               Lay-out do Arquivo de Importa»’o do Anexo de contratos                                                   Folha:  23|
   |----------------------------------------------------------------------------------------------------------------------------------|
   |                                                     Nome do Arquivo: anexo-contrat                                               |
   |                                                             Formato: Texto                                                       |
   |                                               Quantidade de Colunas: 6                                                           |
   |----------------------------------------------------------------------------------------------------------------------------------|
   | Coluna |                         Descri‡Æo                       | Formato | Conteœdo | Decimais | Obrigat½rio |                 |
   |--------+---------------------------------------------------------+---------+----------+----------+-------------+-----------------|
   |    A   | Codigo do registro  (AC00)                              |     4   | Caracter |          |     Sim     |                 |
   |    B   | Sequencia do Anexo                                      |     2   | Inteiro  |          |     Sim     |                 |
   |    C   | Usuario Ultima Alteracao (Controle interno)             |    12   | Caracter |          |     Nao     |                 |
   |    D   | Numero do Contrato                                      |     9   | Inteiro  |          |     Sim     |                 |
   |    E   | Descri‡Æo do Anexo                                      |    32   | Caracter |          |     Sim     |                 |
   |    F   | Narrativa do Anexo                                      |  2000   | Caracter |          |     Sim     |                 |
   +----------------------------------------------------------------------------------------------------------------------------------+   
   */

   DEFINE VARIABLE v-coluna-d LIKE tt-ac00.coluna-d NO-UNDO INITIAL "".
   DEFINE VARIABLE v-coluna-c LIKE tt-ac00.coluna-c NO-UNDO INITIAL "".

   /* Aponta para a ABA CAPA */
   chWorkSheet = chExcelApp:Sheets:Item(2).

   ASSIGN v-coluna-d    = fcTrataCampoInteiro (chWorksheet:Range("A"  + STRING(5)):VALUE)
          v-coluna-c    = fcTrataCampoCaracter(chWorksheet:Range("V"  + STRING(5)):VALUE).

   blk:
   DO i-cont = 5 TO 65000:
      /* Aponta para a ABA ITENS */
      chWorkSheet = chExcelApp:Sheets:Item(7).

      ASSIGN c-valor = chWorksheet:Range("B" + STRING(i-cont)):VALUE.
            
      IF c-valor <> ? THEN DO:
         RUN pi-acompanhar IN h-acomp (INPUT "AC00:" + STRING(i-cont)).
         
         CREATE tt-ac00.
         ASSIGN tt-ac00.sequencia   = i-cont
                tt-ac00.coluna-a    = "AC00"
                tt-ac00.coluna-b    = fcTrataCampoCaracter(chWorksheet:Range("A"  + STRING(i-cont)):VALUE)
                tt-ac00.coluna-c    = v-coluna-c
                tt-ac00.coluna-d    = ""
                tt-ac00.coluna-e    = fcTrataCampoCaracter(chWorksheet:Range("D"  + STRING(i-cont)):VALUE)
                tt-ac00.coluna-f    = fcTrataCampoCaracter(chWorksheet:Range("E"  + STRING(i-cont)):VALUE)
             .

         ASSIGN tt-ac00.coluna-d    = v-coluna-d.
      END.
      ELSE DO:
         ASSIGN i-cont = 65001.
      END.
   END.
END PROCEDURE.

 
PROCEDURE pi-gera-arquivo:
   OUTPUT TO VALUE(ip-arquivo-saida) NO-CONVERT NO-MAP.

   RUN pi-acompanhar IN h-acomp (INPUT "Gerando arquivo").

   /*CCIN*/
   FOR EACH tt-ccin
            NO-LOCK 
           BY tt-ccin.sequencia:
      PUT tt-ccin.coluna-a ";"
          tt-ccin.coluna-b ";" SKIP.
   END.

   /*CC00*/
   FOR EACH tt-cc00
            NO-LOCK 
            BY tt-cc00.sequencia:
      PUT tt-cc00.coluna-a  ";" 
          tt-cc00.coluna-b  ";" 
          tt-cc00.coluna-c  ";" 
          tt-cc00.coluna-d  ";" 
          tt-cc00.coluna-e  ";" 
          tt-cc00.coluna-f  ";" 
          tt-cc00.coluna-g  ";" 
          tt-cc00.coluna-h  ";" 
          tt-cc00.coluna-i  ";" 
          tt-cc00.coluna-j  ";" 
          tt-cc00.coluna-k  ";" 
          tt-cc00.coluna-l  ";" 
          tt-cc00.coluna-m  ";" 
          tt-cc00.coluna-n  ";" 
          tt-cc00.coluna-o  ";" 
          tt-cc00.coluna-p  ";" 
          tt-cc00.coluna-q  ";" 
          tt-cc00.coluna-r  ";" 
          tt-cc00.coluna-s  ";" 
          tt-cc00.coluna-t  ";" 
          tt-cc00.coluna-u  ";" 
          tt-cc00.coluna-v  ";" 
          tt-cc00.coluna-w  ";" 
          tt-cc00.coluna-x  ";" 
          tt-cc00.coluna-y  ";" 
          tt-cc00.coluna-z  ";" 
          tt-cc00.coluna-aa ";" 
          tt-cc00.coluna-ab ";" 
          tt-cc00.coluna-ac ";" 
          tt-cc00.coluna-ad ";" 
          tt-cc00.coluna-ae ";" 
          tt-cc00.coluna-af ";" 
          tt-cc00.coluna-ag ";" 
          SKIP.
   END.

   /*CC01*/
   FOR EACH tt-cc01
            NO-LOCK 
            BY tt-cc01.sequencia:
      PUT tt-cc01.coluna-a  ";" 
          tt-cc01.coluna-b  ";" 
          tt-cc01.coluna-c  ";" 
          tt-cc01.coluna-d  ";" 
          tt-cc01.coluna-e  ";" 
          tt-cc01.coluna-f  ";" 
          tt-cc01.coluna-g  ";" 
          tt-cc01.coluna-h  ";" 
          tt-cc01.coluna-i  ";" 
          tt-cc01.coluna-j  ";" 
          tt-cc01.coluna-k  ";" 
          tt-cc01.coluna-l  ";" 
          tt-cc01.coluna-m  ";" 
          tt-cc01.coluna-n  ";" 
          tt-cc01.coluna-o  ";" 
          tt-cc01.coluna-p  ";" 
          tt-cc01.coluna-q  ";" 
          tt-cc01.coluna-r  ";" 
          tt-cc01.coluna-s  ";" 
          tt-cc01.coluna-t  ";" 
          tt-cc01.coluna-u  ";"
          SKIP.
   END.

   /*PD00*/
   FOR EACH tt-pd00
            NO-LOCK 
            BY tt-pd00.sequencia:
      PUT tt-pd00.coluna-a  ";" 
          tt-pd00.coluna-b  ";" 
          tt-pd00.coluna-c  ";" 
          tt-pd00.coluna-d  ";" 
          tt-pd00.coluna-e  ";" 
          SKIP.
   END.

   /*IC00*/
   FOR EACH tt-ic00
            NO-LOCK 
            BY tt-ic00.sequencia:
      PUT tt-ic00.coluna-a  ";" 
          tt-ic00.coluna-b  ";" 
          tt-ic00.coluna-c  ";" 
          tt-ic00.coluna-d  ";" 
          tt-ic00.coluna-e  ";" 
          tt-ic00.coluna-f  ";" 
          tt-ic00.coluna-g  ";" 
          tt-ic00.coluna-h  ";" 
          tt-ic00.coluna-i  ";" 
          tt-ic00.coluna-j  ";" 
          tt-ic00.coluna-k  ";" 
          tt-ic00.coluna-l  ";" 
          tt-ic00.coluna-m  ";" 
          tt-ic00.coluna-n  ";" 
          tt-ic00.coluna-o  ";" 
          tt-ic00.coluna-p  ";" 
          tt-ic00.coluna-q  ";" 
          tt-ic00.coluna-r  ";" 
          tt-ic00.coluna-s  ";" 
          tt-ic00.coluna-t  ";" 
          tt-ic00.coluna-u  ";" 
          tt-ic00.coluna-v  ";" 
          tt-ic00.coluna-w  ";" 
          tt-ic00.coluna-x  ";" 
          tt-ic00.coluna-y  ";" 
          tt-ic00.coluna-z  ";" 
          tt-ic00.coluna-aa ";" 
          tt-ic00.coluna-ab ";" 
          tt-ic00.coluna-ac ";" 
          tt-ic00.coluna-ad ";" 
          tt-ic00.coluna-ae ";" 
          tt-ic00.coluna-af ";" 
          tt-ic00.coluna-ag ";" 
          tt-ic00.coluna-ah ";" 
          tt-ic00.coluna-ai ";" 
          tt-ic00.coluna-aj ";" 
          tt-ic00.coluna-ak ";" 
          tt-ic00.coluna-al ";" 
          tt-ic00.coluna-am ";" 
          tt-ic00.coluna-an ";" 
          SKIP.
   END.

   /*IC01*/
   FOR EACH tt-ic01
            NO-LOCK 
            BY tt-ic01.sequencia:
      PUT tt-ic01.coluna-a  ";" 
          tt-ic01.coluna-b  ";" 
          tt-ic01.coluna-c  ";" 
          tt-ic01.coluna-d  ";" 
          tt-ic01.coluna-e  ";" 
          tt-ic01.coluna-f  ";" 
          tt-ic01.coluna-g  ";" 
          tt-ic01.coluna-h  ";" 
          tt-ic01.coluna-i  ";" 
          tt-ic01.coluna-j  ";" 
          tt-ic01.coluna-k  ";" 
          tt-ic01.coluna-l  ";" 
          tt-ic01.coluna-m  ";" 
          tt-ic01.coluna-n  ";" 
          tt-ic01.coluna-o  ";" 
          tt-ic01.coluna-p  ";" 
          tt-ic01.coluna-q  ";" 
          tt-ic01.coluna-r  ";" 
          tt-ic01.coluna-s  ";" 
          tt-ic01.coluna-t  ";" 
          tt-ic01.coluna-u  ";" 
          tt-ic01.coluna-v  ";" 
          SKIP.
   END.

   /*MC00*/
   FOR EACH tt-mc00
            NO-LOCK 
            BY tt-mc00.sequencia:
      PUT tt-mc00.coluna-a  ";" 
          tt-mc00.coluna-b  ";" 
          tt-mc00.coluna-c  ";" 
          tt-mc00.coluna-d  ";" 
          tt-mc00.coluna-e  ";" 
          tt-mc00.coluna-f  ";" 
          SKIP.
   END.

   /*MI00*/
   FOR EACH tt-mi00
            NO-LOCK 
            BY tt-mi00.sequencia:
      PUT tt-mi00.coluna-a  ";" 
          tt-mi00.coluna-b  ";" 
          tt-mi00.coluna-c  ";" 
          tt-mi00.coluna-d  ";" 
          tt-mi00.coluna-e  ";" 
          tt-mi00.coluna-f  ";" 
          tt-mi00.coluna-g  ";" 
          tt-mi00.coluna-h  ";" 
          SKIP.
   END.

   /*AC00*/
   FOR EACH tt-ac00
            NO-LOCK 
            BY tt-ac00.sequencia:
      PUT tt-ac00.coluna-a  ";" 
          tt-ac00.coluna-b  ";" 
          tt-ac00.coluna-c  ";" 
          tt-ac00.coluna-d  ";" 
          tt-ac00.coluna-e  ";" 
          tt-ac00.coluna-f  ";" 
          SKIP.
   END.

   OUTPUT CLOSE.
END PROCEDURE.

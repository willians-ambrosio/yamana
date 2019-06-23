/***************************************************************************************************
**    Programa: upc-cn0206-u01.p
**    Objetivo: Chamador de UPC espec�fica
**       Autor: Willians Moreira Ambrosio - Grupo DKP
**   Descri��o: Cria Bot�o Excel, fill-in para buscar arquivo importa��o e gera��o arquivo layout 
**              importa��o
***************************************************************************************************/

{include/i-prgvrs.i upc-cn0206-U01 12.01.19.001} 
{tools/fc-handle-obj.i}

DEF INPUT PARAM p-ind-event  AS CHAR          no-undo.
DEF INPUT PARAM p-ind-object AS CHAR          no-undo.
DEF INPUT PARAM p-wgh-object AS HANDLE        no-undo.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE no-undo.
DEF INPUT PARAM p-cod-table  AS CHAR          no-undo.
DEF INPUT PARAM p-row-table  AS ROWID         no-undo.

def new global shared var wh-cn0206-bt-executar        AS widget-handle no-undo.
def new global shared var wh-cn0206-bt-executar-falso  AS widget-handle no-undo.
def new global shared var wh-cn0206-bt-cancelar        AS widget-handle no-undo.
def new global shared var wh-cn0206-bt-excel           AS widget-handle no-undo.
def new global shared var wh-cn0206-c-arquivo-entrada  AS widget-handle no-undo.  
def new global shared var wh-cn0206-bt-arquivo-entrada AS widget-handle no-undo.  
def new global shared var wh-cn0206-txt-import         AS widget-handle no-undo. 
def new global shared var wh-cn0206-fill-import        AS widget-handle no-undo. 
def new global shared var wh-cn0206-f-pg-par           AS widget-handle no-undo. 
def new global shared var wh-cn0206-bt-search          AS widget-handle no-undo. 
def new global shared var wh-cn0206-RECT-8             AS widget-handle no-undo. 
def new global shared var wh-cn0206-RECT-8-import      AS widget-handle no-undo. 


DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren      AS CHARACTER FORMAT "x(12)" LABEL "Usu�rio Corrente" COLUMN-LABEL "Usu�rio Corrente" NO-UNDO.

DEFINE VARIABLE chExcelApplication AS COM-HANDLE           NO-UNDO.
DEFINE VARIABLE cArquivoImport     AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cArquivoExport     AS CHARACTER            NO-UNDO.
DEFINE VARIABLE c-it-codigo        AS CHARACTER            NO-UNDO.        
DEFINE VARIABLE c-contato          AS CHARACTER            NO-UNDO.        
DEFINE VARIABLE c-cod-comprador    AS CHARACTER            NO-UNDO.        
DEFINE VARIABLE c-narrativa-comp   AS CHARACTER            NO-UNDO.        
DEFINE VARIABLE c-narrativa        AS CHARACTER            NO-UNDO.
DEFINE VARIABLE h-acomp            AS HANDLE               NO-UNDO.
DEFINE VARIABLE l-ipi-incluso      AS LOGICAL              NO-UNDO.        
DEFINE VARIABLE l-multa            AS LOGICAL              NO-UNDO.        
DEFINE VARIABLE l-frete            AS LOGICAL              NO-UNDO.        
DEFINE VARIABLE i_contador         AS INTEGER    INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-nr-contrato      AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE i-mo-moeda         AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE i-icms             AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE i-nr-sequencia     AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE i-situacao         AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE i-tipo-despesa     AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE i-cond-pagto       AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE i-num-ordem        AS INTEGER    INITIAL 0 NO-UNDO.          
DEFINE VARIABLE i-tipo-controle    AS INTEGER    INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-caract-item      AS INTEGER    INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-control-preco    AS INTEGER    INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-prazo-entrega    AS INTEGER    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-vl-base          AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-preco-unit       AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-qt-total         AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-perc-multa       AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-perc-multa-dia   AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-aliq-icms        AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-aliq-ipi         AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-aliq-iss         AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-preco-fornec     AS DECIMAL FORMAT "99999999999.99999"   INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-vl-frete         AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-vl-taxa          AS DECIMAL    INITIAL 0 NO-UNDO.        
DEFINE VARIABLE d-perc-desconto    AS DECIMAL    INITIAL 0 NO-UNDO.        

/* --------------------------------------------------------------------- */
DEFINE TEMP-TABLE tt-import NO-UNDO
    FIELD nr-contrato   AS INTEGER
    FIELD nr-sequencia  AS INTEGER
    FIELD campos        AS CHARACTER EXTENT 65.
/* --------------------------------------------------------------------- */
FUNCTION RetiraSimbolos RETURNS CHAR
   ( INPUT-OUTPUT p_string AS CHAR):

   DEF VAR cStringAux AS CHAR NO-UNDO.      

   IF INDEX(p_string,",") = 0 THEN
      RETURN p_string.

   ASSIGN cStringAux  = SUBSTRING(p_string,INDEX(p_string,","),LENGTH(p_string))
          p_string    = REPLACE(p_string,cStringAux,"").

   RETURN p_string.   /* Retorna a nova string */
END.
/* --------------------------------------------------------------------- */
DEF VAR c-handle-obj             AS CHAR                      NO-UNDO.
DEF VAR ct                       AS INT                       NO-UNDO.
def var i-pagina                 as INTEGER                   no-undo.
/* --------------------------------------------------------------------- */
IF p-ind-event  = "INITIALIZE" AND
   p-ind-object = "CONTAINER"  THEN 
DO:   
   c-handle-obj                 = fc-handle-obj("bt-executar,bt-cancelar", p-wgh-frame).
   wh-cn0206-bt-executar        = WIDGET-HANDLE(ENTRY(01,c-handle-obj)) NO-ERROR.
   wh-cn0206-bt-cancelar        = WIDGET-HANDLE(ENTRY(02,c-handle-obj)) NO-ERROR.
      
   IF VALID-HANDLE(wh-cn0206-bt-cancelar) THEN 
   DO:
      CREATE BUTTON wh-cn0206-bt-excel
      ASSIGN ROW       = wh-cn0206-bt-cancelar:ROW
             COLUMN    = wh-cn0206-bt-cancelar:COL   + 20
             WIDTH     = wh-cn0206-bt-cancelar:WIDTH - 3
             HEIGHT    = wh-cn0206-bt-cancelar:HEIGHT
             FRAME     = wh-cn0206-bt-cancelar:FRAME
             SENSITIVE = wh-cn0206-bt-cancelar:SENSITIVE
             VISIBLE   = wh-cn0206-bt-cancelar:VISIBLE
             LABEL     = "Modelo Excel"
             TOOLTIP   = "Modelo Excel"
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cn0206-u01.p (INPUT "wh-cn0206-bt-excel",   
                                                                 INPUT "upc-cn0206-u01"    ,  
                                                                 INPUT p-wgh-object        ,  
                                                                 INPUT p-wgh-frame         ,  
                                                                 INPUT p-cod-table         ,  
                                                                 INPUT p-row-table         ).     
             END TRIGGERS.
             wh-cn0206-bt-excel:LOAD-IMAGE("image/excel.bmp").

      CREATE BUTTON wh-cn0206-bt-executar-falso
      ASSIGN ROW        = wh-cn0206-bt-executar:ROW
             COLUMN     = wh-cn0206-bt-executar:COL   
             WIDTH      = wh-cn0206-bt-executar:WIDTH 
             HEIGHT     = wh-cn0206-bt-executar:HEIGHT
             FRAME      = wh-cn0206-bt-executar:FRAME
             SENSITIVE  = wh-cn0206-bt-executar:SENSITIVE
             VISIBLE    = wh-cn0206-bt-executar:VISIBLE
             LABEL      = "*" + wh-cn0206-bt-executar:LABEL
             TOOLTIP    = wh-cn0206-bt-executar:TOOLTIP  
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cn0206-u01.p (INPUT "wh-cn0206-bt-executar-falso",   
                                                                 INPUT "upc-cn0206-u01"    ,  
                                                                 INPUT p-wgh-object        ,  
                                                                 INPUT p-wgh-frame         ,  
                                                                 INPUT p-cod-table         ,  
                                                                 INPUT p-row-table         ).     
             END TRIGGERS.        
   END.

   c-handle-obj         = fc-handle-obj("f-pg-par", p-wgh-frame:PARENT).
   wh-cn0206-f-pg-par   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).

   c-handle-obj = fc-handle-obj("c-arquivo-entrada,bt-arquivo-entrada,RECT-8",wh-cn0206-f-pg-par:PARENT).   
   wh-cn0206-c-arquivo-entrada  = WIDGET-HANDLE(ENTRY(01,c-handle-obj)) NO-ERROR. 
   wh-cn0206-bt-arquivo-entrada = WIDGET-HANDLE(ENTRY(02,c-handle-obj)) NO-ERROR. 
   wh-cn0206-RECT-8             = WIDGET-HANDLE(ENTRY(03,c-handle-obj)) NO-ERROR. 

   IF VALID-HANDLE(wh-cn0206-c-arquivo-entrada)  THEN
   DO:
      create text wh-cn0206-txt-import
      assign frame        = wh-cn0206-c-arquivo-entrada:frame
             format       = "x(22)"
             width        = 18
             screen-value = "Arquivo de Importa��o"
             row          = wh-cn0206-c-arquivo-entrada:ROW + 2
             col          = wh-cn0206-c-arquivo-entrada:COL + 1
             visible      = yes. 

      CREATE FILL-IN wh-cn0206-fill-import
      ASSIGN FRAME        = wh-cn0206-txt-import:FRAME
             NAME         = 'fi-arq-import'
             WIDTH        = 40
             HEIGHT       = 0.88
             ROW          = wh-cn0206-c-arquivo-entrada:ROW + 3
             DATA-TYPE    = "CHARACTER"
             FORMAT       = "X(256)"
             COL          = wh-cn0206-c-arquivo-entrada:COL
             BGCOLOR      = wh-cn0206-c-arquivo-entrada:BGCOLOR 
             FGCOLOR      = wh-cn0206-c-arquivo-entrada:FGCOLOR 
             VISIBLE      = YES
             SENSITIVE    = YES 
             TOOLTIP      = ''.
   END.

   IF VALID-HANDLE(wh-cn0206-bt-arquivo-entrada)  THEN
   DO:
      CREATE BUTTON wh-cn0206-bt-search
      ASSIGN ROW       = wh-cn0206-bt-arquivo-entrada:ROW   + 3
             COLUMN    = wh-cn0206-bt-arquivo-entrada:COL
             WIDTH     = wh-cn0206-bt-arquivo-entrada:WIDTH 
             HEIGHT    = wh-cn0206-bt-arquivo-entrada:HEIGHT
             FRAME     = wh-cn0206-bt-arquivo-entrada:FRAME
             SENSITIVE = wh-cn0206-bt-arquivo-entrada:SENSITIVE
             VISIBLE   = wh-cn0206-bt-arquivo-entrada:VISIBLE
             LABEL     = wh-cn0206-bt-arquivo-entrada:LABEL
             TOOLTIP   = wh-cn0206-bt-arquivo-entrada:TOOLTIP
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cn0206-u01.p (INPUT "wh-cn0206-bt-import",
                                                                 INPUT "upc-cn0206-u01"    ,
                                                                 INPUT p-wgh-object        ,
                                                                 INPUT p-wgh-frame         ,
                                                                 INPUT p-cod-table         ,
                                                                 INPUT p-row-table         ).
             END TRIGGERS.

      wh-cn0206-bt-search:LOAD-IMAGE("image/im-sea.bmp") NO-ERROR.      
      wh-cn0206-bt-search:LOAD-IMAGE-INSENSITIVE("image/ii-sea.bmp") NO-ERROR. 
             
   END.

   IF VALID-HANDLE(wh-cn0206-RECT-8 ) THEN
   DO:   
      CREATE RECTANGLE wh-cn0206-RECT-8-import 
          ASSIGN FRAME = wh-cn0206-RECT-8:FRAME
                 WIDTH = wh-cn0206-RECT-8:WIDTH
                   COL = wh-cn0206-RECT-8:COL
                   ROW = wh-cn0206-RECT-8:ROW + 3
               VISIBLE = wh-cn0206-RECT-8:VISIBLE   
                HEIGHT = wh-cn0206-RECT-8:HEIGHT
               FGCOLOR = wh-cn0206-RECT-8:FGCOLOR
               BGCOLOR = wh-cn0206-RECT-8:BGCOLOR
           EDGE-PIXELS = wh-cn0206-RECT-8:EDGE-PIXELS.
   END.
END. 

IF  p-ind-event   = "wh-cn0206-bt-executar-falso" AND
    p-ind-object  = "upc-cn0206-u01"              THEN 
DO:

    IF wh-cn0206-fill-import:SCREEN-VALUE <> "" THEN
    DO:

       IF SEARCH(wh-cn0206-fill-import:SCREEN-VALUE) = ? THEN
       DO:              
          run utp/ut-msgs.p (input "show",
                             input 17006,
                             input "Arquivo Excel Inv�lido~~Informe um arquivo " + wh-cn0206-fill-import:SCREEN-VALUE + "inv�lido e/ou n�o informado corretamente. Informe um arquivo v�lido").        
    
          APPLY "ENTRY" TO wh-cn0206-fill-import.
          RETURN NO-APPLY.
       END.
       ELSE
       DO:
                    
           ASSIGN wh-cn0206-c-arquivo-entrada:SCREEN-VALUE = SESSION:TEMP-DIRECTORY + "cn0206-import" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".lst"
                  c-cod-comprador = v_cod_usuar_corren.

           RUN pi-import-excel.

           RUN pi-export-arquivo.

           RUN pi-ajusta-campos.

       END.
    END.    

    APPLY "choose" TO wh-cn0206-bt-executar.

END.

IF  p-ind-event   = "wh-cn0206-bt-excel" AND
    p-ind-object  = "upc-cn0206-u01"     THEN 
DO:

    DEFINE VARIABLE i-propath       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE hInstance       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cArquivoModelo  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cArquivoTemp    AS CHARACTER   NO-UNDO.

    SESSION:SET-WAIT-STATE("":U).

    REPEAT i-propath = 1 TO NUM-ENTRIES(PROPATH,","):

        IF SEARCH(STRING(ENTRY(i-propath,PROPATH)) + "/modelos/cn0206.xlsx") <> ? THEN DO:

           ASSIGN cArquivoModelo = STRING(ENTRY(i-propath,PROPATH)) + "/modelos/cn0206.xlsx"
                  cArquivoTemp   = SESSION:TEMP-DIRECTORY + "cn0206" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".xlsx".

           /* Copia o modelo j� para o diret�rio tempor�rio */
           OS-COPY VALUE(cArquivoModelo) VALUE(cArquivoTemp).
           
           RUN ShellExecuteA (0,
                              "open",
                              "excel.exe",
                              cArquivoTemp,
                              "",
                              1,
                              OUTPUT hInstance).

           LEAVE.
        END.
    END.
END.

IF  p-ind-event   = "wh-cn0206-bt-import" AND
    p-ind-object  = "upc-cn0206-u01"     THEN 
DO:
    DEFINE VARIABLE procname AS CHARACTER NO-UNDO.
    DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.

    SYSTEM-DIALOG GET-FILE procname
        TITLE      "Destino"
        FILTERS    "Arquivos XLSX (*.xlsx)"      "*.xlsx"
        USE-FILENAME UPDATE OKpressed.

    IF OKpressed = TRUE THEN DO:
       ASSIGN wh-cn0206-fill-import:SCREEN-VALUE =  procname .
    END.
    ELSE 
        LEAVE.
END.

PROCEDURE ShellExecuteA EXTERNAL "shell32":
     DEFINE INPUT PARAMETER HWND         AS LONG.
     DEFINE INPUT PARAMETER lpOperation  AS CHAR.
     DEFINE INPUT PARAMETER lpFile       AS CHAR.
     DEFINE INPUT PARAMETER lpParameters AS CHAR.
     DEFINE INPUT PARAMETER lpDirectory  AS CHAR.
     DEFINE INPUT PARAMETER nShowCmd     AS LONG.
     DEFINE RETURN PARAMETER hInstance   AS LONG.
END.

PROCEDURE pi-import-excel:
    DEFINE VARIABLE cAux AS CHARACTER   NO-UNDO.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    RUN pi-inicializar IN h-acomp(INPUT "Aguarde, Compondo os Dados...").

    CREATE "Excel.Application" chExcelApplication.
    chexcelapplication:workbooks:OPEN(wh-cn0206-fill-import:SCREEN-VALUE,TRUE).
    chexcelapplication:sheets:item(1).

    ASSIGN i_contador = 3.

    EMPTY TEMP-TABLE tt-import NO-ERROR.

    REPEAT:

        /* sai do repeat */
        IF chExcelApplication:Range("A" + STRING(i_contador)):VALUE = ""
        OR chExcelApplication:Range("A" + STRING(i_contador)):VALUE = ? THEN LEAVE.

        ASSIGN i_contador = i_contador + 1.

        ASSIGN cAux  = STRING(chExcelApplication:Range("A"  + STRING(i_contador)):VALUE)                                i-nr-contrato    = INTEGER  (cAux)  /* A - Contrato               */
               cAux  = STRING(chExcelApplication:Range("B"  + STRING(i_contador)):VALUE)                                i-nr-sequencia   = INTEGER  (cAux)  /* B - Seq. Item              */
               cAux  = STRING(chExcelApplication:Range("C"  + STRING(i_contador)):VALUE) cAux  =  RetiraSimbolos (cAux) c-it-codigo      = STRING   (cAux)  /* C - Item                   */ 
               cAux  = STRING(chExcelApplication:Range("D"  + STRING(i_contador)):VALUE) cAux  =  RetiraSimbolos (cAux) c-narrativa      = STRING   (cAux)  /* D - Narrativa              */
               cAux  = STRING(chExcelApplication:Range("E"  + STRING(i_contador)):VALUE)                                i-icms           = INTEGER  (cAux)  /* E - ICMS                   */
               cAux  = STRING(chExcelApplication:Range("F"  + STRING(i_contador)):VALUE)                                d-aliq-icms      = DECIMAL  (cAux)  /* F - Aliquota ICMS          */
               cAux  = STRING(chExcelApplication:Range("G"  + STRING(i_contador)):VALUE)                                i-tipo-controle  = INTEGER  (cAux)  /* G - Tipo Controle          */ 
               cAux  = STRING(chExcelApplication:Range("H"  + STRING(i_contador)):VALUE)                                i-caract-item    = INTEGER  (cAux)  /* H - Caracteristica Item    */
               cAux  = STRING(chExcelApplication:Range("I"  + STRING(i_contador)):VALUE)                                i-control-preco  = INTEGER  (cAux)  /* I - Controle Pre�o/QTDE    */
               cAux  = STRING(chExcelApplication:Range("J"  + STRING(i_contador)):VALUE)                                d-aliq-ipi       = DECIMAL  (cAux)  /* J - Aliquota IPI           */
               cAux  = STRING(chExcelApplication:Range("K"  + STRING(i_contador)):VALUE)                                d-aliq-iss       = DECIMAL  (cAux)  /* K - Aliquota ISS           */
               cAux  = STRING(chExcelApplication:Range("L"  + STRING(i_contador)):VALUE)                                i-tipo-despesa   = INTEGER  (cAux)  /* L - Tipo de Despesa        */
               cAux  = STRING(chExcelApplication:Range("M"  + STRING(i_contador)):VALUE)                                i-prazo-entrega  = INTEGER  (cAux)  /* M - Prazo Entrega          */
               cAux  = STRING(chExcelApplication:Range("N"  + STRING(i_contador)):VALUE) cAux  =  RetiraSimbolos (cAux) c-narrativa-comp = STRING   (cAux)  /* N - Narrativa Compra       */
               cAux  = STRING(chExcelApplication:Range("O"  + STRING(i_contador)):VALUE)                                i-num-ordem      = INTEGER  (cAux)  /* O - Numero Ordem           */
               cAux  = STRING(chExcelApplication:Range("P"  + STRING(i_contador)):VALUE)                                d-preco-unit     = DECIMAL  (cAux)  /* P - Preco Unitario         */ 
               cAux  = STRING(chExcelApplication:Range("Q"  + STRING(i_contador)):VALUE)                                d-preco-fornec   = DECIMAL  (cAux)  /* Q - Preco Fornecedor       */
               cAux  = STRING(chExcelApplication:Range("R"  + STRING(i_contador)):VALUE)                                d-qt-total       = DECIMAL  (cAux)  /* R - Qtd.Total              */ .

        IF c-contato = ? THEN
           ASSIGN c-contato = "".

        IF i-num-ordem = ? THEN
           ASSIGN i-num-ordem = 0.

        IF INDEX(c-it-codigo,",") <> 0 THEN
            ASSIGN cAux        = SUBSTRING(c-it-codigo,INDEX(c-it-codigo,","),LENGTH(c-it-codigo))
                   c-it-codigo = REPLACE(c-it-codigo,cAux,"").

        ASSIGN c-narrativa-comp = REPLACE(c-narrativa-comp,CHR(10),"")
               c-narrativa      = REPLACE(c-narrativa     ,CHR(10),"").

        FIND FIRST ITEM WHERE
                   ITEM.it-codigo = c-it-codigo NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN NEXT.

        FIND FIRST contrato-for WHERE
                   contrato-for.nr-contrato = i-nr-contrato NO-LOCK NO-ERROR.
        IF NOT AVAIL contrato-for THEN NEXT.

        ASSIGN d-vl-base    = 0
               d-preco-unit = 0.

        IF d-preco-unit > 0 THEN
           ASSIGN d-vl-base = d-preco-unit.

        

        ASSIGN i-mo-moeda   = contrato-for.moeda
               i-cond-pagto = contrato-for.cod-cond-pag.

        FIND FIRST tt-import WHERE
                   tt-import.nr-contrato  = i-nr-contrato  AND
                   tt-import.nr-sequencia = i-nr-sequencia EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL tt-import THEN  
        DO:
            CREATE tt-import.                                                                                             /* |----------------------------------------------------------------------------------------------------------------------------------| */  
            ASSIGN tt-import.nr-contrato     = i-nr-contrato                                                              /* | Ordem |                         Descricao                       | Tamanho | Inicio | Termino | Conteudo | Decimais | Obrigatorio | */                                          
                   tt-import.nr-sequencia    = i-nr-sequencia                                                             /* |-------+---------------------------------------------------------+---------+--------+---------+----------+----------+-------------| */  
                   tt-import.campos[01]      = "IC00"                                                                     /* |    1  | Codigo do Registro (IC00)                               |     4   |     1  |     4   | Caracter |          |     Sim     | */  
                   tt-import.campos[02]      = FILL(" ",006)                                                              /* |    2  | Deixar em branco                                        |     6   |     5  |    10   |          |          |     Sim     | */  
                   tt-import.campos[03]      = STRING(i-nr-contrato,"999999999") + FILL(" ",007)                          /* |    3  | Numero do Contrato                                      |    16   |    11  |    26   | Caracter |          |     Sim     | */  
                   tt-import.campos[04]      = REPLACE(REPLACE(STRING(d-preco-unit,"999999.99999"),".",""),",","")        /* |    4  | Preco Unitario                                          |    11   |    27  |    37   | Decimal  |     5    |     Sim     | */                     
                   tt-import.campos[05]      = FILL("0",011)                                                              /* |    5  | Quantidade Minima                                       |    11   |    38  |    48   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[06]      = FILL(" ",011)                                                              /* |    6  | Deixar em Branco                                        |    11   |    49  |    59   |          |          |     Nao     | */  
                   tt-import.campos[07]      = FILL("0",012)                                                              /* |    7  | Valor do Faturamento Minimo Fornecedor para o Item      |    12   |    60  |    71   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[08]      = STRING(i-mo-moeda,"99")                                                    /* |    8  | Codigo da Moeda                                         |     2   |    72  |    73   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[09]      = "S"                                                                        /* |    9  | Contrato Ativo                                          |     1   |    74  |    74   | Logico   |          |     Sim     | */  
                   tt-import.campos[10]      = STRING(c-it-codigo,"X(16)")                                                /* |   10  | Codigo do Item                                          |    16   |    75  |    90   | Caracter |          |     Sim     | */  
                   tt-import.campos[11]      = FILL("0",013)                                                              /* |   11  | Valor Total do Item no Contrato                         |    13   |    91  |   103   | Decimal  |     4    |     Sim     | */  
                   tt-import.campos[12]      = FILL(" ",008)                                                              /* |   12  | Codigo da Refer�ncia                                    |     8   |   104  |   111   | Caracter |          |     Sim     | */  
                   tt-import.campos[13]      = "N"                                                                        /* |   13  | IPI Incluso                                             |     1   |   112  |   112   | Logico   |          |     Sim     | */  
                   tt-import.campos[14]      = STRING(i-icms)                                                             /* |   14  | ICMS                                                    |     1   |   113  |   113   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[15]      = STRING(ITEM.un,"X(02)")                                                    /* |   15  | Unidade de Medida                                       |     2   |   114  |   115   | Caracter |          |     Sim     | */  
                   tt-import.campos[16]      = FILL(" ",012)                                                              /* |   16  | Contato (deixar em branco)                              |    12   |   116  |   127   | Caracter |          |     Sim     | */  
                   tt-import.campos[17]      = STRING(i-nr-sequencia,"9999")                                              /* |   17  | Sequencia do Item                                       |     4   |   128  |   131   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[18]      = "001"                                                                      /* |   18  | Frequencia                                              |     3   |   132  |   134   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[19]      = "1"                                                                        /* |   19  | Situacao:1-Nao emitido 2-Emitido 3-Cancelado 4-Atendido |     1   |   135  |   135   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[20]      = REPLACE(REPLACE(STRING(d-qt-total,"99999999999.9999"),".",""),",","")      /* |   20  | Quantidade Total                                        |    15   |   136  |   150   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[21]      = "1"                                                                        /* |   33  | Unidade do Contrato                                     |     1   |   151  |   151   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[22]      = FILL("0",013)                                                              /* |   21  | Quantidade em Saldo                                     |    13   |   152  |   164   | Decimal  |     4    |     Sim     | */  
                   tt-import.campos[23]      = FILL("0",015)                                                              /* |   22  | Valor Recebido                                          |    15   |   165  |   179   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[24]      = FILL("0",015)                                                              /* |   23  | Quantidade Recebida                                     |    15   |   180  |   194   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[25]      = STRING(i-control-preco)                                                    /* |   24  | Controle: 1-Qtd e pr 2-qtde aberta 3-Limite de cust     |     1   |   195  |   195   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[26]      = FILL("0",015)                                                              /* |   25  | Saldo em Qtde do Contrato Liberado para Recebimento     |    15   |   196  |   210   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[27]      = FILL("0",015)                                                              /* |   26  | Saldo em Valor do Contrato Liberado                     |    15   |   211  |   225   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[28]      = "N"                                                                        /* |   27  | Item Controlado por Evento  (S/N)                       |     1   |   226  |   226   | Logico   |          |     Sim     | */  
                   tt-import.campos[29]      = STRING(i-caract-item)                                                      /* |   28  | Caracteristica : 1 - Aditivo 2 - Redutor                |     1   |   227  |   227   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[30]      = "N"                                                                        /* |   29  | Valor Obrigatorio ao Contrato e Sempre Pago             |     1   |   228  |   228   | Logico   |          |     Sim     | */  
                   tt-import.campos[31]      = "N"                                                                        /* |   30  | Multa                                                   |     1   |   229  |   229   | Logico   |          |     Sim     | */  
                   tt-import.campos[32]      = replace(REPLACE(STRING(d-perc-multa,"999.99"),".",""),",","")              /* |   31  | Percentual Multa diario                                 |     5   |   230  |   234   | Decimal  |     2    |     Nao     | */  
                   tt-import.campos[33]      = replace(REPLACE(STRING(d-perc-multa-dia,"999.99"),".",""),",","")          /* |   32  | Percentual Multa Limite                                 |     5   |   235  |   239   | Decimal  |     2    |     Nao     | */  
                   tt-import.campos[34]      = STRING(ITEM.deposito-pad,"X(03)")                                          /* |   33  | Deposito                                                |     3   |   240  |   242   | Caracter |          |     Sim     | */  
                   tt-import.campos[35]      = replace(REPLACE(STRING(d-aliq-icms,"999.99"),".",""),",","")               /* |   34  | Aliquota ICMS                                           |     5   |   243  |   247   | Decimal  |     2    |     Sim     | */  
                   tt-import.campos[36]      = replace(REPLACE(STRING(d-aliq-ipi,"999.99"),".",""),",","")                /* |   35  | Aliquota IPI                                            |     5   |   248  |   252   | Decimal  |     2    |     Sim     | */  
                   tt-import.campos[37]      = replace(REPLACE(STRING(d-aliq-iss,"999.99"),".",""),",","")                /* |   36  | Aliquota ISS                                            |     5   |   253  |   257   | Decimal  |     2    |     Sim     | */  
                   tt-import.campos[38]      = STRING(i-tipo-despesa,"999")                                               /* |   37  | Tipo de Despesa                                         |     3   |   258  |   260   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[39]      = STRING(i-cond-pagto,"999")                                                 /* |   38  | Condicao de Pagamento                                   |     3   |   261  |   263   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[40]      = "N"                                                                        /* |   39  | Frete                                                   |     1   |   264  |   264   | Logico   |          |     Sim     | */  
                   tt-import.campos[41]      = STRING(c-contato,"X(40)")                                                  /* |   40  | Contato                                                 |    40   |   265  |   304   | Caracter |          |     Sim     | */  
                   tt-import.campos[42]      = STRING(contrato-for.cod-emitente,"999999999")                              /* |   41  | Codigo do Fornecedor                                    |     9   |   305  |   313   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[43]      = STRING(i-nr-sequencia,"99999")                                             /* |   42  | Sequencia do Item (Novo Formato >>>>9)                  |     5   |   314  |   318   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[44]      = FILL("0",012)                                                              /* |   43  | Saldo a receber em valor para o item                    |    12   |   319  |   330   | Decimal  |     4    |     Nao     | */  
                   tt-import.campos[45]      = "IC01"                                                                     /* |    1  | Codigo do Registro (IC01)                               |     4   |     1  |     4   | Caracter |          |     Sim     | */  
                   tt-import.campos[46]      = STRING(i-nr-contrato,"999999999") + FILL(" ",007)                          /* |    2  | Numero do Contrato                                      |    16   |     5  |    20   | Caracter |          |     Sim     | */  
                   tt-import.campos[47]      = STRING(i-nr-sequencia,"9999")                                              /* |    3  | Sequencia do Item                                       |     4   |    21  |    24   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[48]      = replace(REPLACE(STRING(d-preco-fornec,"99999999999.99999"),".",""),",","") /* |    4  | Preco Fornecedor                                        |    16   |    25  |    40   | Decimal  |     5    |     Sim     | */  
                   tt-import.campos[49]      = "N"                                                                        /* |    5  | Taxa Financeira (Incluso/Nao Incluso)                   |     1   |    41  |    41   | Logico   |          |     Sim     | */  
                   tt-import.campos[50]      = replace(REPLACE(STRING(d-vl-frete,"999999999.9999"),".",""),",","")        /* |    6  | Valor Frete                                             |    13   |    42  |    54   | Decimal  |     4    |     Sim     | */  
                   tt-import.campos[51]      = replace(REPLACE(STRING(d-vl-taxa ,"999.9999"),".",""),",","")              /* |    7  | Valor Taxa                                              |     7   |    55  |    61   | Decimal  |     4    |     Sim     | */  
                   tt-import.campos[52]      = STRING(i-prazo-entrega,"9999")                                             /* |    8  | Prazo Entrega                                           |     4   |    62  |    65   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[53]      = REPLACE(STRING(TODAY,"99/99/9999"),"/","")                                 /* |   18  | Data Preco Unitario                                     |     8   |    66  |    73   | Data     |          |     Sim     | */  
                   tt-import.campos[54]      = replace(REPLACE(STRING(d-vl-base,"99999999999.99999"),".",""),",","")      /* |    9  | Preco Base                                              |    16   |    74  |    89   | Decimal  |     5    |     Sim     | */  
                   tt-import.campos[55]      = REPLACE(REPLACE(STRING(c-cod-comprador,"X(12)"),".",""),",","")            /* |   10  | Comprador                                               |    12   |    90  |   101   | Caracter |          |     Sim     | */  
                   tt-import.campos[56]      = REPLACE(REPLACE(STRING(d-perc-desconto,"999.99"),".",""),",","")           /* |   11  | Percentual de Desconto                                  |     5   |   102  |   106   | Decimal  |     2    |     Sim     | */  
                   tt-import.campos[57]      = STRING(c-narrativa-comp,"x(76)")                                           /* |   12  | Narrativa Compra                                        |    76   |   107  |   182   | Caracter |          |     Sim     | */  
                   tt-import.campos[58]      = STRING(i-tipo-controle)                                                    /* |   13  | Controle (1 - Medicao 2 - Ordem 3 - Programacao)        |     1   |   183  |   183   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[59]      = tt-import.campos[04]                                                       /* |   14  | Preco Unitario Final                                    |    16   |   184  |   199   | Decimal  |     5    |     Sim     | */  
                   tt-import.campos[60]      = REPLACE(STRING(TODAY,"99/99/9999"),"/","")                                 /* |   19  | Data Preco Base                                         |     8   |   200  |   207   | Data     |          |     Sim     | */  
                   tt-import.campos[61]      = FILL("0",012)                                                              /* |   15  | Saldo Quantidade Recebido                               |    12   |   208  |   219   | Decimal  |     4    |     Sim     | */  
                   tt-import.campos[62]      = FILL("0",012)                                                              /* |   16  | Saldo Valor Recebido                                    |    12   |   220  |   231   | Decimal  |     4    |     Sim     | */  
                   tt-import.campos[63]      = STRING(i-num-ordem,"99999999")                                             /* |   17  | Numero Ordem                                            |     8   |   232  |   239   | Inteiro  |          |     Sim     | */  
                   tt-import.campos[64]      = STRING(c-narrativa,"X(2000)")                                              /* |   18  | Narrativa                                               |  2000   |   240  |  2239   | Caracter |          |     Nao     | */  
                   tt-import.campos[65]      = STRING(i-nr-sequencia,"99999")                                             /* |   19  | Sequencia do Item (Novo Formato >>>>9)                  |     5   |  2240  |  2244   | Inteiro  |          |     Sim     | */  
                   .                                                                                                      /* +----------------------------------------------------------------------------------------------------------------------------------+ */                     
        END.                                 

    END.

    chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
    chExcelApplication:QUIT().
    RELEASE OBJECT chExcelApplication.

    RUN pi-finalizar IN h-acomp.
END PROCEDURE.

PROCEDURE pi-export-arquivo:                                                /* |----------------------------------------------------------------------------------------------------------------------------------| */  
    OUTPUT TO VALUE(wh-cn0206-c-arquivo-entrada:SCREEN-VALUE) NO-CONVERT.   /* | Ordem |                         Descricao                       | Tamanho | Inicio | Termino | Conteudo | Decimais | Obrigatorio | */              
    FOR EACH tt-import:                                                     /* |-------+---------------------------------------------------------+---------+--------+---------+----------+----------+-------------| */              
        PUT UNFORMATTED tt-import.campos[01]      AT 001                    /* |    1  | Codigo do Registro (IC00)                               |     4   |     1  |     4   | Caracter |          |     Sim     | */
                        tt-import.campos[02]      AT 005                    /* |    2  | Deixar em branco                                        |     6   |     5  |    10   |          |          |     Sim     | */                    
                        tt-import.campos[03]      AT 011                    /* |    3  | Numero do Contrato                                      |    16   |    11  |    26   | Caracter |          |     Sim     | */              
                        tt-import.campos[04]      AT 027                    /* |    4  | Preco Unitario                                          |    11   |    27  |    37   | Decimal  |     5    |     Sim     | */
                        tt-import.campos[05]      AT 038                    /* |    5  | Quantidade Minima                                       |    11   |    38  |    48   | Decimal  |     4    |     Nao     | */         
                        tt-import.campos[06]      AT 049                    /* |    6  | Deixar em Branco                                        |    11   |    49  |    59   |          |          |     Nao     | */                    
                        tt-import.campos[07]      AT 060                    /* |    7  | Valor do Faturamento Minimo Fornecedor para o Item      |    12   |    60  |    71   | Decimal  |     4    |     Nao     | */        
                        tt-import.campos[08]      AT 072                    /* |    8  | Codigo da Moeda                                         |     2   |    72  |    73   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[09]      AT 074                    /* |    9  | Contrato Ativo                                          |     1   |    74  |    74   | Logico   |          |     Sim     | */                    
                        tt-import.campos[10]      AT 075                    /* |   10  | Codigo do Item                                          |    16   |    75  |    90   | Caracter |          |     Sim     | */              
                        tt-import.campos[11]      AT 091                    /* |   11  | Valor Total do Item no Contrato                         |    13   |    91  |   103   | Decimal  |     4    |     Sim     | */                    
                        tt-import.campos[12]      AT 104                    /* |   12  | Codigo da Refer�ncia                                    |     8   |   104  |   111   | Caracter |          |     Sim     | */                    
                        tt-import.campos[13]      AT 112                    /* |   13  | IPI Incluso                                             |     1   |   112  |   112   | Logico   |          |     Sim     | */              
                        tt-import.campos[14]      AT 113                    /* |   14  | ICMS                                                    |     1   |   113  |   113   | Inteiro  |          |     Sim     | */ 
                        tt-import.campos[15]      AT 114                    /* |   15  | Unidade de Medida                                       |     2   |   114  |   115   | Caracter |          |     Sim     | */               
                        tt-import.campos[16]      AT 116                    /* |   16  | Contato (deixar em branco)                              |    12   |   116  |   127   | Caracter |          |     Sim     | */                    
                        tt-import.campos[17]      AT 128                    /* |   17  | Sequencia do Item                                       |     4   |   128  |   131   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[18]      AT 132                    /* |   18  | Frequencia                                              |     3   |   132  |   134   | Inteiro  |          |     Sim     | */                    
                        tt-import.campos[19]      AT 135                    /* |   19  | Situacao:1-Nao emitido 2-Emitido 3-Cancelado 4-Atendido |     1   |   135  |   135   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[20]      AT 136                    /* |   20  | Quantidade Total                                        |    15   |   136  |   150   | Decimal  |     4    |     Nao     | */              
                        tt-import.campos[21]      AT 151                    /* |   33  | Unidade do Contrato                                     |     1   |   151  |   151   | Inteiro  |          |     Sim     | */                    
                        tt-import.campos[22]      AT 152                    /* |   21  | Quantidade em Saldo                                     |    13   |   152  |   164   | Decimal  |     4    |     Sim     | */                    
                        tt-import.campos[23]      AT 165                    /* |   22  | Valor Recebido                                          |    15   |   165  |   179   | Decimal  |     4    |     Nao     | */                    
                        tt-import.campos[24]      AT 180                    /* |   23  | Quantidade Recebida                                     |    15   |   180  |   194   | Decimal  |     4    |     Nao     | */                    
                        tt-import.campos[25]      AT 195                    /* |   24  | Controle: 1 - Quantidade e Preco fixos 2 - qtde aberta  |         |        |         |          |          |             | */                    
                                                                            /* |       | Preco fixo 3 - Limite de custo                          |     1   |   195  |   195   | Inteiro  |          |     Sim     | */                    
                        tt-import.campos[26]      AT 196                    /* |   25  | Saldo em Qtde do Contrato Liberado para Recebimento     |    15   |   196  |   210   | Decimal  |     4    |     Nao     | */                    
                        tt-import.campos[27]      AT 211                    /* |   26  | Saldo em Valor do Contrato Liberado                     |    15   |   211  |   225   | Decimal  |     4    |     Nao     | */                    
                        tt-import.campos[28]      AT 226                    /* |   27  | Item Controlado por Evento  (S/N)                       |     1   |   226  |   226   | Logico   |          |     Sim     | */                    
                        tt-import.campos[29]      AT 227                    /* |   28  | Caracteristica : 1 - Aditivo 2 - Redutor                |     1   |   227  |   227   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[30]      AT 228                    /* |   29  | Valor Obrigatorio ao Contrato e Sempre Pago             |     1   |   228  |   228   | Logico   |          |     Sim     | */                    
                        tt-import.campos[31]      AT 229                    /* |   30  | Multa                                                   |     1   |   229  |   229   | Logico   |          |     Sim     | */              
                        tt-import.campos[32]      AT 230                    /* |   31  | Percentual Multa diario                                 |     5   |   230  |   234   | Decimal  |     2    |     Nao     | */              
                        tt-import.campos[33]      AT 235                    /* |   32  | Percentual Multa Limite                                 |     5   |   235  |   239   | Decimal  |     2    |     Nao     | */              
                        tt-import.campos[34]      AT 240                    /* |   33  | Deposito                                                |     3   |   240  |   242   | Caracter |          |     Sim     | */                    
                        tt-import.campos[35]      AT 243                    /* |   34  | Aliquota ICMS                                           |     5   |   243  |   247   | Decimal  |     2    |     Sim     | */         
                        tt-import.campos[36]      AT 248                    /* |   35  | Aliquota IPI                                            |     5   |   248  |   252   | Decimal  |     2    |     Sim     | */              
                        tt-import.campos[37]      AT 253                    /* |   36  | Aliquota ISS                                            |     5   |   253  |   257   | Decimal  |     2    |     Sim     | */              
                        tt-import.campos[38]      AT 258                    /* |   37  | Tipo de Despesa                                         |     3   |   258  |   260   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[39]      AT 261                    /* |   38  | Condicao de Pagamento                                   |     3   |   261  |   263   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[40]      AT 264                    /* |   39  | Frete                                                   |     1   |   264  |   264   | Logico   |          |     Sim     | */              
                        tt-import.campos[41]      AT 265                    /* |   40  | Contato                                                 |    40   |   265  |   304   | Caracter |          |     Sim     | */              
                        tt-import.campos[42]      AT 305                    /* |   41  | Codigo do Fornecedor                                    |     9   |   305  |   313   | Inteiro  |          |     Sim     | */                    
                        tt-import.campos[43]      AT 314                    /* |   42  | Sequencia do Item (Novo Formato >>>>9)                  |     5   |   314  |   318   | Inteiro  |          |     Sim     | */                    
                        tt-import.campos[44]      AT 319                    /* |   43  | Saldo a receber em valor para o item                    |    12   |   319  |   330   | Decimal  |     4    |     Nao     | */   SKIP
                                                                            /* +----------------------------------------------------------------------------------------------------------------------------------+ */                    
                                                                            /* |----------------------------------------------------------------------------------------------------------------------------------| */                    
                                                                            /* | Ordem |                         Descricao                       | Tamanho | Inicio | Termino | Conteudo | Decimais | Obrigatorio | */                    
                                                                            /* |-------+---------------------------------------------------------+---------+--------+---------+----------+----------+-------------| */                    
                        tt-import.campos[45]      AT 0001                   /* |    1  | Codigo do Registro (IC01)                               |     4   |     1  |     4   | Caracter |          |     Sim     | */                    
                        tt-import.campos[46]      AT 0005                   /* |    2  | Numero do Contrato                                      |    16   |     5  |    20   | Caracter |          |     Sim     | */                    
                        tt-import.campos[47]      AT 0021                   /* |    3  | Sequencia do Item                                       |     4   |    21  |    24   | Inteiro  |          |     Sim     | */                    
                        tt-import.campos[48]      AT 0025                   /* |    4  | Preco Fornecedor                                        |    16   |    25  |    40   | Decimal  |     5    |     Sim     | */              
                        tt-import.campos[49]      AT 0041                   /* |    5  | Taxa Financeira (Incluso/Nao Incluso)                   |     1   |    41  |    41   | Logico   |          |     Sim     | */                    
                        tt-import.campos[50]      AT 0042                   /* |    6  | Valor Frete                                             |    13   |    42  |    54   | Decimal  |     4    |     Sim     | */              
                        tt-import.campos[51]      AT 0055                   /* |    7  | Valor Taxa                                              |     7   |    55  |    61   | Decimal  |     4    |     Sim     | */              
                        tt-import.campos[52]      AT 0062                   /* |    8  | Prazo Entrega                                           |     4   |    62  |    65   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[53]      AT 0066                   /* |   18  | Data Preco Unitario                                     |     8   |    66  |    73   | Data     |          |     Sim     | */                    
                        tt-import.campos[54]      AT 0074                   /* |    9  | Preco Base                                              |    16   |    74  |    89   | Decimal  |     5    |     Sim     | */              
                        tt-import.campos[55]      AT 0090                   /* |   10  | Comprador                                               |    12   |    90  |   101   | Caracter |          |     Sim     | */              
                        tt-import.campos[56]      AT 0102                   /* |   11  | Percentual de Desconto                                  |     5   |   102  |   106   | Decimal  |     2    |     Sim     | */              
                        tt-import.campos[57]      AT 0107                   /* |   12  | Narrativa Compra                                        |    76   |   107  |   182   | Caracter |          |     Sim     | */              
                        tt-import.campos[58]      AT 0183                   /* |   13  | Controle (1 - Medicao 2 - Ordem 3 - Programacao)        |     1   |   183  |   183   | Inteiro  |          |     Sim     | */                    
                        tt-import.campos[59]      AT 0184                   /* |   14  | Preco Unitario Final                                    |    16   |   184  |   199   | Decimal  |     5    |     Sim     | */                    
                        tt-import.campos[60]      AT 0200                   /* |   19  | Data Preco Base                                         |     8   |   200  |   207   | Data     |          |     Sim     | */                    
                        tt-import.campos[61]      AT 0208                   /* |   15  | Saldo Quantidade Recebido                               |    12   |   208  |   219   | Decimal  |     4    |     Sim     | */                    
                        tt-import.campos[62]      AT 0220                   /* |   16  | Saldo Valor Recebido                                    |    12   |   220  |   231   | Decimal  |     4    |     Sim     | */                    
                        tt-import.campos[63]      AT 0232                   /* |   17  | Numero Ordem                                            |     8   |   232  |   239   | Inteiro  |          |     Sim     | */              
                        tt-import.campos[64]      AT 0240                   /* |   18  | Narrativa                                               |  2000   |   240  |  2239   | Caracter |          |     Nao     | */              
                        tt-import.campos[65]      AT 2240                   /* |   19  | Sequencia do Item (Novo Formato >>>>9)                  |     5   |  2240  |  2244   | Inteiro  |          |     Sim     | */                    
                                                                            /* +----------------------------------------------------------------------------------------------------------------------------------+ */ SKIP.
    END.
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE pi-ajusta-campos:
    FIND FIRST param-contrat NO-LOCK NO-ERROR.

    FOR EACH tt-import:

        IF CAN-FIND(FIRST anexo-contrat WHERE
                          anexo-contrat.nr-contrato =  tt-import.nr-contrato) THEN NEXT.

        FIND FIRST anexo-contrat WHERE 
                   anexo-contrat.nr-contrato   = tt-import.nr-contrato             AND
                   anexo-contrat.num-seq-anexo = param-contrat.num-seq-anexo-item  EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL anexo-contrat THEN
        DO:
           CREATE anexo-contrat.
           ASSIGN anexo-contrat.nr-contrato   = tt-import.nr-contrato            
                  anexo-contrat.num-seq-anexo = param-contrat.num-seq-anexo-item 
                  anexo-contrat.des-anexo     = "Itens do Contrato"                  
                  anexo-contrat.dat-revisao   = TODAY.
        END.        
    END.
END PROCEDURE.
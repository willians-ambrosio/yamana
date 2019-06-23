&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ymof0114RP 1.00.00.000}

/* Chamada a include do gerenciador de licenªas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <mΩdulo>:  Informar qual o mΩdulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <mΩdulo>}
&ENDIF

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.
DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    FIELD c-item-i         AS CHAR FORMAT "x(16)"
    FIELD c-item-f         AS CHAR FORMAT "x(16)"
    FIELD c-ncm-i          AS CHAR 
    FIELD c-ncm-f          AS CHAR
    FIELD c-cfa-i          AS CHAR
    FIELD c-cfa-f          AS CHAR
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD rpw              AS LOG.
    

define temp-table tt-digita
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

/*Trocar a geraªío para CSV*/
DEFINE TEMP-TABLE tt-excel NO-UNDO
    FIELD celula AS CHAR EXTENT 2000.


def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp         as handle no-undo.    

form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
    tt-param.arquivo    no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PAR∂METROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÄ«O * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESS«O * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usu†rio * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

/* find empresa                                     */
/*     where empresa.ep-codigo = v_cdn_empres_usuar */
/*     no-lock no-error.                            */
find first param-global no-lock no-error.

{utp/ut-liter.i titulo_sistema * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000".


def var chExcelApp  AS COM-HANDLE no-undo.
def var chWorkbook  AS COM-HANDLE no-undo.
def var chWorksheet AS COM-HANDLE no-undo.
def var cRange      as char       no-undo.
def var i-linha     AS INT INIT 4 no-undo.

DEF VAR c-arq-temp AS CHAR        NO-UNDO.
DEF VAR c-logo     AS CHAR        NO-UNDO.

{utp/ut-glob.i}

DEF TEMP-TABLE tt-es-mov-ext-cfa LIKE es-movto-ext-item-cfa.

DEF BUFFER  b-es-movto-cfa  FOR es-movto-ext-item-cfa.

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 4.71
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

do on stop undo, leave:
    /* {include/i-rpout.i}    */
    /* view frame f-cabec.    */
    /* view frame f-rodape.   */
    run utp/ut-acomp.p persistent set h-acomp.  
    
/*     {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c} */
        
    /*:T --- Colocar aqui o c¢digo de impress∆o --- */
    /* for each [TABELA] no-lock            */
    /*     where [WHERE].                   */

    ASSIGN c-arq-temp = SESSION:TEMP-DIRECTORY + "YMOF0114-" + string(TIME) + ".csv".   

    OUTPUT TO value(c-arq-temp) NO-CONVERT.

    RUN pi-inicializar in h-acomp (input "Processando...":U). 
    RUN pi-processa.
/*         RUN pi-cria-cab-excel. */
    RUN pi-inicializar in h-acomp (input "Descarregando...":U). 
    RUN pi-cria-excel.

    OUTPUT CLOSE.

    RUN pi-cria-arq-excel.


/*         RUN pi-finaliza-handle. */



/*     end. */
    
    run pi-finalizar in h-acomp.
/*     {include/i-rpclo.i} */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-cria-arq-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-arq-excel Procedure 
PROCEDURE pi-cria-arq-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*trocando por CSV*/


/*     PUT UNFORMATTED             */
/*         "ITEM X CFA" SKIP       */
/*                      SKIP       */
/*         "Empresa"     ";"       */
/*         "Cod.Estab"   ";"       */
/*         "Item"        ";"       */
/*         "Descriªío"   ";"       */
/*         "CFA"         ";"       */
/*         "Desc. CFA"   ";"       */
/*         "Pergunta 1"  ";"       */
/*         "Resposta 1"  ";"       */
/*         "Pergunta 2"  ";"       */
/*         "Resposta 2"  ";"       */
/*         "Pergunta 3"  ";"       */
/*         "Resposta 3"  ";"       */
/*         "Pergunta 4"  ";"       */
/*         "Resposta 4"  ";"       */
/*         "Pergunta 5"  ";"       */
/*         "Resposta 5"  ";"       */
/*         "Pergunta 6"  ";"       */
/*         "Resposta 6"  ";"       */
/*         "Pergunta 7"  ";"       */
/*         "Resposta 7"  ";"       */
/*         "Pergunta 8"  ";"       */
/*         "Resposta 8"  ";"       */
/*         "Pergunta 9"  ";"       */
/*         "Resposta 9"  ";"       */
/*         "Pergunta 10" ";"       */
/*         "Resposta 10" ";" SKIP. */


/*                                                                                                              */
/* hworkbooks      = excelAPP:workbooks:ADD.                                  /* Abre normal */                 */
/* /*chWorkSheet = excelAPP:WorkBooks:Add("C:\" + "leo.xlt").              */  /* Chama modelo */               */
/* /*chworkbooks = excelAPP:Sheets:Add                                     */  /* Adiciona modelo como pasta */ */
/* /*chworkbooks = excelAPP:Sheets:Add(,,,m-caminho + "mod\renr223.xlt").  */  /* Adiciona modelo como pasta */ */
/*                                                                                                              */




CREATE "Excel.Application" chExcelApp.
    chExcelApp:VISIBLE = FALSE.
    chWorkbook = chExcelApp:Workbooks:Add(c-arq-temp).
    
    chExcelApp:ActiveWindow:Zoom = 90.
    cRange = "A1:Z1".
/*     chExcelApp:SELECTION:MergeCells = YES.     */
/*     chExcelApp:Range(cRange):FONT:Bold = YES.  */
/*     chExcelApp:Range(cRange):Merge.            */

    chExcelApp:Range(cRange):FONT:NAME = "Courier New".
    chExcelApp:Range(cRange):FONT:SIZE = "11".
    chExcelApp:Range(cRange):HorizontalAlignment = 3.

    chExcelApp:Range("A1:Z1"):FONT:Bold = YES.
    chExcelApp:Range("A1:Z1"):FONT:NAME = "Arial".
    chExcelApp:Range("A1:Z1"):FONT:SIZE = "9".
    chExcelApp:Range("A1:Z1"):HorizontalAlignment = 3.
    chExcelApp:Range("A1:Z1"):AutoFilter(,,,).
    chExcelApp:Cells:EntireColumn:AutoFit.

    chExcelApp:ActiveWindow:SplitColumn = 0.
    chExcelApp:ActiveWindow:SplitRow    = 1.
    chExcelApp:ActiveWindow:FreezePanes = YES.

    chExcelApp:Range("1:8"):INSERT().

    chExcelApp:Range("A2"):SELECT.

    FILE-INFO:file-name = "modelos/logoYamana.png".
    c-logo = FILE-INFO:FULL-PATHNAME.

    chExcelApp:ActiveSheet:pictures:Insert(c-logo).

    chExcelApp:Range("B5"):FONT:Bold = YES.
    chExcelApp:Range("B5"):VALUE = "Empresa:".
    chExcelApp:Range("B5"):HorizontalAlignment = 4.
    chExcelApp:Range("C5"):VALUE = c-empresa.
    chExcelApp:Range("B6"):FONT:Bold = YES.
    chExcelApp:Range("B6"):VALUE = "RelatΩrio:".
    chExcelApp:Range("B6"):HorizontalAlignment = 4.
    chExcelApp:Range("C6"):VALUE = "Item x CFA (Datasul - YMOF0114)".
    chExcelApp:Range("B7"):FONT:Bold = YES.
    chExcelApp:Range("B7"):VALUE = "Data de Extraªío:".
    chExcelApp:Range("B7"):HorizontalAlignment = 4.
    chExcelApp:Range("C7"):VALUE = STRING(TODAY,"99/99/9999").
    chExcelApp:Columns("C:C"):ColumnWidth = 11.60.
    chExcelApp:Range("A8"):SELECT.

    IF tt-param.destino = 3 THEN DO:
        
        ASSIGN c-arq-temp = REPLACE(c-arq-temp,".csv",".xlsx" ).
        chExcelApp:ActiveWorkbook:SaveAs(c-arq-temp,,,,,,). 
        chExcelApp:VISIBLE = TRUE.

    END.
    IF tt-param.destino = 2 THEN DO:


        IF tt-param.rpw = NO THEN DO:
        
            ASSIGN c-arquivo = REPLACE(tt-param.arquivo,"/","\").
            chExcelApp:ActiveWorkbook:SaveAs(c-arquivo,51,,,,,). 
            chExcelApp:VISIBLE = FALSE.
             chExcelApp:quit().
        END.
        ELSE DO:



        END.
    END.

    

    
   /*
    chExcelApp:AlertBeforeOverwriting = NO.
    chWorkSheet = chExcelApp:Sheets:Item(1).
    chWorkSheet:name = "Extraªío Item".
    chExcelApp:ActiveWindow:Zoom = 90.

    cRange = "A1:O1".
    chExcelApp:SELECTION:MergeCells = YES.
    chExcelApp:Range(cRange):FONT:bold = YES.
    chExcelApp:Range(cRange):Merge.
    chWorkSheet:Range(cRange):value = "ITEM X CFA".
    chExcelApp:Range(cRange):FONT:NAME = "Courier New".
    chExcelApp:Range(cRange):FONT:SIZE = "11".
    chExcelApp:Range(cRange):HorizontalAlignment = 3.

    /* ------------------------------------------------------------------------------------------------------------------------- */

    chWorkSheet:Range("A3"):value = "Empresa".
    chWorkSheet:Range("B3"):value = "Cod.Estab".
    chWorkSheet:Range("C3"):value = "Item".
    chWorkSheet:Range("DF3"):value = "Descriªío".
    chWorkSheet:Range("E3"):value = "CFA".
    chWorkSheet:Range("F3"):value = "Desc. CFA".
    chWorkSheet:Range("G3"):value = "Pergunta 1".
    chWorkSheet:Range("H3"):value = "Resposta 1".
    chWorkSheet:Range("I3"):value = "Pergunta 2". 
    chWorkSheet:Range("J3"):value = "Resposta 2". 
    chWorkSheet:Range("K3"):value = "Pergunta 3". 
    chWorkSheet:Range("L3"):value = "Resposta 3". 
    chWorkSheet:Range("M3"):value = "Pergunta 4". 
    chWorkSheet:Range("N3"):value = "Resposta 4". 
    chWorkSheet:Range("O3"):value = "Pergunta 5". 
    chWorkSheet:Range("P3"):value = "Resposta 5". 
    chWorkSheet:Range("Q3"):value = "Pergunta 6".   
    chWorkSheet:Range("R3"):value = "Resposta 6".  
    chWorkSheet:Range("S3"):value = "Pergunta 7".   
    chWorkSheet:Range("T3"):value = "Resposta 7".   
    chWorkSheet:Range("U3"):value = "Pergunta 8".   
    chWorkSheet:Range("V3"):value = "Resposta 8".   
    chWorkSheet:Range("W3"):value = "Pergunta 9". 
    chWorkSheet:Range("X3"):value = "Resposta 9". 
    chWorkSheet:Range("Y3"):value = "Pergunta 10". 
    chWorkSheet:Range("Z3"):value = "Resposta 10" .


    chExcelApp:Range("A3:Z3"):FONT:bold = YES.
    chExcelApp:Range("A3:Z3"):FONT:NAME = "Arial".
    chExcelApp:Range("A3:Z3"):FONT:SIZE = "9".
    chExcelApp:Range("A3:Z3"):HorizontalAlignment = 3.
    chExcelApp:Range("A3:Z3"):AutoFilter(,,,).

    chExcelApp:ActiveWindow:SplitColumn = 0.
    chExcelApp:ActiveWindow:SplitRow    = 3.
    chExcelApp:ActiveWindow:FreezePanes = YES.

  */

    IF VALID-HANDLE(chExcelApp)  THEN RELEASE OBJECT chExcelApp.
    IF VALID-HANDLE(chWorkbook)  THEN RELEASE OBJECT chWorkbook.
    IF VALID-HANDLE(chWorkSheet) THEN RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-excel Procedure 
PROCEDURE pi-cria-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR c-desc-item  AS CHAR        NO-UNDO.
DEF VAR c-perg-resp  AS CHAR        NO-UNDO.
DEF VAR i-coluna     AS INT         NO-UNDO.
DEF VAR i-coluna-ini AS INT  INIT 6 NO-UNDO.
DEF VAR c-coluna1    AS CHAR 
    INIT "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar as,at,au,av,aw,ax,ay,az" NO-UNDO.
DEF VAR c-coluna     AS CHAR        NO-UNDO.                                                                             
DEF VAR i-maior-col  AS INT         NO-UNDO.

ASSIGN i-linha = 4.
/* MESSAGE "monta Excel "                 */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


/*Trocar a geraªío para CSV*/
/* DEFINE TEMP-TABLE tt-excel NO-UNDO  */
/*     FIELD celula AS CHAR EXTENT 50. */

FOR EACH tt-es-mov-ext-cfa BREAK 
    BY tt-es-mov-ext-cfa.ep-codigo
    BY tt-es-mov-ext-cfa.it-codigo
    BY tt-es-mov-ext-cfa.identificador
    BY tt-es-mov-ext-cfa.nr-seq:

    run pi-acompanhar in h-acomp (input string(i-maior-col) + " item EXCEL ===> " +  tt-es-mov-ext-cfa.it-codigo).
   
    IF FIRST-OF(tt-es-mov-ext-cfa.it-codigo) THEN DO:
   
        CREATE tt-excel.

        ASSIGN tt-excel.celula[1] = tt-es-mov-ext-cfa.ep-codigo  /*chWorksheet:range("a" + STRING(i-linha)):VALUE = tt-es-mov-ext-cfa.ep-codigo.*/
              /* tt-excel.celula[2] = tt-es-mov-ext-cfa.cod-estabel */
/*         chWorksheet:range("b" + STRING(i-linha)):VALUE = tt-es-mov-ext-cfa.cod-estabel. */
               tt-excel.celula[3] = tt-es-mov-ext-cfa.it-codigo.

/*         chWorksheet:range("c" + STRING(i-linha)):NumberFormat = "@".                  */
/*         chWorksheet:range("c" + STRING(i-linha)):VALUE = tt-es-mov-ext-cfa.it-codigo. */               
              
        FIND item WHERE item.it-codigo = trim(tt-es-mov-ext-cfa.it-codigo) NO-LOCK NO-ERROR.
        IF AVAIL item THEN DO:
            ASSIGN c-desc-item = item.desc-item.
        END.
        ELSE DO:
            ASSIGN c-desc-item = "".
        END.

        ASSIGN c-desc-item = REPLACE(c-desc-item,CHR(10), " ")
               c-desc-item = REPLACE(c-desc-item,CHR(11), " ")
               c-desc-item = REPLACE(c-desc-item,CHR(12), " ")
               c-desc-item = REPLACE(c-desc-item,CHR(13), " ").
               tt-excel.celula[4] = c-desc-item  .             /*chWorksheet:range("d" + STRING(i-linha)):VALUE = c-desc-item.*/
               tt-excel.celula[5] = tt-es-mov-ext-cfa.classe . /*chWorksheet:range("e" + STRING(i-linha)):VALUE = tt-es-mov-ext-cfa.classe.*/

        FIND es-cfa WHERE es-cfa.classe = tt-es-mov-ext-cfa.classe NO-LOCK NO-ERROR.

        tt-excel.celula[6] = IF AVAIL es-cfa THEN es-cfa.descricao  ELSE "". /*chWorksheet:range("f" + STRING(i-linha)):VALUE = IF AVAIL es-cfa THEN es-cfa.descricao  ELSE "".*/

        ASSIGN i-coluna = i-coluna-ini
               i-coluna = i-coluna + 1
               c-coluna = ENTRY(i-coluna,c-coluna1,",")
               c-perg-resp = tt-es-mov-ext-cfa.pergunta-literal
               c-perg-resp = REPLACE(c-perg-resp,CHR(10), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(11), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(12), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(13), " ").

        IF c-perg-resp = ? THEN
            ASSIGN c-perg-resp = "".

        ASSIGN tt-excel.celula[i-coluna] = c-perg-resp /* tt-es-mov-ext-cfa.pergunta-literal */  /*chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = tt-es-mov-ext-cfa.pergunta-literal.*/
               i-coluna = i-coluna + 1
               c-coluna = ENTRY(i-coluna,c-coluna1,",")
               c-perg-resp = tt-es-mov-ext-cfa.resposta-literal
               c-perg-resp = REPLACE(c-perg-resp,CHR(10), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(11), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(12), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(13), " ").

        IF c-perg-resp = ? THEN
            ASSIGN c-perg-resp = "".

        ASSIGN tt-excel.celula[i-coluna] = c-perg-resp /* tt-es-mov-ext-cfa.resposta-literal */ . /*chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = tt-es-mov-ext-cfa.resposta-literal.*/
               i-coluna = i-coluna + 1.
   
    END.
    ELSE DO:

/*         c-coluna = ENTRY(i-coluna,c-coluna1,","). */

        ASSIGN c-perg-resp = tt-es-mov-ext-cfa.pergunta-literal
               c-perg-resp = REPLACE(c-perg-resp,CHR(10), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(11), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(12), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(13), " ").

        IF c-perg-resp = ? THEN
            ASSIGN c-perg-resp = "".

        ASSIGN tt-excel.celula[i-coluna] = c-perg-resp /* tt-es-mov-ext-cfa.pergunta-literal */  /* chWorksheet:cells( i-linha,i-coluna):VALUE = tt-es-mov-ext-cfa.pergunta-literal.*/
               i-coluna = i-coluna + 1
               c-perg-resp = tt-es-mov-ext-cfa.resposta-literal
               c-perg-resp = REPLACE(c-perg-resp,CHR(10), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(11), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(12), " ")
               c-perg-resp = REPLACE(c-perg-resp,CHR(13), " ").

        IF c-perg-resp = ? THEN
            ASSIGN c-perg-resp = "".

/*         c-coluna = ENTRY(i-coluna,c-coluna1,","). */
        ASSIGN tt-excel.celula[i-coluna] =  c-perg-resp /* tt-es-mov-ext-cfa.resposta-literal */ .  /*chWorksheet:cells(i-linha,i-coluna):VALUE = tt-es-mov-ext-cfa.resposta-literal.*/
               i-coluna = i-coluna + 1.
    END.

    IF LAST-OF(tt-es-mov-ext-cfa.it-codigo) THEN DO:

        IF i-maior-col < i-coluna THEN
            ASSIGN i-maior-col = i-coluna.

        ASSIGN i-linha  = i-linha + 1.
               i-coluna = 1. 

    END.
   
END.

DEF VAR i-col-csv AS INT     NO-UNDO.

PUT UNFORMATTED
    "Empresa"     ";"  
    "Cod.Estab"   ";"  
    "Item"        ";"  
    "Descriªío"   ";"
    "CFA"         ";"  
    "Desc.CFA"    ";"  
    "Pergunta 1"  ";"
    "Resposta 1"  ";"
    "Pergunta 2"  ";"
    "Resposta 2"  ";"
    "Pergunta 3"  ";"
    "Resposta 3"  ";"
    "Pergunta 4"  ";"
    "Resposta 4"  ";"
    "Pergunta 5"  ";"
    "Resposta 5"  ";"
    "Pergunta 6"  ";"
    "Resposta 6"  ";"
    "Pergunta 7"  ";"
    "Resposta 7"  ";"
    "Pergunta 8"  ";"
    "Resposta 8"  ";"
    "Pergunta 9"  ";"
    "Resposta 9"  ";"
    "Pergunta 10" ";"
    "Resposta 10" ";" 
    SKIP.

ASSIGN i-col-csv = 1.
FOR EACH tt-excel:

    DO WHILE i-col-csv <= 50:

        run pi-acompanhar in h-acomp (input "Arquivo ===> " + tt-excel.celula[3] +  STRING(i-col-csv)).

        IF i-col-csv >= 30 AND 
            tt-excel.celula[i-col-csv] = "" THEN LEAVE.

        PUT UNFORMATTED tt-excel.celula[i-col-csv] ";".

        ASSIGN i-col-csv = i-col-csv + 1.

    END.

    PUT SKIP.

    ASSIGN i-col-csv = 1. 

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-finaliza-handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-finaliza-handle Procedure 
PROCEDURE pi-finaliza-handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN c-arquivo = REPLACE(tt-param.arquivo,"/","\").


  IF tt-param.destino = 2 THEN DO:

      run pi-acompanhar in h-acomp (input "Save EXCEL ===> " +  c-arquivo).

      
      chExcelApp:ActiveWorkbook:SaveAs(c-arquivo,,,,,,).
/*        chWorkSheet:SaveAs:Filename("C:\temp\yamana\salvo.xlsx"). */
/*        chWorkSheet:ActiveWorkbook:SaveAs("C:\temp\yamana\salvo.xlsx",1,"","",no,no,no) . */
         chExcelApp:quit(). 

            


  END.
  ELSE
      chExcelApp:VISIBLE = YES.

    IF VALID-HANDLE(chExcelApp)  THEN RELEASE OBJECT chExcelApp.
    IF VALID-HANDLE(chWorkbook)  THEN RELEASE OBJECT chWorkbook.
    IF VALID-HANDLE(chWorkSheet) THEN RELEASE OBJECT chWorksheet.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa Procedure 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE  i-conta2 AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.


    FOR EACH ext-item-cfa NO-LOCK WHERE ext-item-cfa.classe     >= tt-param.c-cfa-i
                                  AND   ext-item-cfa.classe     <= tt-param.c-cfa-f
                                  AND   ext-item-cfa.it-codigo  >= tt-param.c-item-i
                                  AND   ext-item-cfa.it-codigo  <= tt-param.c-item-f
                                  AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario:

        FIND ITEM WHERE ITEM.it-codigo = ext-item-cfa.it-codigo NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN NEXT.

        ASSIGN i-conta2 = 0.
     
        run pi-acompanhar in h-acomp (input "Item===> " + ITEM.it-codigo).

        FOR EACH es-movto-ext-item-cfa NO-LOCK WHERE es-movto-ext-item-cfa.it-codigo = ITEM.it-codigo
                                               AND   es-movto-ext-item-cfa.ep-codigo  = ext-item-cfa.ep-codigo  BY es-movto-ext-item-cfa.identificador DESC:
                 
             ASSIGN i-conta2 = i-conta2 + 1.

             IF i-conta2 = 1 THEN DO:
               
                 FOR EACH b-es-movto-cfa WHERE b-es-movto-cfa.it-codigo     = es-movto-ext-item-cfa.it-codigo                  
                                         AND   b-es-movto-cfa.ep-codigo     = es-movto-ext-item-cfa.ep-codigo                  
                                         AND   b-es-movto-cfa.identificador = es-movto-ext-item-cfa.identificador NO-LOCK: 

                     CREATE tt-es-mov-ext-cfa.                                                                                 
                     BUFFER-COPY b-es-movto-cfa TO tt-es-mov-ext-cfa.     
                     ASSIGN i-cont = i-cont + 1.
                 END.
             END.
             ELSE LEAVE.
        END.
                  
           
            
END.









END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



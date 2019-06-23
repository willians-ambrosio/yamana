/*******************************************************************************
**   PROGRAMA.: PRGINT/UFN/ESUFN003RP.P
**   OBJETIVO.: Exportar / Importar Tipo de Fluxo Financeiro do Fornecedor
**   AUTOR....: Helder Prado Bustamante - DSC Praxis
**   EMPRESA..: 
**   DATA.....: 07/11/2017
             
*******************************************************************************/

ASSIGN CURRENT-LANGUAGE = CURRENT-LANGUAGE.
/* include para controle de versao */
{include/i-prgvrs.i ESUFN003 0.12.00.000}

/* include padrao para variaveis de relatorio */
{include/i-rpvar.i}

/************ [ DEFINIÄÂES DE variaveis] ******************************/
DEF VAR h-acomp             AS HANDLE       NO-UNDO.
DEF VAR i-linha             AS INT          NO-UNDO.
DEF VAR c-linha             AS CHAR         NO-UNDO FORMAT 'x(10000)'.
DEF VAR chExcelApplication  AS COM-HANDLE   NO-UNDO.
DEF VAR chworkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chworksheet         AS COM-HANDLE   NO-UNDO.

DEF TEMP-TABLE tt-param

    FIELD destino            AS INTEGER
    FIELD arquivo            AS CHAR FORMAT "x(35)"
    FIELD usuario            AS CHAR FORMAT "x(12)"
    FIELD data-exec          AS DATE
    FIELD hora-exec          AS INTEGER
    FIELD classifica         AS INTEGER
    FIELD desc-classifica    AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf         AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf      AS LOG
    FIELD rs-execucao        AS INT
    FIELD acao               AS INT
    FIELD cod_grp_fornec-ini AS CHAR FORMAT " x(4)"
    FIELD cod_grp_fornec-fim AS CHAR FORMAT " x(4)"
    FIELD cdn_fornecedor-ini AS INT FORMAT ">>>,>>>,>>9"
    FIELD cdn_fornecedor-fim AS INT FORMAT ">>>,>>>,>>9"
    FIELD arq-import         AS CHAR.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

def new global shared var v_cod_empres_usuar as CHARACTER format "x(3)":U label "Empresa" column-label "Empresa" no-undo.

DEF VAR v_cod_empresa    LIKE fornec_financ.cod_empresa    NO-UNDO.
DEF VAR v_cdn_fornecedor LIKE fornec_financ.cdn_fornecedor NO-UNDO.
DEF VAR l-importa        AS LOGICAL                        NO-UNDO.
DEF VAR c-arq-xls        AS CHAR                           NO-UNDO.
DEF VAR c-arq-csv        AS CHAR                           NO-UNDO.
DEF VAR c-extensao       AS CHAR                           NO-UNDO.
DEF VAR c-delimiter      AS CHAR                           NO-UNDO.
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param no-lock no-error.
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST  mguni.empresa NO-LOCK
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-ERROR.
/*
/************ [ MAIN-BLOCK ] ******************************/
ASSIGN c-programa     = "ESUN003"
       c-versao       = "0.12"
       c-revisao      = ".00.000"
       c-empresa      = empresa.nome
       c-titulo-relat = "Exportaá∆o/Importaá∆o Fluxo Financeiro do Fornecedor".
*/

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Processando...").
/*
{include/i-rpcab.i}
{include/I-rpout.i}  

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.
*/
 
IF tt-param.acao = 1 THEN
   RUN pi-exporta-fluxo-finaceiro.
ELSE
   RUN pi-importa-fluxo-finaceiro.

run pi-finalizar in h-acomp.

 /* {include/i-rpclo.i}  /*&STREAM="stream str-rp"*/ */

RETURN "OK".


PROCEDURE pi-exporta-fluxo-finaceiro:

    /* Abre o Excel */
    RUN pi-iniciar.

    RUN pi-inicializar IN h-acomp(INPUT "Processando...").

    FOR EACH  emsuni.fornecedor WHERE /*fornecedor.cod_empresa     = v_cod_empres_usuar 
                               AND*/ fornecedor.cdn_fornecedor >= tt-param.cdn_fornecedor-ini
                               AND fornecedor.cdn_fornecedor <= tt-param.cdn_fornecedor-fim
                               AND fornecedor.cod_grp_fornec >= tt-param.cod_grp_fornec-ini
                               AND fornecedor.cod_grp_fornec <= tt-param.cod_grp_fornec-fim NO-LOCK:

        RUN pi-acompanhar IN h-acomp(input "Fornecedor : " + STRING(fornecedor.cdn_fornecedor)).  

        FIND fornec_financ WHERE fornec_financ.cod_empresa    = fornecedor.cod_empresa
                             AND fornec_financ.cdn_fornecedor = fornecedor.cdn_fornecedor no-lock no-error.

        ASSIGN i-linha = i-linha + 1.

        ASSIGN chExcelApplication:range("A" + STRING(i-linha)):NumberFormat = "@"
               chExcelApplication:range("E" + STRING(i-linha)):NumberFormat = "@".

        ASSIGN chExcelApplication:range("A" + STRING(i-linha)):VALUE = fornecedor.cod_empresa
               chExcelApplication:range("B" + STRING(i-linha)):VALUE = fornecedor.cod_grp_fornec
               chExcelApplication:range("C" + STRING(i-linha)):VALUE = fornecedor.cdn_fornecedor
               chExcelApplication:range("D" + STRING(i-linha)):VALUE = fornecedor.nom_pessoa
               chExcelApplication:range("E" + STRING(i-linha)):VALUE = IF AVAIL fornec_financ THEN fornec_financ.cod_tip_fluxo_financ ELSE "".

    END.

    /* Fecha o Excel */
    RUN pi-fechar.


END PROCEDURE.

PROCEDURE pi-importa-fluxo-finaceiro:

    ASSIGN c-arq-xls = tt-param.arq-import
           c-arq-xls = REPLACE(c-arq-xls,"/","\")
           c-arq-csv = "".

    RUN pi-converte-xls-em-csv.

    ASSIGN i-linha = 0.
    OUTPUT TO VALUE(ENTRY(1,c-arq-xls,".") + '.txt').
    INPUT FROM VALUE(c-arq-csv) NO-CONVERT.

    REPEAT:

        IMPORT UNFORMATTED c-linha.

        RUN pi-acompanhar IN h-acomp(input "Linha : " + c-linha). 

        ASSIGN i-linha   = i-linha + 1
               l-importa = YES.

        IF i-linha = 1 THEN NEXT.

        IF NUM-ENTRIES(c-linha,";") > 1  THEN
            ASSIGN c-delimiter = ';'.
        ELSE
            ASSIGN c-delimiter = ','.


        IF NUM-ENTRIES(c-linha,c-delimiter) <> 5  THEN DO: 
             PUT UNFORMATTED "Linha (" + STRING(i-linha) + ") n∆o esta com a estrutura de importaá∆o esperada : " c-linha SKIP.
    
             NEXT.
        END.

        ASSIGN v_cod_empresa = ENTRY(1,c-linha,c-delimiter) NO-ERROR.

        IF v_cod_empresa = "" THEN NEXT.

        ASSIGN v_cdn_fornecedor = INT(ENTRY(3,c-linha,c-delimiter)) NO-ERROR.

        IF ERROR-STATUS:ERROR = YES THEN
        DO:
           PUT UNFORMATTED "Linha (" + STRING(i-linha) + ") C¢digo do Fornecedor n∆o Ç um numero : " c-linha SKIP.

           ASSIGN l-importa = NO.
        END.

        FIND emsuni.fornecedor WHERE fornecedor.cod_empresa    = v_cod_empresa 
                                 AND fornecedor.cdn_fornecedor = v_cdn_fornecedor NO-LOCK NO-ERROR.
        IF NOT AVAIL fornecedor THEN
        DO:
            PUT UNFORMATTED "Linha (" + STRING(i-linha) + ") Fornecedor n∆o cadastrado : " c-linha SKIP.

            ASSIGN l-importa = NO.
        END.


        FIND tip_fluxo_financ WHERE tip_fluxo_financ.cod_tip_fluxo_financ = ENTRY(5,c-linha,c-delimiter) NO-LOCK NO-ERROR.

        PAUSE 1.

        IF NOT AVAIL tip_fluxo_financ OR ENTRY(5,c-linha,c-delimiter) = "" THEN
        DO:
 
            PUT UNFORMATTED "Linha (" + STRING(i-linha) + ") Tipo de Fluxo Financeiro n∆o cadastrado : " c-linha SKIP.
       
            ASSIGN l-importa = NO.
        END.

        FIND fornec_financ WHERE fornec_financ.cod_empresa    = v_cod_empresa 
                             AND fornec_financ.cdn_fornecedor = v_cdn_fornecedor EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL fornec_financ THEN
        DO:
            PUT UNFORMATTED "Linha (" + STRING(i-linha) + ") Fornecedor Financeiro n∆o cadastrado : " c-linha SKIP.

            ASSIGN l-importa = NO.
        END.

        IF l-importa = NO THEN NEXT.
 
        ASSIGN fornec_financ.cod_tip_fluxo_financ = ENTRY(5,c-linha,c-delimiter).

        PUT UNFORMATTED "Linha (" + STRING(i-linha) + ") Alterado com Sucesso! : " c-linha SKIP.

    END.

    INPUT CLOSE.
    OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE pi-converte-xls-em-csv:


        /* Abre excel */
        ASSIGN c-extensao = ENTRY(2,c-arq-xls,".")
               c-arq-csv  = replace(c-arq-xls,c-extensao,"csv").

        OS-DELETE VALUE(c-arq-csv).

        CREATE "excel.application" chExcelApplication.
        chExcelApplication:VISIBLE = FALSE.

        chExcelApplication:workbooks:OPEN(c-arq-xls).
        chExcelApplication:worksheets:ITEM(1):SELECT.

        chExcelApplication:APPLICATION:DisplayAlerts = TRUE.


        chExcelApplication:ActiveWorkbook:SaveAs(c-arq-csv, 6,"","",NO,NO,NO).


        chExcelApplication:ActiveWorkbook:Save.
        chExcelApplication:ActiveWindow:Close(FALSE).

        RELEASE OBJECT chExcelApplication NO-ERROR.
        RELEASE OBJECT chworkbook         NO-ERROR.
        RELEASE OBJECT chworksheet        NO-ERROR.



END PROCEDURE.


PROCEDURE pi-iniciar:

    CREATE "Excel.Application" chExcelApplication.
    ASSIGN chworkBook  = chExcelApplication:Workbooks:ADD()
           chworkSheet = chExcelApplication:sheets:ITEM(1)
           chExcelApplication:DisplayAlerts  = NO
           chExcelApplication:VISIBLE        = NO.
    
    ASSIGN i-linha = 1.

    ASSIGN chExcelApplication:range("A" + STRING(i-linha)):VALUE = "Empresa"
           chExcelApplication:range("B" + STRING(i-linha)):VALUE = "Grupo Fornec."
           chExcelApplication:range("C" + STRING(i-linha)):VALUE = "Fornecedor"
           chExcelApplication:range("D" + STRING(i-linha)):VALUE = "Raz∆o Social"
           chExcelApplication:range("E" + STRING(i-linha)):VALUE = "Tipo Fluxo Financeiro".

END PROCEDURE.

PROCEDURE pi-fechar:

    RUN pi-acompanhar IN h-acomp (INPUT "Imprimindo RelatΩrio").

    chExcelApplication:Cells:Select.
    chExcelApplication:Cells:EntireColumn:AutoFit.

    chExcelApplication:Range("A3"):SELECT.

    chExcelApplication:VISIBLE = TRUE.

    RELEASE OBJECT chExcelApplication NO-ERROR.
    RELEASE OBJECT chworkbook         NO-ERROR.
    RELEASE OBJECT chworksheet        NO-ERROR.

END PROCEDURE.



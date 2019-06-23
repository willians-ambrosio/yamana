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
{include/i-prgvrs.i YMOF0119 1.00.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    FIELD arquivo          AS CHAR
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.
 
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
{utp/ut-liter.i PARÂMETROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÇÃO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÃO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usuário * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

/* find ems2cadme.empresa                           */
/*     where empresa.ep-codigo = v_cdn_empres_usuar */
/*     no-lock no-error.                            */
find first param-global no-lock no-error.

{utp/ut-liter.i titulo_sistema * }
assign c-sistema = "TOTVS11".
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = "Carga de Familias".
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.


DEF STREAM str-rp.

DEFINE VARIABLE chExcelapp  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorkbook  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE      NO-UNDO.
DEF SHARED VAR c-seg-usuario AS CHAR FORMAT "x(16)" NO-UNDO.

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{include/i-rpcab.i }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


 
 OUTPUT TO  value(tt-param.arq-destino) .


/*                                                                */
 do on stop undo, leave:                                     
/*     {include/i-rpout.i } */
    view   frame f-cabec.                                     
     VIEW   frame f-rodape.                                   
     run utp/ut-acomp.p persistent set h-acomp.                 
/*                                                                */
/*                                                                */
/*                                                                */
/*                                                                */
/*                                                                */
/*                                                                */
    {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}                  
/*                                                                */
    run pi-inicializar in h-acomp (input "Processando...":U). 
/*                                                                */
/*                                                                */
    RUN pi-abre-excel.
    RUN pi-carrega-planilha.
/*                                                                */
/*                                                                */
/*    run pi-acompanhar in h-acomp (input "xxxxxxxxxxxxxx":U).    */
/*                                                                */
/*                                                                */
      run pi-finalizar in h-acomp.                               
/*     {include/i-rpclo.i} */
 OUTPUT CLOSE.
 end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-abre-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel Procedure 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE "Excel.Application" chExcelapp.
chExcelapp:VISIBLE = FALSE.
chWorkbook         = chExcelapp:Workbooks:ADD(tt-param.arq-entrada) .
chWorksheet        = chExcelapp:Sheets:ITEM(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-carrega-planilha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-planilha Procedure 
PROCEDURE pi-carrega-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DEFINE VARIABLE i-linha    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-formato  AS INTE   NO-UNDO.

DEFINE VARIABLE i-conta009 AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-teste    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-conta    AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-planilha AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-um       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-fam      AS CHARACTER   NO-UNDO.

i-linha = 2.
DO i-conta = 1 TO 10000.

   
    c-planilha  = chWorksheet:range("a" + STRING(i-linha)):VALUE.

    IF c-planilha = "" OR  c-planilha = ? THEN LEAVE.
    c-um = chWorksheet:range("c" + STRING(i-linha)):VALUE.
    FIND TAB-unidade WHERE TAB-unidade.un = c-um NO-LOCK NO-ERROR.
    IF NOT AVAIL TAB-unidade THEN DO:
       PUT UNFORMAT  "Linha => " + STRING(i-linha) + " Erro " + "unidade de medida " + c-um +  "  nÆo cadastrada"  SKIP.
       i-linha = i-linha + 1 .
       NEXT.
    END.

    c-fam = chWorksheet:range("a" + STRING(i-linha)):VALUE.

    FIND familia WHERE familia.fm-codigo = c-fam NO-LOCK NO-ERROR.
    IF AVAIL familia THEN DO:
         PUT UNFORMAT  "Linha => " + STRING(i-linha) + " Erro " + "familia " + c-fam + " " +  "ja cadastrada "    SKIP.
         i-linha = i-linha + 1 .
         NEXT.
    END.
      
    CREATE familia.
    ASSIGN familia.fm-codigo = chWorksheet:range("a" + STRING(i-linha)):VALUE.
           familia.descricao = SUBSTR(chWorksheet:range("b" + STRING(i-linha)):VALUE,1,30).
           familia.un        = chWorksheet:range("c" + STRING(i-linha)):VALUE.

    run pi-acompanhar in h-acomp (INPUT c-fam  ). 
           
    i-linha = i-linha + 1 . 
 
END.


RUN pi-fecha-planilha.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-fecha-planilha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fecha-planilha Procedure 
PROCEDURE pi-fecha-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-nomearq AS CHARACTER   NO-UNDO.


    c-nomearq = "c:\temp\ymof0119rp" +  ".xlsx".  
     chExcelapp:workbooks:item(1):SaveAs(c-nomearq,,,,,,,).   
     chExcelapp:workbooks:APPLICATION:QUIT.
     OS-COMMAND SILENT DEL value(c-nomearq) . 

    if  valid-handle( chExcelapp) then release object chExcelapp.       
    if  valid-handle( chWorkbook) then release object chWorkbook.       
    if  valid-handle( chWorksheet) then release object chWorksheet.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


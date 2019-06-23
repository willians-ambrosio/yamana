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
{include/i-prgvrs.i XX9999RP 9.99.99.999}

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

DEFINE VARIABLE chExcelapp  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorkbook  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE i-linha-ini    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-coluna-ini   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-qt-perguntas AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod2 AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-es-mov-ext-item-cfa LIKE es-movto-ext-item-cfa.



/* define temp-table tt-param                             */
/*     field destino            as integer                */
/*     field arquivo            as char format "x(50)":U  */
/*     field usuario            as char format "x(12)":U  */
/*     field data-exec          as date                   */
/*     field hora-exec          as integer                */
/*     field classifica         as integer                */
/*     field desc-classifica    as char format "x(40)":U. */

define temp-table tt-param
    field destino          as integer
    field arquivo          as char format "x(50)":U 
    field arq-destino      as char
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
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.

DEF BUFFER b-tt-es-mov-ext-item-cfa FOR tt-es-mov-ext-item-cfa.


DEFINE VARIABLE l-existe AS LOGICAL     NO-UNDO.


DEF TEMP-TABLE tt-1
   FIELD TAB   AS CHAR
   FIELD cod   AS INT
   FIELD DEScr AS CHAR FORMAT "x(40)".

 DEF SHARED VAR c-seg-usuario AS CHAR FORMAT "x(16)" NO-UNDO.

 DEFINE VARIABLE c-nomearq AS CHARACTER FORMAT "x(35)"  NO-UNDO.

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
         HEIGHT             = 2.08
         WIDTH              = 39.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/* {include/i-rpcab.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


do on stop undo, leave:

    
  /*     {include/i-rpout.i}       */
  /*   view frame f-cabec.         */
  /*   view frame f-rodape.        */
    
   run utp/ut-acomp.p persistent set h-acomp.                     
                                                                  
   {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}                       
                                                                    
  run pi-inicializar in h-acomp (input "Processando...":U).         

    
   
       RUN pi-carrega-combos.
       RUN pi-carrega-planilha.

      

        

    
      run pi-finalizar in h-acomp.  
/*      {include/i-rpclo.i} */


 MESSAGE "Carga de Itens Finalizada!!!"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.


end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-abre-planilha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-planilha Procedure 
PROCEDURE pi-abre-planilha :
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

&IF DEFINED(EXCLUDE-pi-carrega-combos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-combos Procedure 
PROCEDURE pi-carrega-combos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chExcelapp     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorkbook     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorksheet    AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE i-linha-ini    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-coluna-ini   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-qt-perguntas AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-tab          AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-coluna       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-linha        AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-dados        AS CHARACTER  FORMAT "x(40)"  NO-UNDO.





DEFINE VARIABLE i-conta AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-coluna1   AS CHARACTER INIT "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z"  NO-UNDO.



DEFINE VARIABLE c-arquivo-entrada AS CHARACTER  INIT "d:\temp\calccfa.xlsm"  NO-UNDO.

CREATE "Excel.Application" chExcelapp.
chExcelapp:VISIBLE = FALSE. 
chWorkbook         = chExcelapp:Workbooks:ADD(tt-param.arq-entrada) .
chWorksheet        = chExcelapp:Sheets:ITEM(3). 

IF chExcelapp:Sheets:COUNT() < 4 THEN DO:

    MESSAGE "<4"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Planilha de carga nÆo possue o layout correto favor verificar!!!" ).

    
    QUIT.
END.



IF chExcelapp:Sheets:ITEM(3):NAME <> "questionario" THEN DO:

    MESSAGE "questionario"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


    run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Planilha de carga nÆo possui o layout correto favor verificar!!!" ).

    
    QUIT.
END.



















i-conta = 0 .
 DO i-conta = 1 TO 20:
    i-tab = i-tab + 1.
    c-coluna = ENTRY(i-conta,c-coluna1,",").

     
      DO i-linha = 5 TO 200:
         c-coluna = ENTRY(i-conta + 1,c-coluna1,",").
         IF chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE = "" OR chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE = ? THEN LEAVE.
          c-coluna = ENTRY(i-conta,c-coluna1,",").

         CREATE tt-1.                                                                    
                ASSIGN tt-1.cod   = int(chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE).
                c-coluna = ENTRY(i-conta + 1,c-coluna1,",").
                       c-dados = chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE.
                       tt-1.descr = c-dados.  
                       tt-1.TAB   = string(i-tab).


      END.
      i-conta = i-conta + 1.
      

 END.

c-nomearq = "c:\temp\ymof0107" + c-seg-usuario + ".xlsx".

OS-COMMAND SILENT DEL VALUE(c-nomearq).
chExcelapp:workbooks:item(1):SaveAs(c-nomearq,,,,,,,).  
chExcelapp:workbooks:APPLICATION:QUIT.

RELEASE OBJECT  chExcelapp  NO-ERROR. 
RELEASE OBJECT  chWorkbook  NO-ERROR. 
RELEASE OBJECT  chWorksheet NO-ERROR.







 

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


DEFINE VARIABLE i-linha    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-coluna   AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-coluna   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-coluna1   AS CHARACTER  INIT "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj"  NO-UNDO.
DEFINE VARIABLE c-data     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-contaq   AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-chancela AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-seq      AS INT         NO-UNDO.
DEFINE VARIABLE i-conta    AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-hora     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-formato  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arquivo  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE I-ident    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-conta2   AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-formato  AS INTE   NO-UNDO.

DEFINE VARIABLE i-conta009 AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-teste    AS INTEGER     NO-UNDO.

ASSIGN c-arquivo = "C:\TEMP\LOGYMOF0110-" + STRING(TODAY,"99999999") +   ".txt".

OUTPUT TO  VALUE(c-arquivo) NO-CONVERT.


RUN pi-abre-planilha.
RUN pi-le-param-plan.


ASSIGN i-linha  = 2.
       i-coluna = 1.
c-coluna = ENTRY(i-coluna,c-coluna1,",").

ASSIGN c-data = STRING(TODAY,"99/99/9999").


i-contaq = 1.


DO i-contaq = 1 TO 1000000.

  
    c-chancela = chWorksheet:range("a" + STRING(i-linha)):VALUE.
    
   IF c-chancela = "" OR  c-chancela = ? THEN LEAVE.
       
        
    i-seq = 0.
    i-coluna = 7.
    i-qt-perguntas = 10.
    i-qt-perguntas = i-coluna + i-qt-perguntas.



    DO i-conta = i-coluna TO i-qt-perguntas:

         
        IF i-conta = i-qt-perguntas THEN LEAVE.

         i-seq = i-seq + 1.
         c-hora   = STRING(TIME,"hh:mm").
         c-coluna = ENTRY(i-conta,c-coluna1,",").

         c-formato = "".

         c-cod2 = ENTRY(1,chWorksheet:range("d" + STRING(i-linha)):VALUE,",").

         run pi-acompanhar in h-acomp (INPUT c-cod2).
        
        CREATE tt-es-mov-ext-item-cfa.
        ASSIGN tt-es-mov-ext-item-cfa.ep-codigo        = ENTRY(1,chWorksheet:range("b" + STRING(i-linha)):VALUE,",").
               tt-es-mov-ext-item-cfa.classe           = entry(1,chWorksheet:range("f" + STRING(i-linha)):VALUE,",").
/*                tt-es-mov-ext-item-cfa.cod-estabel      = entry(1,chWorksheet:range("c" + STRING(i-linha)):VALUE,","). */
               c-formato = chWorksheet:range( c-coluna  + STRING(1)):VALUE.
               tt-es-mov-ext-item-cfa.pergunta-literal =  c-formato.
               tt-es-mov-ext-item-cfa.dt-movto         = date(c-data).
               tt-es-mov-ext-item-cfa.hr-movto         = c-hora.
               tt-es-mov-ext-item-cfa.identificador    = 1.
               tt-es-mov-ext-item-cfa.it-codigo        = entry(1,chWorksheet:range("d" + STRING(i-linha)):VALUE,",").
               tt-es-mov-ext-item-cfa.nr-seq           = i-seq.
               chWorksheet:range(c-coluna  + STRING(i-linha)):NumberFormat = "@".
               i-formato = INT(chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                   c-formato = chWorksheet:range(c-coluna  + STRING(i-linha)):VALUE.
               END.
               ELSE DO:
                    c-formato = STRING(i-formato).
               END.
               tt-es-mov-ext-item-cfa.resposta-literal = c-formato.
               tt-es-mov-ext-item-cfa.usuario          =  chWorksheet:range("a" + STRING(i-linha)):VALUE.
    END.
    i-linha = i-linha + 1 . 
 
END.

I-ident = 0.


FOR EACH tt-es-mov-ext-item-cfa:

    i-teste = INT(tt-es-mov-ext-item-cfa.resposta) NO-ERROR.
    IF i-teste = ? OR i-teste = 0 THEN NEXT.


    FIND tt-1 WHERE tt-1.TAB = string(tt-es-mov-ext-item-cfa.nr-seq)
              AND   tt-1.cod = int(tt-es-mov-ext-item-cfa.resposta) NO-LOCK NO-ERROR.

    IF AVAIL tt-1 THEN DO:
          ASSIGN tt-es-mov-ext-item-cfa.resposta = tt-es-mov-ext-item-cfa.resposta + "|" + tt-1.DESCr.
    END.












END.


















FOR EACH tt-es-mov-ext-item-cfa  BREAK BY tt-es-mov-ext-item-cfa.ep-codigo BY tt-es-mov-ext-item-cfa.it-codigo:


    run pi-acompanhar in h-acomp (INPUT tt-es-mov-ext-item-cfa.it-codigo).

    
    IF FIRST-OF(tt-es-mov-ext-item-cfa.ep-codigo) AND FIRST-OF(tt-es-mov-ext-item-cfa.it-codigo) THEN DO:

       
        i-conta2 = 0.
        FOR EACH es-movto-ext-item-cfa  WHERE es-movto-ext-item-cfa.it-codigo   = tt-es-mov-ext-item-cfa.it-codigo 
                                        AND   es-movto-ext-item-cfa.ep-codigo   = tt-es-mov-ext-item-cfa.ep-codigo  BY  es-movto-ext-item-cfa.identificador DESC  BY es-movto-ext-item-cfa.nr-seq :
         
            ASSIGN i-conta2 = i-conta2 + 1.
            IF i-conta2 = 1 THEN DO:
                 ASSIGN I-ident = es-movto-ext-item-cfa.identificador + 1.
                 LEAVE.
            END.
        END.
        IF I-ident = 0 THEN I-ident = 1.
    END.

   ASSIGN l-existe = FALSE.  
   /*  RUN esp\conectaemp.p (INPUT  tt-es-mov-ext-item-cfa.ep-codigo,       */
   /*                        INPUT  tt-es-mov-ext-item-cfa.it-codigo,       */
   /*                        OUTPUT l-existe  ).                            */

   ASSIGN l-existe = TRUE.



    IF l-existe = FALSE  THEN DO:
        FIND es-de-para-it-padr WHERE es-de-para-it-padr.it-codigo-orig = tt-es-mov-ext-item-cfa.it-codigo 
                                       AND   es-de-para-it-padr.ep-codigo = tt-es-mov-ext-item-cfa.ep-codigo NO-LOCK NO-ERROR.

        IF AVAIL es-de-para-it-padr  THEN DO:
             ASSIGN tt-es-mov-ext-item-cfa.it-codigo = es-de-para-it-padr.it-codigo-padr.
        END.
        ELSE DO:

            PUT UNFORMAT "ITEM => "  + tt-es-mov-ext-item-cfa.it-codigo  + " NÇO CADASTRADO" SKIP.
            NEXT.

        END.
    END.
    
    FIND es-cfa WHERE es-cfa.classe = tt-es-mov-ext-item-cfa.classe NO-LOCK NO-ERROR.
    IF NOT AVAIL es-cfa THEN DO:
       PUT UNFORMAT "CFA => "  + tt-es-mov-ext-item-cfa.classe + " NÇO CADASTRADA" SKIP.
       NEXT.
    END.
     
    /* FIND estabelec WHERE estabelec.cod-estabel = tt-es-mov-ext-item-cfa.cod-estabel NO-LOCK NO-ERROR.                */
    /* IF NOT AVAIL estabelec THEN DO:                                                                                  */
    /*    PUT UNFORMAT "Estabelecimento =>   "  + STRING(tt-es-mov-ext-item-cfa.cod-estabel) + " NÇO CADASTRADO" SKIP.  */
    /*    NEXT.                                                                                                         */
    /* END.                                                                                                             */
    /*                                                                                                                  */
    FIND ems2cadme.empresa WHERE ems2cadme.empresa.ep-codigo = tt-es-mov-ext-item-cfa.ep-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL ems2cadme.empresa THEN DO:

       PUT UNFORMAT "Empresa => "  + tt-es-mov-ext-item-cfa.ep-codigo + " NÇO CADASTRADA" SKIP. 

        NEXT.
    END.

    
      FIND ext-item-cfa WHERE ext-item-cfa.it-codigo   = tt-es-mov-ext-item-cfa.it-codigo 
                        AND   ext-item-cfa.ep-codigo   = tt-es-mov-ext-item-cfa.ep-codigo
                        EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL ext-item-cfa  THEN DO:
          ASSIGN ext-item-cfa.classe = tt-es-mov-ext-item-cfa.classe.
      END.
      ELSE DO:
          CREATE ext-item-cfa.
          ASSIGN ext-item-cfa.it-codigo   = tt-es-mov-ext-item-cfa.it-codigo 
                 ext-item-cfa.ep-codigo   = tt-es-mov-ext-item-cfa.ep-codigo.
                 
      END.
      
      CREATE es-movto-ext-item-cfa.
      BUFFER-COPY tt-es-mov-ext-item-cfa  EXCEPT tt-es-mov-ext-item-cfa.identificador TO es-movto-ext-item-cfa.
      ASSIGN es-movto-ext-item-cfa.identificador = I-ident.

END.





     c-nomearq = "c:\temp\ymof0107" + c-seg-usuario + ".xlsx".
     OS-COMMAND SILENT DEL VALUE(c-nomearq) .
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

&IF DEFINED(EXCLUDE-pi-le-param-plan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-le-param-plan Procedure 
PROCEDURE pi-le-param-plan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
i-linha-ini    = 2.
i-coluna-ini   = 7.
i-qt-perguntas = 10.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-valida-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-item Procedure 
PROCEDURE pi-valida-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/






END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


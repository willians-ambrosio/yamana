&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
DATA        : 23/12/2008
Autor       : Wellington Aparecido - WSA
Empresa     : DSC
Atualizacao :001
Notas       : Feita corre‡Æo no tratameto de variavel do valor de imposto 
              e impressao do CGC quando pessoa fisica.

*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i ESDES001RP 2.06.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

/* ***************************  Definitions  ************************** */

/*   ##### DEFINICAO DE TEMP-TABLE #####   */ 
    define temp-table tt-param no-undo
        field destino          as integer
        field arquivo          as char format "x(35)"
        field usuario          as char format "x(12)"
        field data-exec        as date
        field hora-exec        as integer
        FIELD emitente-ini     LIKE emitente.cod-emitente
        FIELD emitente-fim     LIKE emitente.cod-emitente
        FIELD serie-ini        LIKE nota-fiscal.serie
        FIELD serie-fim        LIKE nota-fiscal.serie
        FIELD nro-docto-ini    LIKE docum-est.nro-docto
        FIELD nro-docto-fim    LIKE docum-est.nro-docto
        FIELD nat-operacao-ini LIKE docum-est.nat-operacao
        FIELD nat-operacao-fim LIKE docum-est.nat-operacao
        FIELD tipo-relat       AS INT
        FIELD dt-emis-ini      AS DATE
        FIELD dt-emis-fim      AS DATE.

def temp-table tt-raw-digita
    field raw-digita      as raw.

def var chexcelapplication as com-handle.
def var chworkbook         as com-handle.
def var chworksheet        as com-handle.
def var c-range as char.
def var linha   as int.
DEF VAR d-vl-imposto AS DEC.


DEF VARIABLE c-nr-docto       AS CHAR FORMAT "x(7)"     NO-UNDO.
DEF VARIABLE i-nr-docto       AS INT FORMAT  "9999999" NO-UNDO.


/*   ##### DEFINICAO DE PARAMETROS DO PROGRAMA #####   */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*   ##### PREPARACAO DOS PARAMETROS #####   */
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.
                    
/*   ##### DEFINICAO DE STREAM #####   */
DEF STREAM s-saida.

/*   ##### INCLUDES PARA O RELATORIO #####   */
{include/i-rpvar.i}

/*{include/i-rpcab.i}*/
 

/*   ##### DEFINICAO DE VARIAVEIS LOCAIS #####   */
DEF VAR h-acomp             AS HANDLE       NO-UNDO.

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
   Type: Procedure
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
         HEIGHT             = 5.75
         WIDTH              = 31.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND FIRST param-global NO-LOCK NO-ERROR.

FIND FIRST empresa 
     WHERE empresa.ep-codigo = param-global.empresa-prin 
           NO-LOCK NO-ERROR.
/*
ASSIGN c-programa     = "ESDES001rp"
       c-versao       = "2.04"
       c-revisao      = ".00.000"
       c-empresa      = empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "Gera DES". 
 */
/*----- DIRECIONAMENTO DO RELATORIO -----*/
/*{include/i-rpout.i PAGE-SIZE 0} */
OUTPUT TO VALUE(tt-param.arquivo) PAGE-SIZE 0.

/*
VIEW FRAME f-cabec.
VIEW FRAME f-rodape.
 */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp ("Aguarde processando ...").

/*     #####     Chamada para logica do PROGRAMA     ######     */

IF tt-param.tipo-relat = 1 THEN
    
    RUN pi-executar-relat.

ELSE 

    RUN pi-executar-des.

{include/i-rpclo.i}

RUN pi-finalizar IN h-acomp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-executar-des) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar-des Procedure 
PROCEDURE pi-executar-des :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE de-AUX-VL-BASE LIKE es-gera-des.tot-valor   NO-UNDO.
DEFINE VARIABLE c-aux-cgc      AS CHARACTER FORMAT "x(14)"  NO-UNDO.

FOR EACH es-gera-des NO-LOCK
  WHERE es-gera-des.cod-emitente  >= tt-param.emitente-ini    
  AND es-gera-des.cod-emitente  <= tt-param.emitente-fim    
  AND es-gera-des.serie-docto   >= tt-param.serie-ini       
  AND es-gera-des.serie-docto   <= tt-param.serie-fim       
  AND es-gera-des.nro-docto     >= tt-param.nro-docto-ini   
  AND es-gera-des.nro-docto     <= tt-param.nro-docto-fim
  AND es-gera-des.dt-trans      >= tt-param.dt-emis-ini
  AND es-gera-des.dt-trans      <= tt-param.dt-emis-fim :

  RUN pi-acompanhar IN h-acomp ("Item:" + STRING(es-gera-des.cod-emitente)).  

  FIND FIRST emitente NO-LOCK 
    WHERE emitente.cod-emitente = es-gera-des.cod-emitente NO-ERROR. 
  IF NOT AVAIL emitente THEN NEXT.

  FIND FIRST estabelec NO-LOCK
    WHERE estabelec.cod-estabel = es-gera-des.cod-estabel NO-ERROR.

  ASSIGN DE-AUX-VL-BASE = 0.

  FIND FIRST ext-tipo-tax NO-LOCK
    WHERE ext-tipo-tax.cod-tax = INT(es-gera-des.cod-tax) NO-ERROR.
  IF AVAIL mgesp.ext-tipo-tax AND mgesp.ext-tipo-tax.gera-des = YES THEN DO:
      
      ASSIGN DE-AUX-VL-BASE = es-gera-des.tot-valor .                       
  END.

  ASSIGN c-aux-cgc = "".

  IF LENGTH(emitente.cgc)  = 11 THEN
    ASSIGN c-aux-cgc = "000" + STRING(emitente.cgc, "99999999999999").
  ELSE
    ASSIGN c-aux-cgc = STRING(emitente.cgc, "99999999999999").

  
  PUT 
    estabelec.cgc                  FORMAT "X(14)"
    es-gera-des.tipo-docto         FORMAT "x(5)"
    es-gera-des.serie              FORMAT "x(5)"
    es-gera-des.nro-docto          FORMAT "99999999"
    es-gera-des.dt-trans         
    replace(string(es-gera-des.tot-valor,"9999999999999.99"),",","") FORMAT "999999999999999" 
    int(es-gera-des.cod-tax )      FORMAT "99999"
    replace(string(DE-AUX-VL-BASE,"9999999999999.99"),",","") FORMAT "999999999999999" 
    c-aux-cgc                      FORMAT "99999999999999"
    emitente.estado                FORMAT "X(2)"
    es-gera-de.cidade              FORMAT "x(50)"
    FILL(" ", 200)                 FORMAT "x(200)"  SKIP.

end.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-executar-relat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar-relat Procedure 
PROCEDURE pi-executar-relat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:DES.
chExcelApplication:Visible = true.

RUN pi-acompanhar IN h-acomp("Abrindo o excel...").

ASSIGN chexcelapplication:range("a1"):value = "CNPJ YAMANA"
       chexcelapplication:range("b1"):value = "TP.DOCTO"
       chexcelapplication:range("c1"):value = "SERIE"
       chexcelapplication:range("d1"):value = "NRO.DOCTO"
       chexcelapplication:range("e1"):value = "DT.EMISSAO"
       chexcelapplication:range("f1"):value = "VL.DOCTO"
       chexcelapplication:range("g1"):value = "COD.SERV"
       chexcelapplication:range("h1"):value = "VL.BASE.CALC"
       chexcelapplication:range("i1"):value = "VL.IMP"
       chexcelapplication:range("j1"):value = "ESP"
       chexcelapplication:range("k1"):value = "NAT.OPER"
   /*    chexcelapplication:range("l1"):value = "P" */
       chexcelapplication:range("l1"):value = "EMITENTE"
       chexcelapplication:range("m1"):value = "CNPJ"
       chexcelapplication:range("n1"):value = "UF"
       chexcelapplication:range("o1"):value = "MUNICIPIO"
       chexcelapplication:Range("a1:o1"):Font:Bold = TRUE.

assign linha = 2.

/* Fava - Kraft Consulting - 20/05/10 */
chexcelapplication:range("a:a"):numberformat   = "@".
chexcelapplication:range("b:b"):numberformat   = "@".
chexcelapplication:range("c:c"):numberformat   = "@".
chexcelapplication:range("d:d"):numberformat   = "@".
chexcelapplication:range("e:e"):numberformat   = "@".
chexcelapplication:range("j:j"):numberformat   = "@".
chexcelapplication:range("k:k"):numberformat   = "@".
chexcelapplication:range("l:l"):numberformat   = "@".
chexcelapplication:range("m:m"):numberformat   = "@".
chexcelapplication:range("n:n"):numberformat   = "@".
chexcelapplication:range("o:o"):numberformat   = "@".
/*chexcelapplication:range("p:p"):numberformat   = "@".*/
/* Fava - Kraft Consulting - 20/05/10 */

FOR EACH es-gera-des NO-LOCK
   WHERE es-gera-des.cod-emitente  >= tt-param.emitente-ini    
     AND es-gera-des.cod-emitente  <= tt-param.emitente-fim    
     AND es-gera-des.serie-docto   >= tt-param.serie-ini       
     AND es-gera-des.serie-docto   <= tt-param.serie-fim       
     AND es-gera-des.nro-docto     >= tt-param.nro-docto-ini   
     AND es-gera-des.nro-docto     <= tt-param.nro-docto-fim
     AND es-gera-des.dt-trans      >= tt-param.dt-emis-ini
     AND es-gera-des.dt-trans      <= tt-param.dt-emis-fim:

  RUN pi-acompanhar IN h-acomp ("Item:" + STRING(es-gera-des.cod-emitente)).  

  ASSIGN 
     c-nr-docto     = es-gera-des.nro-docto  
     i-nr-docto     = int(c-nr-docto)
     c-nr-docto     = STRING(i-nr-docto,"9999999").

  FIND FIRST emitente NO-LOCK 
       WHERE emitente.cod-emitente = es-gera-des.cod-emitente NO-ERROR. 
       IF NOT AVAIL emitente THEN NEXT.

  FIND FIRST estabelec NO-LOCK
       WHERE estabelec.cod-estabel = es-gera-des.cod-estabel NO-ERROR.

  ASSIGN d-vl-imposto = 0.

  FOR EACH dupli-imp NO-LOCK
     WHERE dupli-imp.cod-emitente = es-gera-des.cod-emitente
       AND dupli-imp.serie        = es-gera-des.serie
       AND dupli-imp.nro-docto    = c-nr-docto
       AND dupli-imp.nat-operacao = es-gera-des.nat-operacao :

    FIND FIRST mgesp.ext-tipo-tax NO-LOCK
         WHERE mgesp.ext-tipo-tax.cod-tax = dupli-imp.cod-reten NO-ERROR.
      IF AVAIL mgesp.ext-tipo-tax 
           AND mgesp.ext-tipo-tax.gera-des = YES THEN DO:

        ASSIGN d-vl-imposto = dupli-imp.vl-imposto.
    end.
  end.

      ASSIGN chexcelapplication:range("a"+ string(linha)) = estabelec.cgc
           /*  chexcelapplication:range("a"+ string(linha)):numberformat = "00000000000000" */ /* Fava - Kraft Consulting - 20/05/10 */
             chexcelapplication:range("b"+ string(linha)) = es-gera-des.tipo-docto
             chexcelapplication:range("c"+ string(linha)) = CAPS(es-gera-des.serie)
             chexcelapplication:range("d"+ string(linha)) = es-gera-des.nro-docto
             chexcelapplication:range("e"+ string(linha)) = STRING(es-gera-des.dt-trans, '99/99/9999')
             chexcelapplication:range("f"+ string(linha)) = es-gera-des.tot-valor
             chexcelapplication:range("g"+ string(linha)) = es-gera-des.cod-tax
             chexcelapplication:range("h"+ string(linha)) = es-gera-des.tot-valor-base
             chexcelapplication:range("i"+ string(linha)) = d-vl-imposto
             chexcelapplication:range("j"+ string(linha)) = es-gera-des.cod-esp
             chexcelapplication:range("k"+ string(linha)) = es-gera-des.nat-operacao
          /*   chexcelapplication:range("l"+ string(linha)) = es-gera-des.parcela */
             chexcelapplication:range("l"+ string(linha)) = es-gera-des.cod-emitente
             chexcelapplication:range("m"+ string(linha)) = emitente.cgc
        /*     chexcelapplication:range("n"+ string(linha)):numberformat = "00000000000000" */ /* Fava - Kraft Consulting - 20/05/10 */
             chexcelapplication:range("n"+ string(linha)) = emitente.estado
             chexcelapplication:range("o"+ string(linha)) = es-gera-des.cidade
             linha = linha + 1.   

   run pi-acompanhar in h-acomp("Imprimindo " + string(es-gera-des.nro-docto)).

END. 

/* Fava - Kraft Consulting - 20/05/10 */
chexcelapplication:range("A1:O1"):SELECT.
chexcelapplication:Range("A1:O1"):Font:Bold = TRUE.
chexcelapplication:Range("A1:O1"):FONT:COLOR = "-2".
chexcelapplication:range("A1:O1"):interior:colorindex = 11.
chexcelapplication:ActiveWindow:Zoom = 85.
chexcelapplication:range("A2"):SELECT.
chexcelapplication:ActiveWindow:FreezePanes = True.  
chexcelapplication:range("A1:O1"):AutoFilter(,,,).
chexcelapplication:Cells:EntireColumn:AutoFit.
/* Fava - Kraft Consulting - 20/05/10 */

RELEASE OBJECT chExcelApplication.     
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


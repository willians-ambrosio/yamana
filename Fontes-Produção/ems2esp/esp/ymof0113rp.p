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


DEF BUFFER ccusto FOR ems5.ccusto.

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.
def var c-destino                    as character format "x(15)":U.


def var chExcelApp  AS COM-HANDLE no-undo.
def var chWorkbook  AS COM-HANDLE no-undo.
def var chWorksheet AS COM-HANDLE no-undo.
def var cRange      as char       no-undo.
def var i-linha     AS INT INIT 4       no-undo.





define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    FIELD estab-i          AS CHAR
    FIELD estab-f          AS CHAR
    FIELD item-i           AS CHAR FORMAT "x(16)"
    FIELD item-f           AS CHAR FORMAT "x(16)"
    FIELD grp-i            AS INT
    FIELD grp-f            AS INT
    FIELD ccusto-i         AS CHAR
    FIELD ccusto-f         AS CHAR
    FIELD periodo-i        AS DATE
    FIELD periodo-f        AS DATE
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    FIELD t-req            AS LOG
    FIELD t-dev            AS LOG
    FIELD t-all            AS LOG
    field l-habilitaRtf    as LOG.

define temp-table tt-digita no-undo
     FIELD it-codigo AS CHAR FORMAT "x(16)"
     FIELD un        AS CHAR FORMAT "x(2)"
     FIELD descricao AS CHAR FORMAT "x(40)".

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
/* {utp/ut-liter.i PARÂMETROS * r}                                    */
/* assign c-liter-par = return-value.                                 */
/* {utp/ut-liter.i SELEÇÃO * r}                                       */
/* assign c-liter-sel = return-value.                                 */
/* {utp/ut-liter.i IMPRESSÃO * r}                                     */
/* assign c-liter-imp = return-value.                                 */
/* {utp/ut-liter.i Destino * l}                                       */
/* assign c-destino:label in frame f-impressao = return-value.        */
/* {utp/ut-liter.i Usuário * l}                                       */
/* assign tt-param.usuario:label in frame f-impressao = return-value. */
/*fim-traducao*/

/* {include/i-rpvar.i} */

/* find empresa                                     */
/*     where empresa.ep-codigo = v_cdn_empres_usuar */
/*     no-lock no-error.                            */
find first param-global no-lock no-error.

/* {utp/ut-liter.i titulo_sistema * }                              */
/* assign c-sistema = return-value.                                */
/* {utp/ut-liter.i titulo_relatorio * }                            */
/* assign c-titulo-relat = return-value.                           */
/* assign c-empresa     = param-global.grupo                       */
/*        c-programa    = "{&programa}":U                          */
/*        c-versao      = "1.00":U                                 */
/*        c-revisao     = "000"                                    */
/*        c-destino     = {varinc/var00002.i 04 tt-param.destino}. */




DEF TEMP-TABLE tt-it-estorno
    FIELD it-codigo AS CHAR FORMAT "x(16)"
    FIELD estado    AS CHAR FORMAT "x(2)".


DEF TEMP-TABLE tt-es-cc-estorno-cred LIKE es-cc-uf-estorno-cred.



DEF TEMP-TABLE tt-fisc-excel
    FIELD dt-docto             LIKE  it-doc-fisc.dt-docto
    FIELD nr-doc-fis           LIKE  it-doc-fisc.nr-doc-fis
    FIELD it-codigo            LIKE  it-doc-fisc.it-codigo
    FIELD desc-item            LIKE ITEM.desc-item
    FIELD vl-tot-item          LIKE   it-doc-fisc.vl-tot-item 
    FIELD vl-bicms-it          LIKE   it-doc-fisc.vl-bicms-it
    FIELD aliquota-icm         LIKE   it-doc-fisc.aliquota-icm 
    FIELD vl-icms-it           LIKE   it-doc-fisc.vl-icms-it
    FIELD val-base-calc-pis    LIKE   it-doc-fisc.val-base-calc-pis 
    FIELD char-2               LIKE   it-doc-fisc.char-2
    FIELD val-pis              LIKE   it-doc-fisc.val-pis
    FIELD val-base-calc-cofins LIKE   it-doc-fisc.val-base-calc-cofins 
    FIELD char-confis          LIKE   it-doc-fisc.char-2
    FIELD val-cofins           LIKE   it-doc-fisc.val-cofins .


DEFINE VARIABLE i-conta-doc     AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-conta-consumo AS INTEGER     NO-UNDO.


DEF TEMP-TABLE tt-it-mov-consumo
   FIELD ct-codigo LIKE movto-estoq.ct-codigo
   FIELD c-titulo  LIKE conta-contab.titulo
   FIELD sc-codigo LIKE movto-estoq.sc-codigo  
   FIELD descricao LIKE ems2cadme.ccusto.descricao
   FIELD cod-estabel LIKE  movto-estoq.cod-estabel 
   FIELD dt-trans    LIKE  movto-estoq.dt-trans
   FIELD espdocto    AS CHAR FORMAT "x(35)"
   FIELD serie      LIKE movto-estoq.serie
   FIELD nro-docto  LIKE movto-estoq.nro-docto 
   FIELD cod-emitennte LIKE movto-estoq.cod-emitente
   FIELD it-codigo     LIKE  movto-estoq.it-codigo
   FIELD desc-item     LIKE ITEM.desc-item 
   FIELD vl-debito     LIKE  movto-estoq.val-cofins
   FIELD vl-credito    LIKE  movto-estoq.val-cofins
   FIELD  estorna-piscofins AS LOG FORMAT "SIM/NÇO"
   FIELD  estorna-icms      AS LOG FORMAT "SIM/NÇO"
   FIELD  narrativa        AS CHAR FORMAT "x(50)".

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
         HEIGHT             = 1.92
         WIDTH              = 40.
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
   /*  {include/i-rpout.i}       */
   /*  view frame f-cabec.       */
   /*  view frame f-rodape.      */
    run utp/ut-acomp.p persistent set h-acomp.  
    
   {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c} 
    
    run pi-inicializar in h-acomp (input "Processando...":U). 
   /*                                                           */
   /*  /*:T --- Colocar aqui o código de impressão --- */       */
   /*  for each [TABELA] no-lock                                */
   /*      where [WHERE].                                       */

/*         run pi-acompanhar in h-acomp (input "xxxxxxxxxxxxxx":U). */
   /*  end.         */


   RUN pi-processa.
    
    run pi-finalizar in h-acomp.
/*     {include/i-rpclo.i} */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-carrega-it-estorno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-it-estorno Procedure 
PROCEDURE pi-carrega-it-estorno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST tt-digita NO-LOCK NO-ERROR.

IF NOT AVAIL tt-digita THEN DO:
    FOR EACH es-it-uf-estorno-cred WHERE es-it-uf-estorno-cred.it-codigo >= tt-param.item-i
                                   AND   es-it-uf-estorno-cred.it-codigo <= tt-param.item-f
                                   NO-LOCK :
        CREATE tt-it-estorno.                                 
        ASSIGN tt-it-estorno.it-codigo = es-it-uf-estorno-cred.it-codigo.
               tt-it-estorno.estado    = es-it-uf-estorno-cred.estado. 
    END.
END.
ELSE DO:
    FOR EACH tt-digita:
        CREATE tt-it-estorno.                                                
        ASSIGN tt-it-estorno.it-codigo = tt-digita.it-codigo.    
               tt-it-estorno.estado    = "".       

    END.
END.








FOR EACH es-cc-uf-estorno-cred WHERE es-cc-uf-estorno-cred.cod_ccusto >= tt-param.ccusto-i
                               AND   es-cc-uf-estorno-cred.cod_ccusto <= tt-param.ccusto-f :
    CREATE tt-es-cc-estorno-cred.
    BUFFER-COPY es-cc-uf-estorno-cred TO tt-es-cc-estorno-cred.
END.






                                                                
                                 


   









END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-cabe-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-cabe-excel Procedure 
PROCEDURE pi-cria-cabe-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: */
   
  
    CREATE "Excel.Application" chExcelApp.
    chExcelApp:VISIBLE = FALSE.
    chWorkbook = chExcelApp:Workbooks:Add().
    chExcelApp:AlertBeforeOverwriting = NO.
    chWorkSheet = chExcelApp:Sheets:Item(1).
    chWorkSheet:name = "Composi‡Æo dos Cr‚ditos".
    chExcelApp:ActiveWindow:Zoom = 90.

    cRange = "A1:Q1".
    chExcelApp:SELECTION:MergeCells = YES.
    chExcelApp:Range(cRange):FONT:bold = YES.
    chExcelApp:Range(cRange):Merge.
    chWorkSheet:Range(cRange):value = "Composi‡Æo dos Cr‚ditos".
    chExcelApp:Range(cRange):FONT:NAME = "Courier New".
    chExcelApp:Range(cRange):FONT:SIZE = "11".
    chExcelApp:Range(cRange):HorizontalAlignment = 3.

    /* ------------------------------------------------------------------------------------------------------------------------- */

    chWorkSheet:Range("A3"):value = "Data".                                  
    chWorkSheet:Range("B3"):value = "Nota Fiscal".                           
    chWorkSheet:Range("C3"):value = "Item".                                  
    chWorkSheet:Range("D3"):value = "Descri‡Æo".                             
    chWorkSheet:Range("E3"):value = "Valor Total ".                          
    chWorkSheet:Range("F3"):value = "Base Calc. ICMS".                       
    chWorkSheet:Range("G3"):value = "Aliquota ICMS".                         
    chWorkSheet:Range("H3"):value = "Credito ICMS".                          
    chWorkSheet:Range("I3"):value = "Base Calc. PIS".                        
    chWorkSheet:Range("J3"):value = "Aliquota PIS".                          
    chWorkSheet:Range("K3"):value = "Credito PIS".                           
    chWorkSheet:Range("L3"):value = "Base Calc. Cofins".                     
    chWorkSheet:Range("M3"):value = "Aliquota Cofins".                       
    chWorkSheet:Range("N3"):value = "Credito Cofins".                   
  
    chExcelApp:Range("A3:n3"):FONT:bold = YES.
    chExcelApp:Range("A3:n3"):FONT:NAME = "Arial".
    chExcelApp:Range("A3:n3"):FONT:SIZE = "9".
    chExcelApp:Range("A3:n3"):HorizontalAlignment = 3.
    chExcelApp:Range("A3:n3"):AutoFilter(,,,).

    chExcelApp:ActiveWindow:SplitColumn = 0.
    chExcelApp:ActiveWindow:SplitRow    = 3.
    chExcelApp:ActiveWindow:FreezePanes = YES.


    /**consumo**/

    chWorkSheet = chExcelApp:Sheets:Item(2).
    chWorkSheet:name = "Movimenta‡Æo de Consumo".
    chExcelApp:ActiveWindow:Zoom = 90.
    cRange = "A1:Q1".
    chExcelApp:Range(cRange):SELECT.
    chExcelApp:SELECTION:MergeCells = YES.
    chExcelApp:Range(cRange):FONT:bold = YES.
    chExcelApp:Range(cRange):Merge.
    chWorkSheet:Range(cRange):value = "".
    chExcelApp:Range(cRange):FONT:NAME = "Courier New".
    chExcelApp:Range(cRange):FONT:SIZE = "11".
    chExcelApp:Range(cRange):HorizontalAlignment = 3.

    chWorkSheet:Range("A3"):value = "Conta".                             
    chWorkSheet:Range("B3"):value = "Descri‡Æo Conta".                      
    chWorkSheet:Range("C3"):value = "Centro Custo".                             
    chWorkSheet:Range("D3"):value = "Descri‡Æo C.Custo".                        
    chWorkSheet:Range("E3"):value = "Estab".                     
    chWorkSheet:Range("F3"):value = "Data TransÆo".                  
    chWorkSheet:Range("G3"):value = "Especie".                    
    chWorkSheet:Range("H3"):value = "Serie".                     
    chWorkSheet:Range("I3"):value = "Documento".                   
    chWorkSheet:Range("J3"):value = "Emitente".                     
    chWorkSheet:Range("K3"):value = "Item".                      
    chWorkSheet:Range("L3"):value = "Descri‡Æo".                
    chWorkSheet:Range("M3"):value = "D‚bito".
    chWorkSheet:Range("n3"):value = "Cr‚dito".
    chWorkSheet:Range("o3"):value = "Estorna PISCOFINS".
    chWorkSheet:Range("p3"):value = "Estorna ICMS".
    chWorkSheet:Range("q3"):value = "Narrativa".
    chExcelApp:Range("A3:q3"):FONT:bold = YES.
    chExcelApp:Range("A3:q3"):FONT:NAME = "Arial".
    chExcelApp:Range("A3:q3"):FONT:SIZE = "9".
    chExcelApp:Range("A3:q3"):HorizontalAlignment = 3.
    chExcelApp:Range("A3:q3"):AutoFilter(,,,).

    chExcelApp:ActiveWindow:SplitColumn = 0.
    chExcelApp:ActiveWindow:SplitRow    = 3.
    chExcelApp:ActiveWindow:FreezePanes = YES.











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

  chWorkSheet = chExcelApp:Sheets:Item(1).
  FOR EACH tt-fisc-excel:
       RUN pi-acompanhar in h-acomp(INPUT "Item Excel: " +  "--" + STRING(tt-fisc-excel.it-codigo) + "-" + STRING(i-linha) ).  

       chWorkSheet:Range("A" + string(i-linha)):VALUE = tt-fisc-excel.dt-docto      .     
       chWorkSheet:Range("B" + string(i-linha)):VALUE = tt-fisc-excel.nr-doc-fis    .     
       chWorkSheet:Range("C" + string(i-linha)):VALUE = tt-fisc-excel.it-codigo     .
       chWorkSheet:Range("D" + string(i-linha)):VALUE = tt-fisc-excel.desc-item     .
       chWorkSheet:Range("E" + string(i-linha)):VALUE = tt-fisc-excel.vl-tot-item   .
       chWorkSheet:Range("F" + string(i-linha)):VALUE = tt-fisc-excel.vl-bicms-it   .
       chWorkSheet:Range("G" + string(i-linha)):VALUE = tt-fisc-excel.aliquota-icm  .
       chWorkSheet:Range("H" + string(i-linha)):VALUE = tt-fisc-excel.vl-icms-it   .
       chWorkSheet:Range("I" + string(i-linha)):VALUE = tt-fisc-excel.val-base-calc-pis .
       chWorkSheet:Range("J" + string(i-linha)):VALUE = tt-fisc-excel.char-2   .
       chWorkSheet:Range("K" + string(i-linha)):VALUE = tt-fisc-excel.val-pis  .
       chWorkSheet:Range("L" + string(i-linha)):VALUE = tt-fisc-excel.val-base-calc-cofins .
       chWorkSheet:Range("M" + string(i-linha)):VALUE = tt-fisc-excel.char-confis.
       chWorkSheet:Range("N" + string(i-linha)):VALUE = tt-fisc-excel.val-cofins .

       ASSIGN i-linha = i-linha + 1.
       
   END.

   
    i-linha = 4.
     chWorkSheet = chExcelApp:Sheets:Item(2).
   FOR EACH tt-it-mov-consumo:

     

     RUN pi-acompanhar in h-acomp(INPUT "Item Excel Consumo: " +  "--" + STRING(tt-it-mov-consumo.it-codigo) + "-" + STRING(i-conta-consumo) ).  

     chWorkSheet:Range("A" + string(i-linha)):VALUE =  tt-it-mov-consumo.ct-codigo.         
     chWorkSheet:Range("B" + string(i-linha)):VALUE =  tt-it-mov-consumo.c-titulo.    
     chWorkSheet:Range("C" + string(i-linha)):VALUE =  tt-it-mov-consumo.sc-codigo.   
     chWorkSheet:Range("D" + string(i-linha)):VALUE =  tt-it-mov-consumo.descricao.   
     chWorkSheet:Range("E" + string(i-linha)):VALUE =  tt-it-mov-consumo.cod-estabel. 
     chWorkSheet:Range("F" + string(i-linha)):VALUE =  tt-it-mov-consumo.dt-trans.   
     chWorkSheet:Range("G" + string(i-linha)):VALUE =  tt-it-mov-consumo.espdocto.   
     chWorkSheet:Range("H" + string(i-linha)):VALUE =  tt-it-mov-consumo.serie.             
     chWorkSheet:Range("I" + string(i-linha)):VALUE =  tt-it-mov-consumo.nro-docto.         
     chWorkSheet:Range("J" + string(i-linha)):VALUE =  tt-it-mov-consumo.cod-emitennte.           
     chWorkSheet:Range("K" + string(i-linha)):VALUE =  tt-it-mov-consumo.it-codigo.            
     chWorkSheet:Range("L" + string(i-linha)):VALUE =  tt-it-mov-consumo.desc-item   .           
     chWorkSheet:Range("M" + string(i-linha)):VALUE =  tt-it-mov-consumo.vl-debito     .              
     chWorkSheet:Range("N" + string(i-linha)):VALUE =  tt-it-mov-consumo.vl-credito  .     
     chWorkSheet:Range("O" + string(i-linha)):VALUE =  tt-it-mov-consumo.estorna-piscofins .  
     chWorkSheet:Range("P" + string(i-linha)):VALUE =  tt-it-mov-consumo.estorna-icms  .   
     chWorkSheet:Range("Q" + string(i-linha)):VALUE =  tt-it-mov-consumo.narrativa     . 

     ASSIGN i-linha = i-linha + 1.
     i-conta-consumo = i-conta-consumo - 1.
   END.


































  



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-finaliza-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-finaliza-excel Procedure 
PROCEDURE pi-finaliza-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
chWorkSheet:Range("A:n"):EntireColumn:AutoFit.
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

RUN pi-carrega-it-estorno.
RUN pi-processa-entradas.
RUN pi-processa-consumo.
RUN pi-cria-cabe-excel.
RUN pi-cria-excel.
RUN pi-finaliza-excel.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa-consumo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa-consumo Procedure 
PROCEDURE pi-processa-consumo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE  de-vl-credito LIKE movto-estoq.valor-pis   NO-UNDO.
 DEFINE VARIABLE  de-vl-debito  LIKE movto-estoq.valor-pis   NO-UNDO.


 i-conta-consumo = 0.


    

 FOR EACH movto-estoq WHERE ( IF NOT tt-param.t-all  THEN movto-estoq.esp-docto = 28 ELSE YES
                      OR    IF NOT tt-param.t-all  THEN movto-estoq.esp-docto   = 5  ELSE YES)
                      AND  movto-estoq.dt-trans >= tt-param.periodo-i
                      AND  movto-estoq.dt-trans <= tt-param.periodo-f  NO-LOCK:





     FIND estabelec WHERE estabelec.cod-estabel = movto-estoq.cod-estabel NO-LOCK NO-ERROR.


    


     FIND FIRST  tt-it-estorno WHERE  tt-it-estorno.it-codigo = movto-estoq.it-codigo NO-LOCK NO-ERROR.
     IF NOT AVAIL tt-it-estorno THEN NEXT.
     ELSE DO:
          IF tt-it-estorno.estado = "" THEN.
          ELSE DO:
          IF tt-it-estorno.estado = estabelec.estado THEN .
          ELSE  NEXT.
          END.


     END.


     i-conta-consumo = i-conta-consumo + 1.

     RUN pi-acompanhar in h-acomp(INPUT "Item Consumo: " +  "--" + STRING(movto-estoq.it-codigo) + "-" + STRING(i-conta-consumo) ). 

     FIND ITEM OF movto-estoq NO-LOCK NO-ERROR.

   
    
 
    FIND tt-es-cc-estorno-cred WHERE tt-es-cc-estorno-cred.cod_ccusto = movto-estoq.sc-codigo 
                               AND   tt-es-cc-estorno-cred.estado     = estabelec.estado
                               NO-LOCK  NO-ERROR.  
                                                                    




      FIND ems5.ccusto  WHERE ccusto.cod_ccusto = movto-estoq.sc-codigo NO-LOCK NO-ERROR.               
                                                                                                                                                                                          
      FIND FIRST  ems2cadme.conta-contab WHERE conta-contab.ct-codigo = movto-estoq.ct-codigo NO-LOCK  NO-ERROR.

            de-vl-credito = 0. 
            de-vl-debito  = 0.

            IF movto-estoq.tipo-valor = 1   THEN  ASSIGN de-vl-credito = movto-estoq.valor-mat-m[1].
            ELSE ASSIGN de-vl-debito = movto-estoq.valor-mat-m[1].
                                                                                  
     
    
            CREATE tt-it-mov-consumo.                                                                                                   
            tt-it-mov-consumo.ct-codigo   = movto-estoq.ct-codigo .                                                  
            tt-it-mov-consumo.c-titulo    = IF AVAIL conta-contab THEN conta-contab.titulo ELSE "" .                 
            tt-it-mov-consumo.sc-codigo   = movto-estoq.sc-codigo.                                                  
            tt-it-mov-consumo.descricao   = IF AVAIL ccusto THEN ccusto.des_tit_ctbl  ELSE "".       
            tt-it-mov-consumo.cod-estabel = movto-estoq.cod-estabel .                                               
            tt-it-mov-consumo.dt-trans    = movto-estoq.dt-trans .                                                  
            tt-it-mov-consumo.espdocto    = ENTRY(movto-estoq.esp-docto,{ininc/i03in218.i 3},",") .
            tt-it-mov-consumo.serie       = movto-estoq.serie  .                                                    
            tt-it-mov-consumo.nro-docto   = movto-estoq.nro-docto .                                                 
            tt-it-mov-consumo.cod-emitennte =  movto-estoq.cod-emitente .                                             
            tt-it-mov-consumo.it-codigo   = movto-estoq.it-codigo .                                                 
            tt-it-mov-consumo.desc-item   =  ITEM.desc-item .                                                        
            tt-it-mov-consumo.vl-debito   = de-vl-debito  .                                       
            tt-it-mov-consumo.vl-credito  =  de-vl-credito .
            tt-it-mov-consumo.estorna-piscofins = IF AVAIL tt-es-cc-estorno-cred  THEN tt-es-cc-estorno-cred.estorna-pis-confis  ELSE NO.
            tt-it-mov-consumo.estorna-icms      = IF AVAIL tt-es-cc-estorno-cred THEN  tt-es-cc-estorno-cred.estorna-icms ELSE NO .
            tt-it-mov-consumo.narrativa     = "".
                                                                                                 
                                                                                                 
       
END.                                                                                 















                                                                                  

                                                                                  
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa-entradas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa-entradas Procedure 
PROCEDURE pi-processa-entradas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH doc-fiscal  WHERE doc-fiscal.tipo-nat = 1 
                     AND  doc-fiscal.dt-doc >= tt-param.periodo-i 
                     AND  doc-fiscal.dt-doc <= tt-param.periodo-f  NO-LOCK,
    EACH it-doc-fisc WHERE (it-doc-fisc.vl-icms-it > 0
                     OR    it-doc-fisc.val-pis > 0
                     OR    it-doc-fisc.val-cofins > 0)
      
    OF doc-fiscal,
    EACH ITEM OF it-doc-fisc NO-LOCK:


    FIND FIRST  tt-it-estorno WHERE  tt-it-estorno.it-codigo = it-doc-fisc.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-it-estorno THEN NEXT.

    RUN pi-acompanhar in h-acomp(INPUT "Item Entradas: " +  "--" + STRING(it-doc-fisc.it-codigo) ). 
 
  CREATE tt-fisc-excel.                                                                                                
 ASSIGN tt-fisc-excel.dt-docto            =   it-doc-fisc.dt-docto
        tt-fisc-excel.nr-doc-fis          =   it-doc-fisc.nr-doc-fis                                              
        tt-fisc-excel.it-codigo           =   it-doc-fisc.it-codigo                                               
        tt-fisc-excel.desc-item           =   ITEM.desc-item                                                      
        tt-fisc-excel.vl-tot-item         =   it-doc-fisc.vl-tot-item                                             
        tt-fisc-excel.vl-bicms-it         =   it-doc-fisc.vl-bicms-it                                             
        tt-fisc-excel.aliquota-icm        =   it-doc-fisc.aliquota-icm                                            
        tt-fisc-excel.vl-icms-it          =   it-doc-fisc.vl-icms-it                                              
        tt-fisc-excel.val-base-calc-pis   =   it-doc-fisc.val-base-calc-pis                                       
        tt-fisc-excel.char-2              =   substr(it-doc-fisc.char-2,22,7)     
        tt-fisc-excel.val-pis             =   it-doc-fisc.val-pis                                                 
        tt-fisc-excel.val-base-calc-cofins =  it-doc-fisc.val-base-calc-cofins                                    
        tt-fisc-excel.char-confis          =  substr(it-doc-fisc.char-2,30,7) 
        tt-fisc-excel.val-cofins           =   it-doc-fisc.val-cofins  .

END.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/************************************************************************
**
**  Programa: ESMAT001RP
**
**  Objetivo: Relatorio de Gastos de Materiais
**
**     Autor: Maria Aparecida L Nogueira - DSC
**
**      Data: Agosto/2007
**
**    Versao: 2.06.00.000 - Desenvolvimento Inicial
**            
**
************************************************************************/
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i ESMAT001RP 2.06.00.001}

/* Definicao de temp-table */
   

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD idi-tipo-imp     AS INT
    Field impressora       As Char Format "x(256)"
    FIELD dt-base          AS DATE FORMAT "99/99/9999"
    FIELD dt-base-fim      AS DATE FORMAT "99/99/9999"
    FIELD fi-cc-ini        AS CHAR FORMAT "x(8)"
    FIELD fi-cc-fim        AS CHAR FORMAT "x(8)"
    FIELD fi-item-ini      AS CHAR FORMAT "x(16)"
    FIELD fi-item-fim      AS char FORMAT "x(16)"
    FIELD rs-tp-contr      AS INT
    FIELD fi-est-ini       AS CHAR FORMAT "x(3)"
    FIELD fi-est-fim       AS CHAR FORMAT "x(3)".      


DEFINE TEMP-TABLE tt-controle  NO-UNDO     
 FIELD dt-trans       LIKE movto-estoq.dt-trans
 FIELD sc-codigo      LIKE movto-estoq.sc-codigo
 FIELD tipo-contr     AS INT /* 1- Fisico, 2- Total, 3- Consignado, 4- Debito Direto */
 field it-codigo      LIKE movto-estoq.it-codigo               
 FIELD descricao      AS CHAR FORMAT "x(32)" 
 FIELD cod-estabel    LIKE movto-estoq.cod-estabel             
 FIELD conta-contabil LIKE movto-estoq.conta-contabil        
 FIELD qtde-cons      LIKE movto-estoq.quantidade            
 FIELD pr-unit-per    AS DEC                                   
 FIELD pr-tot         AS DEC FORMAT "->>>>>,>>>,>>9.9999"
 FIELD nr-docto       LIKE movto-estoq.nro-docto
 FIELD un             LIKE ITEM.un
 FIELD desc-conta     LIKE cta_ctbl.des_tit_ctbl
 FIELD desc-sc-codigo LIKE centro-custo.descricao.


    
define temp-table tt-digita no-undo
    field seq              AS INT
    field cod-depos        LIKE deposito.cod-depos
    field ge-codigo        LIKE grup-estoque.ge-codigo
    index id seq.
            
def temp-table tt-raw-digita NO-UNDO
    field raw-digita      as raw.


DEF TEMP-TABLE tt-vendas
    FIELD it-codigo LIKE item.it-codigo
    FIELD periodo   AS CHAR
    FIELD vl-vendas LIKE ped-item.vl-tot-it
    FIELD qt-aberto LIKE ped-item.vl-tot-it.
    
DEF TEMP-TABLE tt-consumo
    FIELD it-codigo    LIKE item.it-codigo
    FIELD periodo      as   char 
    field qt-consumo   like movto-estoq.quantidade.
       

def INPUT parameter raw-param as raw NO-UNDO.
def INPUT parameter TABLE for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

/* definicao de buffers */
{include/i-rpvar.i} 

/* Definicao de Variaveis */
DEF VAR h-acomp          AS HANDLE                    NO-UNDO.
DEF VAR i-linha          AS INT                       NO-UNDO.
DEF VAR da-data          AS DATE FORMAT "99/99/9999"  NO-UNDO.
DEF VAR i-mes-1          AS INT  FORMAT 99            NO-UNDO.
DEF VAR i-qtd-meses      AS INT  FORMAT 99            NO-UNDO.
DEF VAR tot-qtde         LIKE movto-estoq.quantidade .

/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.

/* Processamento */
FIND FIRST tt-param NO-ERROR.

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa NO-LOCK
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-ERROR.

ASSIGN c-programa     = "ESMAT001RP"
       c-versao       = "2.06.00"
       c-revisao      = "001"
       c-empresa      = empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "Relatorio de Materiais".

/* Definicao de LayOut do Relatorio */
/* {include/i-rpcab.i}  */

/* Processamento das Informacoes */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar in h-acomp (INPUT "Aguarde, Selecionando os Dados...").

/* {include/i-rpout.i}   */
/* VIEW FRAME f-cabec.   */
/* VIEW FRAME f-rodape.  */


 /* tipo de controle total ou consignado */
 IF tt-param.rs-tp-contr = 2 OR 
    tt-param.rs-tp-contr = 3 THEN DO:      

  FOR EACH tt-controle:
      DELETE tt-controle.
  END.

 
 FOR EACH ITEM 
     WHERE (ITEM.tipo-contr = 2
        OR  ITEM.tipo-contr = 3) 
       AND item.it-codigo >= fi-item-ini
       AND ITEM.it-codigo <= fi-item-fim  NO-LOCK:

  FOR EACH  movto-estoq NO-LOCK /* USE-INDEX data */
     WHERE  movto-estoq.it-codigo     = ITEM.it-codigo
       AND (movto-estoq.sc-codigo    >= tt-param.fi-cc-ini
       AND  movto-estoq.sc-codigo    <= tt-param.fi-cc-fim)
      /* AND (movto-estoq.it-codigo    >= tt-param.fi-item-ini  
       AND  movto-estoq.it-codigo    <= tt-param.fi-item-fim) */ 
       AND (movto-estoq.dt-trans     >= tt-param.dt-base   
       and  movto-estoq.dt-trans     <= tt-param.dt-base-fim)
       AND (movto-estoq.cod-estabel  >= tt-param.fi-est-ini   
       and  movto-estoq.cod-estabel  <= tt-param.fi-est-fim)
      BREAK BY(movto-estoq.ct-codigo)
            BY(movto-estoq.sc-codigo)
            BY(movto-estoq.it-codigo): 

  IF esp-docto = 5 OR 
     esp-docto = 7 OR 
     esp-docto = 28 OR 
     esp-docto = 30 THEN  DO:

     
      RUN pi-acompanhar IN h-acomp("Movto ITEM: " + movto-estoq.it-codigo + "-"
                                             + STRING(movto-estoq.dt-trans,"99/99/9999")).

      IF esp-docto = 28 OR
         esp-docto = 30 THEN
         ASSIGN tot-qtde = tot-qtde + movto-estoq.quantidade.
      ELSE 
      IF esp-docto = 5 OR
         esp-docto = 7 THEN
         ASSIGN tot-qtde = tot-qtde - movto-estoq.quantidade.

    
       IF LAST-OF(movto-estoq.it-codigo) 
          /*AND  LAST-OF(movto-estoq.sc-codigo) */ THEN DO:
          
             CREATE tt-controle.
             ASSIGN tt-controle.tipo-contr     = int(item.tipo-contr)        
                    tt-controle.dt-trans       = movto-estoq.dt-trans  
                    tt-controle.it-codigo      = movto-estoq.it-codigo
                    tt-controle.descricao      = ITEM.descricao-1 + ITEM.descricao-2
                    tt-controle.cod-estabel    = movto-estoq.cod-estabel
                    tt-controle.conta-contabil = movto-estoq.ct-codigo
                    tt-controle.sc-codigo      = movto-estoq.sc-codigo
                    tt-controle.qtde-cons      = tot-qtde
                    tt-controle.un             = ITEM.un. 

             find first cta_ctbl no-lock
                  where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
                    and cta_ctbl.cod_cta_ctbl       = movto-estoq.ct-codigo no-error.
             IF AVAIL cta_ctbl THEN DO:
                ASSIGN desc-conta = cta_ctbl.des_tit_ctbl.
                FIND centro-custo WHERE 
                     centro-custo.cc-codigo = movto-estoq.sc-codigo NO-LOCK NO-ERROR.
                IF AVAIL centro-custo THEN 
                   ASSIGN desc-sc-codigo = centro-custo.descricao.
             END.

             FIND LAST pr-it-per                                                        
                 WHERE pr-it-per.periodo    <= tt-param.dt-base-fim 
                   AND pr-it-per.it-codigo   = movto-estoq.it-codigo                      
                   AND pr-it-per.cod-estabel = movto-estoq.cod-estabel NO-LOCK NO-ERROR.
                                                                                        
             IF AVAIL pr-it-per THEN DO:                                                 
                ASSIGN tt-controle.pr-unit-per = pr-it-per.val-unit-mat-m[1] +           
                                                 pr-it-per.val-unit-mob-m[1] +           
                                                 pr-it-per.val-unit-ggf-m[1]
                       tt-controle.pr-tot = tt-controle.pr-unit-per * tot-qtde.

             END.
              
             ASSIGN tot-qtde = 0.
              
       END.
  END. /* movto-estoq */
  END.
 END. /* item */

  
 END.

 
 /* tipo de controle fisico e debito direto */
 IF (tt-param.rs-tp-contr = 1      OR
     tt-param.rs-tp-contr = 4)     THEN DO:


  FOR EACH tt-controle:
      DELETE tt-controle.
  END.

  FOR EACH ITEM 
     WHERE (ITEM.tipo-contr = 1
        OR  ITEM.tipo-contr = 4) 
       AND item.it-codigo >= fi-item-ini
       AND ITEM.it-codigo <= fi-item-fim  NO-LOCK:
  

   FOR EACH movto-estoq  NO-LOCK /* USE-INDEX data */                 
       WHERE  movto-estoq.it-codigo    = ITEM.it-codigo
       AND   (movto-estoq.sc-codigo   >= tt-param.fi-cc-ini           
       AND    movto-estoq.sc-codigo   <= tt-param.fi-cc-fim)          
      /*AND   (movto-estoq.it-codigo   >= tt-param.fi-item-ini         
       and    movto-estoq.it-codigo   <= tt-param.fi-item-fim) */       
       AND   (movto-estoq.dt-trans    >= tt-param.dt-base             
       and    movto-estoq.dt-trans    <= tt-param.dt-base-fim)
       AND   (movto-estoq.cod-estabel >= tt-param.fi-est-ini   
       and    movto-estoq.cod-estabel <= tt-param.fi-est-fim)
       AND   (movto-estoq.esp-docto    = 21 OR
              movto-estoq.esp-docto    = 18)
       AND    movto-estoq.tipo-trans   = 2:       


       IF movto-estoq.ct-codigo = "13601002" OR
          movto-estoq.ct-codigo = "17202002" OR
          movto-estoq.ct-codigo = "11511004" OR
          movto-estoq.ct-codigo = "17202014" OR
          movto-estoq.ct-codigo = "41106002" OR
          movto-estoq.ct-codigo = "11508202" THEN
          NEXT.
 
       RUN pi-acompanhar IN h-acomp("Movto ITEM: " + movto-estoq.it-codigo + "-"
                                             + STRING(movto-estoq.dt-trans,"99/99/9999")).
          IF it-codigo = '6180039' THEN      /*  valida informa‡Æo */
              MESSAGE it-codigo cod-estabel. /*                     */
            
            CREATE tt-controle.                                                        
            ASSIGN tt-controle.tipo-contr     = int(item.tipo-contr)                        
                   tt-controle.dt-trans       = movto-estoq.dt-trans                   
                   tt-controle.tipo-contr     = ITEM.tipo-contr                        
                   tt-controle.it-codigo      = movto-estoq.it-codigo                  
                   tt-controle.descricao      = ITEM.descricao-1 + ITEM.descricao-2    
                   tt-controle.cod-estabel    = movto-estoq.cod-estabel                
                   tt-controle.conta-contabil = movto-estoq.ct-codigo    
                   tt-controle.sc-codigo      = movto-estoq.sc-codigo
                   tt-controle.qtde-cons      = movto-estoq.quantidade 
                   tt-controle.nr-docto       = movto-estoq.nro-docto.
          

            ASSIGN  tt-controle.pr-tot = valor-ggf-m[1] + valor-mat-m[1] + valor-mob-m[1] .

            find first cta_ctbl no-lock
                 where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
                   and cta_ctbl.cod_cta_ctbl       = movto-estoq.ct-codigo no-error.
            IF AVAIL cta_ctbl THEN DO:
                ASSIGN desc-conta = cta_ctbl.des_tit_ctbl.
                FIND centro-custo NO-LOCK WHERE 
                     centro-custo.cc-codigo = movto-estoq.sc-codigo NO-ERROR.
                IF AVAIL centro-custo THEN 
                   ASSIGN desc-sc-codigo = centro-custo.descricao.
            END.


   END.

  END.
    
 END.




 /************* Criacao do Aplicativo EXCEL **********/
CREATE "Excel.Application" chExcelApplication.
FILE-INFO:FILE-NAME = "esp/esmat001.xls". 
chexcelapplication:workbooks:open(file-info:full-pathname,TRUE).
chexcelapplication:sheets:item(1).

/* CREATE "excel.application" chexcelapplication. /* Abre o excel */

   */


FIND FIRST tt-controle NO-LOCK NO-ERROR.

IF AVAIL tt-controle and
   (tt-controle.tipo-contr = 2 OR
    tt-controle.tipo-contr = 3) THEN DO:

   /* Cabe»alho  do Controle */


   ASSIGN  I-LINHA = 1.

   ASSIGN chexcelapplication:range("A" + STRING(I-LINHA)) = "Codigo do Item"
          chexcelapplication:range("B" + STRING(I-LINHA)) = "Descri‡Æo do Item"
          chexcelapplication:range("C" + STRING(I-LINHA)) = "UN" 
          chexcelapplication:range("D" + STRING(I-LINHA)) = "Est"
          chexcelapplication:range("E" + STRING(I-LINHA)) = "Conta Contabil"
          chexcelapplication:range("F" + STRING(I-LINHA)) = "Descri‡Æo"
          chexcelapplication:range("G" + STRING(I-LINHA)) = "Centro de Custo"
          chexcelapplication:range("H" + STRING(I-LINHA)) = "Descri‡Æo"
          chexcelapplication:range("I" + STRING(I-LINHA)) = "Qtde Cosumida"
          chexcelapplication:range("J" + STRING(I-LINHA)) = "Pre‡o Unit rio"
          chexcelapplication:range("K" + STRING(I-LINHA)) = "Total".
        
END.

IF AVAIL tt-controle and
   (tt-controle.tipo-contr = 1 OR
    tt-controle.tipo-contr = 4) THEN DO:

   /* Cabe»alho  do Controle */
   ASSIGN  I-LINHA = 1.

   ASSIGN chexcelapplication:range("A" + STRING(I-LINHA)) = "Codigo do Item"
          chexcelapplication:range("B" + STRING(I-LINHA)) = "Descri‡Æo do Item"
          chexcelapplication:range("C" + STRING(I-LINHA)) = "UN"
          chexcelapplication:range("D" + STRING(I-LINHA)) = "Est"
          chexcelapplication:range("E" + STRING(I-LINHA)) = "Conta Cont bil"
          chexcelapplication:range("F" + STRING(I-LINHA)) = "Descri‡Æo"
          chexcelapplication:range("G" + STRING(I-LINHA)) = "Centro de Custo"
          chexcelapplication:range("H" + STRING(I-LINHA)) = "Descri‡Æo"
          chexcelapplication:range("I" + STRING(I-LINHA)) = "Qtde Comprada"
          chexcelapplication:range("J" + STRING(I-LINHA)) = "Nota de Entrada".
          chexcelapplication:range("K" + STRING(I-LINHA)) = "Valor da Compra".
        
END.



ASSIGN i-linha = 3.

FOR EACH tt-controle:

    IF tt-controle.tipo-contr = 2 OR
       tt-controle.tipo-contr = 3 THEN DO:

       ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-controle.it-codigo
              chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-controle.descricao
              chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-controle.un
              chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-controle.cod-estabel
              chExcelApplication:range( "E" + STRING(i-linha) ):value = tt-controle.conta-contabil
              chExcelApplication:range( "F" + STRING(i-linha) ):value = tt-controle.desc-conta
              chExcelApplication:range( "G" + STRING(i-linha) ):value = tt-controle.sc-codigo
              chExcelApplication:range( "h" + STRING(i-linha) ):value = tt-controle.desc-sc-codigo
              /* chExcelApplication:Range( "F" + STRING(i-linha)):NumberFormat="#.##0,00" */
              chExcelApplication:range( "i" + STRING(i-linha) ):value = tt-controle.qtde-cons
              chExcelApplication:range( "j" + STRING(i-linha) ):value = tt-controle.pr-unit-per
              chExcelApplication:range( "k" + STRING(i-linha) ):value = tt-controle.pr-tot.
    END.

    IF tt-controle.tipo-contr = 1 OR
       tt-controle.tipo-contr = 4 THEN DO:

       ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-controle.it-codigo
              chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-controle.descricao
              chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-controle.un
              chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-controle.cod-estabel
              chExcelApplication:range( "E" + STRING(i-linha) ):value = tt-controle.conta-contabil
              chExcelApplication:range( "F" + STRING(i-linha) ):value = tt-controle.desc-conta
              chExcelApplication:range( "G" + STRING(i-linha) ):value = tt-controle.sc-codigo
              chExcelApplication:range( "h" + STRING(i-linha) ):value = tt-controle.desc-sc-codigo
              /* chExcelApplication:Range( "F" + STRING(i-linha)):NumberFormat="#.##0,00" */
              chExcelApplication:range( "i" + STRING(i-linha) ):value = tt-controle.qtde-cons
              chExcelApplication:range( "j" + STRING(i-linha) ):value = tt-controle.nr-docto 
              chExcelApplication:range( "k" + STRING(i-linha) ):value = tt-controle.pr-tot. 

        
    END.


    i-linha = i-linha + 1.

END.

CASE tt-param.idi-tipo-imp:
    WHEN 1 THEN DO: /* Impressora */
        chExcelApplication:ActiveSheet:PrintOut.
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:QUIT().
    END.

   /* WHEN 2 THEN DO: /* Arquivo    */
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:ActiveSheet:SaveAs(tt-param.arquivo,,,,,,).
        chExcelApplication:QUIT().
    END. */
    WHEN 2 THEN DO: /* Terminal   */
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:Visible = TRUE.
    END.

END CASE.

RELEASE OBJECT chExcelApplication NO-ERROR.

RUN pi-finalizar IN h-acomp.

/* {include/i-rpclo.i}  */

RETURN "OK":U.

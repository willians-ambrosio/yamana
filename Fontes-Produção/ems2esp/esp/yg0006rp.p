/********************************************************************************************
**  Programa: YG0006RP
**  Data....: 26 de Julho de 2017
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Movimento de Estoque - Garantia
**  Vers?o..: 12.1.17.000 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de vers?o */
{include/i-prgvrs.i YG0006RP 12.1.17.000}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
{esp/yg0006.i}  /* Definiá‰es da tt-param */

    
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

DEFINE STREAM st-excel.
/********************************************************************************************/

/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de parœmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param.
/********************************************************************************************/

/********************************** DEFINICAO DE VARIAVEIS **********************************/
define new global shared variable v_cod_usuar_corren as character no-undo.
define variable h-acomp        as handle    no-undo.

DEFINE VARIABLE i-cont AS INTEGER NO-UNDO.

&IF "{&mguni_version}" >= "2.071" &THEN
def new global shared var i-ep-codigo-usuario  LIKE ems2cadme.empresa.ep-codigo format "x(03)" no-undo.
&ELSE
def new global shared var i-ep-codigo-usuario  as integer format ">>9" no-undo.
&ENDIF
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

{include/i-rpvar.i}

   DEFINE VARIABLE c-cgc            LIKE  emitente.cgc        NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER           NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE    NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER             NO-UNDO.
   DEFINE VARIABLE i-restante       AS    INTEGER             NO-UNDO.


/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST  ems2cadme.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "yg0006RP"
	   c-versao	    = "12.1"
	   c-revisao	    = "17.000"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Movimentaá∆o de Estoque - Garantia".

FORM HEADER 
     FILL("-", 132)        FORMAT "x(132)" SKIP 
     c-empresa 
     c-titulo-relat AT 050
     "Pagina:":U    AT 120 
     page-number    AT 128 FORMAT ">>>>9" SKIP 
     FILL("-", 112)        FORMAT "x(110)" 
     TODAY                 FORMAT "99/99/9999"
     "-" 
     STRING(TIME,"HH:MM:SS":U) SKIP 
     "                       " SKIP
     "-----------------------"
WITH STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 132 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 132 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(132)"
  WITH STREAM-IO WIDTH 132 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/********************************************************************************************/
      
/************************************* BLOCO PRINCIPAL **************************************/
run utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Imprimindo *}

run pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* include padr?o para output de relatˆrios */
{include/i-rpout.i}

RUN pi-inicia-excel.
RUN pi-impressao.
run pi-encerra-excel.

/* fechamento do output do relatˆrio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
procedure pi-impressao:
   FOR EACH tt-raw-digita NO-LOCK:
      CREATE tt-digita.
      RAW-TRANSFER tt-raw-digita.raw-digita to tt-digita.
    
      CREATE tt-digita.
      RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
    END.

   FOR EACH es-garantia-item
            WHERE es-garantia-item.cod-estabel  >= tt-param.cod-estabel-ini   AND
                  es-garantia-item.cod-estabel  <= tt-param.cod-estabel-fim   AND
                 /* es-garantia-item.cod-emitente >= cod-emitente-ini           AND
                  es-garantia-item.cod-emitente <= cod-emitente-fim           AND*/
                  es-garantia-item.it-codigo    >= tt-param.item-ini          AND
                  es-garantia-item.it-codigo    <= tt-param.item-fim     
            NO-LOCK,
            FIRST es-garantia OF es-garantia-item
                  NO-LOCK:
               
       RUN pi-acompanhar IN h-acomp (input es-garantia-item.cod-estabel + "/" + es-garantia-item.it-codigo).
    
       FIND estabelec
            WHERE estabelec.cod-estabel = es-garantia-item.cod-estabel
            NO-LOCK NO-ERROR.
       FIND ems2cadme.empresa OF estabelec
            NO-LOCK NO-ERROR. 
       FIND item
            WHERE item.it-codigo = es-garantia-item.it-codigo
            NO-LOCK NO-ERROR.  
       FIND item-uni-estab
            WHERE item-uni-estab.cod-estabel = es-garantia-item.cod-estabel AND
                  item-uni-estab.it-codigo   = es-garantia-item.it-codigo
            NO-LOCK NO-ERROR.
            
       blk_mvt:
       FOR EACH movto-estoq USE-INDEX data-item
                WHERE movto-estoq.cod-estabel       = es-garantia-item.cod-estabel
                  AND movto-estoq.it-codigo         = es-garantia-item.it-codigo
                  AND movto-estoq.dt-trans         >= tt-param.dt-trans-ini
                  AND movto-estoq.dt-trans         <= tt-param.dt-trans-fim
                  AND movto-estoq.cod-refer        >= tt-param.cod-refer-ini
                  AND movto-estoq.cod-refer        <= tt-param.cod-refer-fim
                  AND movto-estoq.cod-depos        >= tt-param.cod-depos-ini
                  AND movto-estoq.cod-depos        <= tt-param.cod-depos-fim
                  AND movto-estoq.lote             >= tt-param.lote-ini
                  AND movto-estoq.lote             <= tt-param.lote-fim
                  AND movto-estoq.cod-localiz      >= tt-param.cod-localiz-ini
                  AND movto-estoq.cod-localiz      <= tt-param.cod-localiz-fim
/*                   AND movto-estoq.esp-docto        >= tt-param.especie-ini */
/*                   AND movto-estoq.esp-docto        <= tt-param.especie-fim */
                  AND movto-estoq.serie-docto      >= tt-param.serie-docto-ini
                  AND movto-estoq.serie-docto      <= tt-param.serie-docto-fim
                  AND movto-estoq.nro-docto        >= tt-param.nro-docto-ini
                  AND movto-estoq.nro-docto        <= tt-param.nro-docto-fim 
                NO-LOCK:
                
         IF NOT CAN-FIND(FIRST tt-digita
                         WHERE tt-digita.esp-docto   = movto-estoq.esp-docto AND
                               tt-digita.selecionado = "*"
                         NO-LOCK) THEN 
            NEXT blk_mvt.

             

         RUN pi-acompanhar IN h-acomp (INPUT "Data: " + string(movto-estoq.dt-trans,'99/99/9999')).
         
         RUN pi-dados-excel.
         
      END.
   END.
END PROCEDURE.

PROCEDURE pi-dados-excel:
   ASSIGN ch-excel:Range( "A"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(movto-estoq.esp-docto, {ininc/i03in218.i 03})                   
          ch-excel:Range( "B"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(movto-estoq.tipo-trans,{ininc/i01in218.i 03})                   
          ch-excel:Range( "C"  + STRING(i-cont-linha,'99999')):VALUE = ems2cadme.empresa.ep-codigo                                                     
          ch-excel:Range( "D"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.cod-estabel                                               
          ch-excel:Range( "E"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.it-codigo                                             
          ch-excel:Range( "F"  + STRING(i-cont-linha,'99999')):VALUE = item.desc-item                                                        
          ch-excel:Range( "G"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(item.tipo-contr,       {ininc/i09in122.i 03})     
          ch-excel:Range( "H"  + STRING(i-cont-linha,'99999')):VALUE = item-uni-estab.deposito-pad                                           
          ch-excel:Range( "I"  + STRING(i-cont-linha,'99999')):VALUE = item-uni-estab.cod-localiz                                            
          ch-excel:Range( "J"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(item.tipo-con-est,     {ininc/i01in122.i 03}) 
          ch-excel:Range( "K"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.lote                                                  
          ch-excel:Range( "L"  + STRING(i-cont-linha,'99999')):VALUE = item.un                                                               
          ch-excel:Range( "M"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.quantidade                                            
          ch-excel:Range( "N"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.valor-mat-m[1] + movto-estoq.valor-mob-m[1] + movto-estoq.valor-ggf-m[1]. 

   IF movto-estoq.tipo-trans = 1 THEN
      ASSIGN ch-excel:Range( "O"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.nro-docto   
             ch-excel:Range( "P"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.serie-docto 
             ch-excel:Range( "Q"  + STRING(i-cont-linha,'99999')):VALUE = ""
             ch-excel:Range( "R"  + STRING(i-cont-linha,'99999')):VALUE = "".
   ELSE
      ASSIGN ch-excel:Range( "O"  + STRING(i-cont-linha,'99999')):VALUE = "" 
             ch-excel:Range( "P"  + STRING(i-cont-linha,'99999')):VALUE = "" 
             ch-excel:Range( "Q"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.nro-docto  
             ch-excel:Range( "R"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.serie-docto.

   ASSIGN ch-excel:Range( "S"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.numero-ordem                
          ch-excel:Range( "T"  + STRING(i-cont-linha,'99999')):VALUE = es-garantia.qt-garantia.

   ASSIGN i-restante = 0.    
             
   FIND FIRST docum-est OF movto-estoq
        NO-LOCK NO-ERROR.
   IF AVAILABLE(docum-est) THEN DO:
      ASSIGN ch-excel:Range( "U"  + STRING(i-cont-linha,'99999')):VALUE = docum-est.dt-emissao.

      IF (docum-est.dt-emissao + es-garantia.qt-garantia) >= TODAY THEN
         ASSIGN i-restante = (docum-est.dt-emissao + es-garantia.qt-garantia) - TODAY.    
   END.
   
   ASSIGN ch-excel:Range( "V"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.dt-trans                 
          ch-excel:Range( "W"  + STRING(i-cont-linha,'99999')):VALUE = i-restante                           
          ch-excel:Range( "X"  + STRING(i-cont-linha,'99999')):VALUE = movto-estoq.nr-ord-produ  
       .

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-inicia-excel:

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + "_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".xlsx".
      
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo NO-ERROR.
  
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "YG0006".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "RELAT‡RIO DE MOVIMENTO DE ESTOQUE - GARANTIA  - YG0006 -  "  + STRING(TODAY).

   ASSIGN i-cont-linha = 3.

   ASSIGN ch-excel:columns( "A"   ):NumberFormat = "@"                                /* Tipo              */
          ch-excel:columns( "B"   ):NumberFormat = "@"                                /* Esp               */
          ch-excel:columns( "C"   ):NumberFormat = "@"                                /* Empresa           */
          ch-excel:columns( "D"   ):NumberFormat = "@"                                /* Estab             */
          ch-excel:columns( "E"   ):NumberFormat = "@"                                /* Item              */
          ch-excel:columns( "F"   ):NumberFormat = "@"                                /* Descriá∆o         */
          ch-excel:columns( "G"   ):NumberFormat = "@"                                /* Tipo Controle     */
          ch-excel:columns( "H"   ):NumberFormat = "@"                                /* Dep¢sito          */
          ch-excel:columns( "I"   ):NumberFormat = "@"                                /* Localizaá∆o       */
          ch-excel:columns( "J"   ):NumberFormat = "@"                                /* Tipo Controle     */
          ch-excel:columns( "K"   ):NumberFormat = "@"                                /* Lote/SÇrie        */
          ch-excel:columns( "L"   ):NumberFormat = "@"                                /* UN                */
          ch-excel:columns( "M"   ):NumberFormat = "#.##0,0000"                       /* Quantidade        */
          ch-excel:columns( "N"   ):NumberFormat = "#.##0,0000"                       /* Valor             */
          ch-excel:columns( "O"   ):NumberFormat = "@"                                /* Docto Entrada     */
          ch-excel:columns( "P"   ):NumberFormat = "@"                                /* SÇrie             */
          ch-excel:columns( "Q"   ):NumberFormat = "@"                                /* Docto Sa°da       */
          ch-excel:columns( "R"   ):NumberFormat = "@"                                /* SÇrie             */
          ch-excel:columns( "S"   ):NumberFormat = "@"                                /* Pedido Compra     */
          ch-excel:columns( "T"   ):NumberFormat = "@"                                /* Garantia          */
          ch-excel:columns( "U"   ):NumberFormat = "dd/mm/aaaa;@"                     /* Data Docto        */
          ch-excel:columns( "V"   ):NumberFormat = "dd/mm/aaaa;@"                     /* Data Movto        */
          ch-excel:columns( "W"   ):NumberFormat = "@"                                /* Restante          */
          ch-excel:columns( "X"   ):NumberFormat = "@"                                /* Ordem Manutená∆o  */
              .

   ASSIGN ch-excel:Range( "A"  + STRING(i-cont-linha,'99999')):VALUE = "Tipo"                      
          ch-excel:Range( "B"  + STRING(i-cont-linha,'99999')):VALUE = "Esp"                       
          ch-excel:Range( "C"  + STRING(i-cont-linha,'99999')):VALUE = "Empresa"                   
          ch-excel:Range( "D"  + STRING(i-cont-linha,'99999')):VALUE = "Estab"                     
          ch-excel:Range( "E"  + STRING(i-cont-linha,'99999')):VALUE = "Item"                      
          ch-excel:Range( "F"  + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o"                 
          ch-excel:Range( "G"  + STRING(i-cont-linha,'99999')):VALUE = "Tipo Controle"             
          ch-excel:Range( "H"  + STRING(i-cont-linha,'99999')):VALUE = "Dep¢sito"                  
          ch-excel:Range( "I"  + STRING(i-cont-linha,'99999')):VALUE = "Localizaá∆o"               
          ch-excel:Range( "J"  + STRING(i-cont-linha,'99999')):VALUE = "Tipo Controle"            
          ch-excel:Range( "K"  + STRING(i-cont-linha,'99999')):VALUE = "Lote/SÇrie"               
          ch-excel:Range( "L"  + STRING(i-cont-linha,'99999')):VALUE = "UN"                        
          ch-excel:Range( "M"  + STRING(i-cont-linha,'99999')):VALUE = "Quantidade"              
          ch-excel:Range( "N"  + STRING(i-cont-linha,'99999')):VALUE = "Valor"                     
          ch-excel:Range( "O"  + STRING(i-cont-linha,'99999')):VALUE = "Docto Entrada"             
          ch-excel:Range( "P"  + STRING(i-cont-linha,'99999')):VALUE = "SÇrie"                     
          ch-excel:Range( "Q"  + STRING(i-cont-linha,'99999')):VALUE = "Docto Sa°da"              
          ch-excel:Range( "R"  + STRING(i-cont-linha,'99999')):VALUE = "SÇrie"                     
          ch-excel:Range( "S"  + STRING(i-cont-linha,'99999')):VALUE = "Pedido Compra"               
          ch-excel:Range( "T"  + STRING(i-cont-linha,'99999')):VALUE = "Garantia"                  
          ch-excel:Range( "U"  + STRING(i-cont-linha,'99999')):VALUE = "Data Docto"                
          ch-excel:Range( "V"  + STRING(i-cont-linha,'99999')):VALUE = "Data Movto"                
          ch-excel:Range( "W"  + STRING(i-cont-linha,'99999')):VALUE = "Restante"                    
          ch-excel:Range( "X"  + STRING(i-cont-linha,'99999')):VALUE = "Ordem Manutená∆o"          
          .

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   ch-excel:Range("A3:X" + string(i-cont-linha - 1)):autofilter(,,,).

   ch-excel:Range("A1:X" + string(i-cont-linha - 1)):select.

   ch-excel:Range("A1:X3"):Font:Bold = TRUE.
       
   ch-excel:Columns("A"):ColumnWidth = 12.
   
   ch-excel:workbooks:item(1):SaveAs(c-arquivo,,,,,,).


   case tt-param.destino:
       when 1 then do:
          ch-excel:worksheets:item(1):select.
          ch-excel:Sheets:PrintOut.
          ch-excel:application:DisplayAlerts = false.
          ch-excel:quit().
       end.
       when 2 then do:
          ch-excel:visible = false.
          ch-excel:ActiveWorkbook:close(yes,c-arquivo).
       end.
       when 3 then do:
          ch-excel:visible = true.
       end.
   end case.

   release object ch-excel no-error.
END PROCEDURE.

/*     ch-excel:workbooks:item(1):SaveAs(c-planilha-envio,,,,,,). */
/*     ch-excel:range("A1"):select. */
/*    */
/*     ch-excel:ActiveSheet:Enableselection ="1". */
/*     ch-excel:DisplayAlerts = false. */
/*     if  tt-param.destino = 3 then */
/*         assign ch-excel:visible = yes. /* Mostra a planilha Excel na tela */ */
/*     else */
/*         ch-excel:quit. */
/*    */
/*     release object ch-excel. */
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/

/********************************************************************************************
**  Programa: escc002
**  Data....: 04 de Julho de 2016
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Relat¢rio Hist¢rico de Contratos
**  VersÒo..: 2.06.00.002 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versÒo */
{include/i-prgvrs.i ESCC002RP 2.06.00.002}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* defini¯Êo das temp-tables para recebimento de parmetros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELDS cod-estabel-ini          LIKE contrato-for.cod-estabel    
    FIELDS cod-estabel-fim          LIKE contrato-for.cod-estabel    
    FIELDS cod-estab-gestor-ini     LIKE contrato-for.cod-estab-gestor
    FIELDS cod-estab-gestor-fim     LIKE contrato-for.cod-estab-gestor
    FIELDS nr-contrato-ini          LIKE contrato-for.nr-contrato    
    FIELDS nr-contrato-fim          LIKE contrato-for.nr-contrato    
    FIELDS dt-ini-validade-ini      LIKE contrato-for.dt-ini-validade      
    FIELDS dt-ini-validade-fim      LIKE contrato-for.dt-ini-validade      
    FIELDS dt-ter-validade-ini      LIKE contrato-for.dt-ter-validade      
    FIELDS dt-ter-validade-fim      LIKE contrato-for.dt-ter-validade      
    FIELDS gestor-tecnico-ini       LIKE contrato-for.gestor-tecnico 
    FIELDS gestor-tecnico-fim       LIKE contrato-for.gestor-tecnico 
    FIELDS l-csv                    AS   LOGICAL.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.
/********************************************************************************************/

/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de parmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param.
/********************************************************************************************/

/********************************** DEFINICAO DE VARIAVEIS **********************************/
define new global shared variable v_cod_usuar_corren as character no-undo.
define variable h-acomp        as handle    no-undo.

{include/i-rpvar.i}

DEFINE STREAM st-csv.
/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST ems2cadme.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESCC002RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.002"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "CCP"
	   c-titulo-relat   = "Relat¢rio de Hist¢rico de Contratos".

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

/* include padrÒo para output de relat®rios */
{include/i-rpout.i}

IF tt-param.l-csv THEN
   RUN pi-impressao-csv.
ELSE
   RUN pi-impressao.

/* fechamento do output do relat®rio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
procedure pi-impressao:
   DEFINE VARIABLE c-narrativa       AS CHARACTER         NO-UNDO.
   DEFINE VARIABLE c-arquivo         AS CHARACTER         NO-UNDO.
   DEFINE VARIABLE ch-excel          AS COMPONENT-HANDLE  NO-UNDO.
   DEFINE VARIABLE i-cont-linha      AS INTEGER           NO-UNDO.
   DEFINE VARIABLE de-saldo-contrato AS DECIMAL           NO-UNDO.
   DEFINE VARIABLE d-saldo-contrato  AS DECIMAL           NO-UNDO.

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".xls".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "ESCC002".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Hist¢rico de Contratos  - ESCC002 -  "  + STRING(TODAY).

   ASSIGN i-cont-linha = 3.

   ASSIGN ch-excel:columns( "A"):NumberFormat = "@"
          ch-excel:columns( "B"):NumberFormat = "@"
          ch-excel:columns( "C"):NumberFormat = "@"
          ch-excel:columns( "D"):NumberFormat = "@"
          ch-excel:columns( "E"):NumberFormat = "@"
          ch-excel:columns( "F"):NumberFormat = "@"           
          ch-excel:columns( "G"):NumberFormat = "@"           
          ch-excel:columns( "H"):NumberFormat = "dd/mm/aaaa;@"    
          ch-excel:columns( "I"):NumberFormat = "dd/mm/aaaa;@"    
          ch-excel:columns( "J"):NumberFormat = "@"    
          ch-excel:columns( "K"):NumberFormat = "@"
          ch-excel:columns( "L"):NumberFormat = "#.##0,00"
          ch-excel:columns( "M"):NumberFormat = "#.##0,00"
          ch-excel:columns( "N"):NumberFormat = "#.##0,00"
          ch-excel:columns( "O"):NumberFormat = "#.##0,00"
          ch-excel:columns( "P"):NumberFormat = "@"           
          ch-excel:columns( "Q"):NumberFormat = "@"           
          ch-excel:columns( "R"):NumberFormat = "@"           
          ch-excel:columns( "S"):NumberFormat = "@"           
          ch-excel:columns( "T"):NumberFormat = "@".           


   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Estab"
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = "Estab Gestor"
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = "Nr Contrato "
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = "Gestor Tec."      
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = "Cod.Comprador"    
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = "Cod Forn. "       
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = "Nome"          
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "Data In¡cio "
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = "Data T‚rmino "    
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = "Moeda "           
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = "Descri‡Æo"
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = "Vl. Contrato "    
          ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = "Vl. Medido "      	
          ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = "Saldo Contrato "  
          ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = "Aguardando Pagto "
          ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = "Cond Pagto "      
          ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = "Natureza"         
          ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = "Descri‡Æo"        
          ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = "Status"           
          ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = "Ativo"            
       .           

   ASSIGN i-cont-linha = i-cont-linha + 1.

   blk:
   FOR EACH contrato-for
            WHERE contrato-for.nr-contrato >= tt-param.nr-contrato-ini AND
                  contrato-for.nr-contrato <= tt-param.nr-contrato-fim
            NO-LOCK:

       IF contrato-for.cod-estabel < tt-param.cod-estabel-ini OR
          contrato-for.cod-estabel > tt-param.cod-estabel-fim THEN NEXT blk.

       IF contrato-for.cod-estab-gestor < tt-param.cod-estab-gestor-ini OR
          contrato-for.cod-estab-gestor > tt-param.cod-estab-gestor-fim THEN NEXT blk.

       IF contrato-for.dt-ini-validade < tt-param.dt-ini-validade-ini OR
          contrato-for.dt-ini-validade > tt-param.dt-ini-validade-fim THEN NEXT blk.

       IF contrato-for.dt-ter-validade < tt-param.dt-ter-validade-ini OR
          contrato-for.dt-ter-validade > tt-param.dt-ter-validade-fim THEN NEXT blk.

       IF contrato-for.gestor-tecnico < tt-param.gestor-tecnico-ini OR
          contrato-for.gestor-tecnico > tt-param.gestor-tecnico-fim THEN NEXT blk.


      RUN pi-acompanhar IN h-acomp (INPUT "Contrato : " + STRING(contrato-for.nr-contrato)).  

      FIND moeda OF contrato-for
           NO-LOCK NO-ERROR.
      FIND emitente OF contrato-for 
           NO-LOCK NO-ERROR.

      ASSIGN de-saldo-contrato = 0
             d-saldo-contrato  = 0.

      FOR EACH ITEM-contrat
               WHERE item-contrat.nr-contrato = contrato-for.nr-contrato 
               AND   item-contrat.ind-caract-item = 1 NO-LOCK:
         ASSIGN de-saldo-contrato = de-saldo-contrato + item-contrat.sld-val.
      END.
          
      IF contrato-for.dec-2 <> 0 THEN  
         ASSIGN d-saldo-contrato = contrato-for.dec-2 - de-saldo-contrato .

      ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.cod-estabel
             ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.cod-estab-gestor
             ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.nr-contrato                                
             ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.gestor-tecnico                             
             ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.cod-comprado                               
             ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.cod-emitente                               
             ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = IF AVAILABLE(emitente) THEN emitente.nome-abrev ELSE ""
             ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.dt-ini-validade                            
             ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.dt-ter-validade                            
             ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.mo-codigo                                  
             ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = IF AVAILABLE(moeda) THEN moeda.descricao ELSE ""
             ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.dec-2                                       
             ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.sld-val                                     
             ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = d-saldo-contrato
             ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.sld-val-liber
             ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = contrato-for.cod-cond-pag                                
             ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = ENTRY(contrato-for.natureza,{ininc/i01in274.i 03})                                                  
             ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = REPLACE(REPLACE(REPLACE(contrato-for.narrat-contrat,CHR(10)," "),CHR(13)," "),'"',"")                                                
             ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = ENTRY(contrato-for.ind-sit-contr,{ininc/i05in065.i 03})                                              
             ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = (IF contrato-for.log-libera = YES THEN "SIM" ELSE "NAO").
             
      ASSIGN i-cont-linha = i-cont-linha + 1.
   END.

   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

/*   ch-excel:Range("A1:T" + string(i-cont-linha - 1)):autofilter(,,,).*/

   ch-excel:Range("A1:T" + string(i-cont-linha - 1)):select.

   ch-excel:Range("A1:T3"):Font:Bold = TRUE.
       
   ch-excel:Columns("A"):ColumnWidth = 12.


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

procedure pi-impressao-csv:
   DEFINE VARIABLE c-narrativa       AS CHARACTER         NO-UNDO.
   DEFINE VARIABLE c-arquivo         AS CHARACTER         NO-UNDO.
   DEFINE VARIABLE ch-excel          AS COMPONENT-HANDLE  NO-UNDO.
   DEFINE VARIABLE i-cont-linha      AS INTEGER           NO-UNDO.
   DEFINE VARIABLE de-saldo-contrato AS DECIMAL           NO-UNDO.
   DEFINE VARIABLE d-saldo-contrato  LIKE contrato-for.dec-2           NO-UNDO.

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".csv".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-csv TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   PUT STREAM st-csv
       "Estab"             ";"
       "Estab Gestor"      ";"
       "Nr Contrato "      ";"
       "Gestor Tec."       ";"
       "Cod.Comprador"     ";"
       "Cod Forn. "        ";"
       "Nome"              ";"
       "Data In¡cio "      ";"
       "Data T‚rmino "     ";"
       "Moeda "            ";"
       "Descri‡Æo"         ";"
       "Vl. Contrato "     ";"
       "Vl. Medido "       ";"
       "Saldo Contrato "   ";"
       "Aguardando Pagto " ";"
       "Cond Pagto "       ";"
       "Natureza"          ";"
       "Descri‡Æo"         ";"
       "Status"            ";"
       "Ativo"             ";" SKIP.           

   blk:
   FOR EACH contrato-for
            WHERE contrato-for.nr-contrato >= tt-param.nr-contrato-ini AND
                  contrato-for.nr-contrato <= tt-param.nr-contrato-fim
            NO-LOCK:

       IF contrato-for.cod-estabel < tt-param.cod-estabel-ini OR
          contrato-for.cod-estabel > tt-param.cod-estabel-fim THEN NEXT blk.

       IF contrato-for.cod-estab-gestor < tt-param.cod-estab-gestor-ini OR
          contrato-for.cod-estab-gestor > tt-param.cod-estab-gestor-fim THEN NEXT blk.

       IF contrato-for.dt-ini-validade < tt-param.dt-ini-validade-ini OR
          contrato-for.dt-ini-validade > tt-param.dt-ini-validade-fim THEN NEXT blk.

       IF contrato-for.dt-ter-validade < tt-param.dt-ter-validade-ini OR
          contrato-for.dt-ter-validade > tt-param.dt-ter-validade-fim THEN NEXT blk.

       IF contrato-for.gestor-tecnico < tt-param.gestor-tecnico-ini OR
          contrato-for.gestor-tecnico > tt-param.gestor-tecnico-fim THEN NEXT blk.


      RUN pi-acompanhar IN h-acomp (INPUT "Contrato : " + STRING(contrato-for.nr-contrato)).  

      FIND moeda OF contrato-for
           NO-LOCK NO-ERROR.
      FIND emitente OF contrato-for 
           NO-LOCK NO-ERROR.

      ASSIGN de-saldo-contrato = 0
             d-saldo-contrato  = 0.

      FOR EACH ITEM-contrat
               WHERE item-contrat.nr-contrato = contrato-for.nr-contrato 
               AND   item-contrat.ind-caract-item = 1 NO-LOCK:
         ASSIGN de-saldo-contrato = de-saldo-contrato + item-contrat.sld-val.
      END.
          
      IF contrato-for.dec-2 <> 0 THEN  
         ASSIGN d-saldo-contrato = contrato-for.dec-2 - de-saldo-contrato .

      PUT STREAM st-csv
          contrato-for.cod-estabel                                               ";"
          contrato-for.cod-estab-gestor                                          ";"
          contrato-for.nr-contrato                                               ";"
          contrato-for.gestor-tecnico                                            ";"
          contrato-for.cod-comprado                                              ";"
          contrato-for.cod-emitente                                              ";"
          IF AVAILABLE(emitente) THEN emitente.nome-abrev ELSE ""                ";"
          contrato-for.dt-ini-validade                                           ";"
          contrato-for.dt-ter-validade                                           ";"
          contrato-for.mo-codigo                                                 ";"
          IF AVAILABLE(moeda) THEN moeda.descricao ELSE ""                       ";"
          contrato-for.dec-2                                                     ";"
          contrato-for.sld-val                                                   ";"
          d-saldo-contrato                                                       ";"
          contrato-for.sld-val-liber                                             ";"
          contrato-for.cod-cond-pag                                              ";"
          ENTRY(contrato-for.natureza,{ininc/i01in274.i 03})                     ";"                             
          REPLACE(REPLACE(REPLACE(contrato-for.narrat-contrat,CHR(10)," "),CHR(13)," "),'"',"") ";"                                              
          ENTRY(contrato-for.ind-sit-contr,{ininc/i05in065.i 03})                ";"                              
          (IF contrato-for.log-libera = YES THEN "SIM" ELSE "NAO")               ";" SKIP. 
   END.

   OUTPUT STREAM st-csv CLOSE.

   IF i-num-ped-exec-rpw = 0 THEN
      DOS SILENT START excel.exe VALUE(c-arquivo).

END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/

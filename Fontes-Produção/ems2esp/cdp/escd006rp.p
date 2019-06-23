/********************************************************************************************
**  Programa: escd006rp
**  Data....: 08 de Outubro de 2015
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Extrator de Lan‡amentos Cont beis
**  Versão..: 2.06.00.001 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versão */
{include/i-prgvrs.i ESCD006RP 12.1.13.001}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* definiîÒo das temp-tables para recebimento de par±metros */
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
    FIELDS dat_lancto_ctbl_ini AS DATE FORMAT "99/99/9999"
    FIELDS dat_lancto_ctbl_fim AS DATE FORMAT "99/99/9999"
    FIELDS cod_modul_dtsul_ini AS CHAR FORMAT "x(3)"
    FIELDS cod_modul_dtsul_fim AS CHAR FORMAT "x(3)"
    FIELDS cod_usuario_ini     AS CHAR FORMAT "x(12)"
    FIELDS cod_usuario_fim     AS CHAR FORMAT "x(12)"
    FIELDS rs-formato-arquivo   AS INTEGER.
    .

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.
/********************************************************************************************/

/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de par±metros */
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

def new global shared var v_cod_empres_usuar as CHARACTER NO-UNDO.

DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.

DEFINE VARIABLE v-des_histor_lancto_ctbl LIKE item_lancto_ctbl.des_histor_lancto_ctbl NO-UNDO.
DEFINE VARIABLE d-val_lancto_ctbl        LIKE aprop_lancto_ctbl.val_lancto_ctbl       NO-UNDO.
   
/********************************************************************************************/

/************************************ DEFINICAO DE STREAMS **********************************/
DEFINE STREAM st-csv.
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
/* FIND FIRST empresa     */
/*      NO-LOCK NO-ERROR. */

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESCD006RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.001"
/*        c-empresa        = empresa.razao-social */
	   c-sistema	    = "TMS"
	   c-titulo-relat   = "Extrator de Lan‡amentos Cont beis".

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

/* include padrão para output de relat©rios */
{include/i-rpout.i}

/* RUN pi-impressao-csv. */

IF tt-param.rs-formato-arquivo = 2 THEN DO:
   RUN pi-abre-csv.
   RUN pi-dados.
   RUN pi-encerra-csv.
END.
ELSE DO:
   RUN pi-abre-excel.
   RUN pi-dados.
   RUN pi-encerra-excel.
END.
   


/* fechamento do output do relat©rio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
procedure pi-abre-excel:
   ASSIGN tt-param.arquivo =  SESSION:TEMP-DIRECTORY + 'RELT01AB_' + REPLACE (STRING(TODAY,'99/99/9999'),'/','') + REPLACE (STRING(TIME,'hh:mm:ss'),':','') + ".xlsx".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = tt-param.arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "ESCD006".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:columns( "A"):NumberFormat = "#.##0"
          ch-excel:columns( "B"):NumberFormat = "#.##0"
          ch-excel:columns( "C"):NumberFormat = "#.##0"
          ch-excel:columns( "D"):NumberFormat = "@"
          ch-excel:columns( "E"):NumberFormat = "@"
          ch-excel:columns( "F"):NumberFormat = "@"           
          ch-excel:columns( "G"):NumberFormat = "dd/mm/aaaa;@"           
          ch-excel:columns( "H"):NumberFormat = "#.##0"    
          ch-excel:columns( "I"):NumberFormat = "@"    
          ch-excel:columns( "J"):NumberFormat = "#.##0"    
          ch-excel:columns( "K"):NumberFormat = "#.##0,00"
          ch-excel:columns( "L"):NumberFormat = "@"
          ch-excel:columns( "M"):NumberFormat = "@"
          ch-excel:columns( "N"):NumberFormat = "@".

   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Nro. Lote Ctbl"     
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = "Nro. Lancamento"    
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = "Sequencia Lacto"    
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = "Empresa"            
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = "Cod. Modulo"        
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = "Cod. Estabel."      
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = "Data Lancamento"    
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "ID Debito e Credito"
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = "Descricao"          
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = "ID Conta Contabil"  
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = "Valor"              
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = "Moeda"
          ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = "Usuario"
          ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = "Nome".

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-dados:
   blk:
   FOR EACH lancto_ctbl 
            WHERE lancto_ctbl.ind_sit_lancto_ctbl = "Ctbz"                    AND
                  lancto_ctbl.cod_empresa         = v_cod_empres_usuar        AND
                  lancto_ctbl.dat_lancto_ctbl >= tt-param.dat_lancto_ctbl_ini AND /* DATA DE INICIO DOS LANCAMENTOS */
                  lancto_ctbl.dat_lancto_ctbl <= tt-param.dat_lancto_ctbl_fim
            NO-LOCK:

      IF (lancto_ctbl.cod_modul_dtsul < tt-param.cod_modul_dtsul_ini) OR
         (lancto_ctbl.cod_modul_dtsul > tt-param.cod_modul_dtsul_fim) THEN 
          NEXT blk.

      FIND FIRST lote_ctbl OF lancto_ctbl 
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(LOTE_CTBL) THEN 
         NEXT blk.

     IF (lote_ctbl.cod_usuar_ult_atualiz < tt-param.cod_usuario_ini) OR
         (lote_ctbl.cod_usuar_ult_atualiz > tt-param.cod_usuario_fim) THEN
          NEXT blk.

      FIND FIRST usuar_mestre
           WHERE usuar_mestre.cod_usuar = lote_ctbl.cod_usuar_ult_atualiz
           NO-LOCK NO-ERROR.

      FOR EACH item_lancto_ctbl OF lancto_ctbl 
               WHERE item_lancto_ctbl.cod_cenar_ctbl = " " OR
                     item_lancto_ctbl.cod_cenar_ctbl = "CONTSOC"
               NO-LOCK:

         RUN pi-acompanhar IN h-acomp (INPUT "Carregando Lote:" + STRING(lancto_ctbl.num_lote_ctbl) + "-" + STRING(item_lancto_ctbl.num_lancto_ctbl) + "-" + STRING(item_lancto_ctbl.num_seq_lancto_ctbl) + "-" + STRING(lancto_ctbl.dat_lancto_ctbl,"99/99/9999")).

         IF item_lancto_ctbl.cod_indic_econ  <> "Real" THEN
             FIND FIRST aprop_lancto_ctbl OF item_lancto_ctbl 
                  NO-LOCK NO-ERROR.

         ASSIGN v-des_histor_lancto_ctbl = replace(replace(REPLACE(REPLACE(REPLACE(item_lancto_ctbl.des_histor_lancto_ctbl,CHR(10),""),CHR(9),""),CHR(13),""),";"," "),","," ").

         ASSIGN d-val_lancto_ctbl = IF AVAIL aprop_lancto_ctbl AND item_lancto_ctbl.cod_indic_econ  <> "Real" THEN aprop_lancto_ctbl.val_lancto_ctbl ELSE item_lancto_ctbl.val_lancto_ctbl.

         IF tt-param.rs-formato-arquivo = 1 THEN DO:
            ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = lote_ctbl.num_lote_ctbl
                   ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.num_lancto_ctbl
                   ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.num_seq_lancto_ctbl
                   ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.cod_empresa
                   ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = lote_ctbl.cod_modul_dtsul
                   ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.cod_estab
                   ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.dat_lancto_ctbl
                   ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.ind_natur_lancto_ctbl
                   ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = v-des_histor_lancto_ctbl
                   ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.cod_cta_ctbl
                   ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = d-val_lancto_ctbl
                   ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = item_lancto_ctbl.cod_indic_econ
                   ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = lote_ctbl.cod_usuar_ult_atualiz
                   ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = IF AVAILABLE usuar_mestre THEN usuar_mestre.nom_usuar ELSE "".

            ASSIGN i-cont-linha = i-cont-linha + 1.
         END.
         ELSE DO:
             PUT STREAM st-csv
                 lote_ctbl.num_lote_ctbl                  ";"
                 item_lancto_ctbl.num_lancto_ctbl         ";"
                 item_lancto_ctbl.num_seq_lancto_ctbl     ";"
                 item_lancto_ctbl.cod_empresa             ";"
                 lote_ctbl.cod_modul_dtsul                ";"
                 item_lancto_ctbl.cod_estab               ";"
                 item_lancto_ctbl.dat_lancto_ctbl         ";"
                 item_lancto_ctbl.ind_natur_lancto_ctbl   ";"
                 v-des_histor_lancto_ctbl                 ";"
                 item_lancto_ctbl.cod_cta_ctbl            ";"
                 d-val_lancto_ctbl                        ";"
                 item_lancto_ctbl.cod_indic_econ          ";"
                 lote_ctbl.cod_usuar_ult_atualiz          ";" 
                 IF AVAILABLE usuar_mestre THEN usuar_mestre.nom_usuar ELSE "" FORMAT "x(50)" SKIP.
         END.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   ch-excel:Range("A1:N" + string(i-cont-linha - 1)):autofilter(,,,).

   ch-excel:Range("A1:N" + string(i-cont-linha - 1)):select.

   case tt-param.destino:
       when 1 then do:
          ch-excel:worksheets:item(1):select.
          ch-excel:Sheets:PrintOut.
          ch-excel:application:DisplayAlerts = false.
          ch-excel:quit().
       end.
       when 2 then do:
          ch-excel:visible = false.
          ch-excel:ActiveWorkbook:close(yes,tt-param.arquivo).
       end.
       when 3 then do:
          ch-excel:visible = true.
       end.
   end case.

   release object ch-excel no-error.
END PROCEDURE.


PROCEDURE pi-encerra-csv:
   OUTPUT CLOSE.

   IF i-num-ped-exec-rpw = 0 THEN
      DOS SILENT START excel.exe VALUE(tt-param.arquivo).
END PROCEDURE.

PROCEDURE pi-abre-csv:
   ASSIGN tt-param.arquivo =  SESSION:TEMP-DIRECTORY + 'ESCD006_' + REPLACE (STRING(TODAY,'99/99/9999'),'/','') + REPLACE (STRING(TIME,'hh:mm:ss'),':','') + ".csv".
    
   FILE-INFO:FILE-NAME = tt-param.arquivo.
   
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
   
   OUTPUT STREAM st-csv TO VALUE (tt-param.arquivo) NO-CONVERT.

   PUT STREAM st-csv
       "Nro. Lote Ctbl"      ";" 
       "Nro. Lancamento"     ";" 
       "Sequencia Lacto"     ";" 
       "Empresa"             ";" 
       "Cod. Modulo"         ";" 
       "Cod. Estabel."       ";" 
       "Data Lancamento"     ";" 
       "ID Debito e Credito" ";" 
       "Descricao"           ";" 
       "ID Conta Contabil"   ";" 
       "Valor"               ";" 
       "Moeda"               ";" 
       "Usuario"             ";" 
       "Nome"                ";" SKIP.             
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/

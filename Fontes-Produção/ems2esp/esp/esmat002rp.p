/************************************************************************
**
**  Programa: ESMAT002RP
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

{include/i-prgvrs.i ESMAT002RP 12.1.17.001}

/* Definicao de temp-table */
   

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
    FIELD dt-base          AS DATE FORMAT "99/99/9999"
    FIELD dt-base-fim      AS DATE FORMAT "99/99/9999"
    FIELD fi-cc-ini        AS CHAR FORMAT "x(8)"
    FIELD fi-cc-fim        AS CHAR FORMAT "x(8)"
    FIELD fi-item-ini      AS CHAR FORMAT "x(16)"
    FIELD fi-item-fim      AS char FORMAT "x(16)"
    FIELD rs-tp-contr      AS INT
    FIELD fi-est-ini       AS CHAR FORMAT "x(3)"
    FIELD fi-est-fim       AS CHAR FORMAT "x(3)"
    FIELD l-excel          AS LOGICAL
    FIELD l-coluna         AS LOGICAL.       


DEFINE TEMP-TABLE tt-controle  NO-UNDO 
 FIELD tipo-contr     AS   INTEGER
 FIELD dt-trans       LIKE movto-estoq.dt-trans
 FIELD sc-codigo      LIKE movto-estoq.sc-codigo
 field it-codigo      LIKE movto-estoq.it-codigo               
 FIELD descricao      AS CHAR FORMAT "x(32)" 
 FIELD cod-estabel    LIKE movto-estoq.cod-estabel             
 FIELD conta-contabil LIKE movto-estoq.conta-contabil        
 FIELD qtde-cons      LIKE movto-estoq.quantidade            
 FIELD pr-unit-per    LIKE pr-it-per.val-unit-mat-m[1]                                   
 FIELD pr-tot         AS DEC FORMAT "->>>>>,>>>,>>9.9999"
 FIELD nr-docto       LIKE movto-estoq.nro-docto
 FIELD un             LIKE ITEM.un
 FIELD desc-conta     LIKE cta_ctbl.des_tit_ctbl
 FIELD desc-sc-codigo LIKE centro-custo.descricao
 FIELD tipo-controle  AS   CHARACTER FORMAT "x(20)"
 FIELD esp-docto      LIKE movto-estoq.esp-docto
 FIELD tipo-trans     LIKE movto-estoq.tipo-trans
 FIELD desc-esp-docto  AS   CHARACTER FORMAT "X(3)"
 FIELD desc-tipo-trans AS   CHARACTER FORMAT "X(8)".
 .

def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro  as integer format ">>>>,>>9" label "Námero" column-label "Námero"
    field ttv_des_msg_ajuda as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro  as character format "x(60)" label "Mensagem Erro" column-label "Inconsistªncia".
    
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
    
    def var h_api_cta          as handle no-undo.
def var h_api_ccust        as handle no-undo.
def var p_cod_format_cta_ctbl   as char   no-undo.
DEF VAR p_cod_format_ccusto     AS CHAR NO-UNDO.
DEF VAR v_des_cta_ctbl        AS CHAR NO-UNDO.
DEF VAR v_num_tip_cta_ctbl AS CHAR NO-UNDO.
DEF VAR v_num_sit_cta_ctbl AS CHAR NO-UNDO.
DEF VAR v_ind_finalid_cta AS CHAR NO-UNDO.
DEF VAR v_des_ccusto AS CHAR NO-UNDO.

def var c-conta   like movto-estoq.ct-codigo NO-UNDO.
def var sc-conta   like movto-estoq.sc-codigo NO-UNDO.

   DEFINE VARIABLE i-empresa    LIKE estabelec.ep-codigo   NO-UNDO.
   
def var de-valor-mat as decimal format "->>>>>>>,>>9.9999"    no-undo.
def var de-valor-mob as decimal format "->>>>>>>,>>9.9999"    no-undo.
def var de-valor-ggf as decimal format "->>>>>>>,>>9.9999"    no-undo.
def var de-total     as decimal format "->>>>>>,>>>,>>9.9999" no-undo.   
       

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
DEF VAR da-data          AS DATE FORMAT "99/99/9999"  NO-UNDO.
DEF VAR i-mes-1          AS INT  FORMAT 99            NO-UNDO.
DEF VAR i-qtd-meses      AS INT  FORMAT 99            NO-UNDO.
DEF VAR tot-qtde         LIKE movto-estoq.quantidade .

/* Variaveis para Gerar em Excel */

DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.

/* Processamento */
FIND FIRST tt-param NO-ERROR.

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa NO-LOCK
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-ERROR.

ASSIGN c-programa     = "ESMAT002RP"
       c-versao       = "12.1.17"
       c-revisao      = "001"
       c-empresa      = empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "Relatorio de Materiais".

/* Definicao de LayOut do Relatorio */

/* Processamento das Informacoes */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar in h-acomp (INPUT "Processando").
RUN pi-gera-tabela.
RUN pi-abre-excel.
RUN pi-excel.
RUN pi-encerra-excel.
RUN pi-finalizar IN h-acomp.

RETURN "OK":U.

PROCEDURE pi-abre-excel:
   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".xlsx".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "ESMAT002".
   ch-excel:SheetsInNewWorkbook = 1.

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   IF i-cont-linha <> 1 THEN DO:
      ch-excel:Range("A1:O" + string(i-cont-linha - 1)):autofilter(,,,).
   END.

   ch-excel:Range("A1:O" + string(i-cont-linha - 1)):select.

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

PROCEDURE pi-gera-tabela:
   DEFINE VARIABLE d-data AS DATE NO-UNDO.

   EMPTY TEMP-TABLE tt-controle.
   
   RUN prgint/utb/utb743za.py PERSISTENT SET h_api_cta.
   RUN prgint/utb/utb742za.py PERSISTENT SET h_api_ccust.   
   
   DO d-data = tt-param.dt-base TO tt-param.dt-base-fim:

      blk:
      FOR EACH movto-estoq  
               USE-INDEX data-item
               WHERE movto-estoq.dt-trans     = d-data                AND
                     movto-estoq.it-codigo   >= tt-param.fi-item-ini  AND
                     movto-estoq.it-codigo   <= tt-param.fi-item-fim  AND
                     movto-estoq.cod-estabel >= tt-param.fi-est-ini   AND 
                     movto-estoq.cod-estabel <= tt-param.fi-est-fim   AND
                     movto-estoq.sc-codigo   >= tt-param.fi-cc-ini    AND       
                     movto-estoq.sc-codigo   <= tt-param.fi-cc-fim
               NO-LOCK:
 
         RUN pi-acompanhar IN h-acomp("Movimento (Data/Item): " + STRING(movto-estoq.dt-trans,"99/99/9999") + "/" + movto-estoq.it-codigo).
                  
         FIND item OF movto-estoq
              NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(item) THEN 
            NEXT blk.
                     
         IF (ITEM.tipo-contr = 2 OR ITEM.tipo-contr = 3) THEN DO:
            IF movto-estoq.esp-docto = 5  OR 
               movto-estoq.esp-docto = 7  OR 
               movto-estoq.esp-docto = 28 OR 
               movto-estoq.esp-docto = 30 THEN DO:
               RUN pi-cria-tabela.
            END.
         END.
         ELSE DO:
            IF (movto-estoq.esp-docto  = 21 OR movto-estoq.esp-docto = 18) AND movto-estoq.tipo-trans = 2 THEN DO:       
               IF movto-estoq.ct-codigo = "13601002" OR
                  movto-estoq.ct-codigo = "17202002" OR
                  movto-estoq.ct-codigo = "11511004" OR
                  movto-estoq.ct-codigo = "17202014" OR
                  movto-estoq.ct-codigo = "41106002" OR
                  movto-estoq.ct-codigo = "11508202" THEN
                  NEXT blk.  
                            
               RUN pi-cria-tabela.         
               
            END.
         END.
      END.
   END.    /* DO d-data */
      
   DELETE PROCEDURE  h_api_cta.
   DELETE PROCEDURE  h_api_ccust.

END PROCEDURE.

PROCEDURE pi-cria-tabela:
   CREATE tt-controle.                                                        
   ASSIGN tt-controle.un             = ITEM.un 
          tt-controle.dt-trans       = movto-estoq.dt-trans                   
          tt-controle.tipo-contr     = ITEM.tipo-contr                        
          tt-controle.it-codigo      = movto-estoq.it-codigo                  
          tt-controle.descricao      = ITEM.descricao-1 + ITEM.descricao-2    
          tt-controle.cod-estabel    = movto-estoq.cod-estabel                
          tt-controle.conta-contabil = movto-estoq.ct-codigo    
          tt-controle.sc-codigo      = movto-estoq.sc-codigo
          tt-controle.qtde-cons      = IF movto-estoq.tipo-trans = 1 THEN (movto-estoq.quantidade * -1) ELSE movto-estoq.quantidade
          tt-controle.nr-docto       = movto-estoq.nro-docto
          tt-controle.tipo-contr     = ITEM.tipo-contr
          tt-controle.tipo-controle  = ENTRY(ITEM.tipo-contr,{ininc/i09in122.i 03})
          tt-controle.esp-docto      = movto-estoq.esp-docto
          tt-controle.desc-esp-docto = ENTRY(movto-estoq.esp-docto ,{ininc/i03in218.i 03})
          tt-controle.tipo-trans     = movto-estoq.tipo-trans
          tt-controle.desc-tipo-trans= ENTRY(movto-estoq.tipo-trans,{ininc/i01in218.i 03}). 
         
   ASSIGN tt-controle.pr-tot = IF movto-estoq.tipo-trans = 1 THEN ((valor-ggf-m[1] + valor-mat-m[1] + valor-mob-m[1]) * -1 ) ELSE (valor-ggf-m[1] + valor-mat-m[1] + valor-mob-m[1]).
   
   IF tt-controle.pr-tot = 0 THEN DO:
      FIND FIRST item-estab
           WHERE item-estab.cod-estabel = movto-estoq.cod-estabel AND
                 item-estab.it-codigo   = movto-estoq.it-codigo
           NO-LOCK NO-ERROR.
      IF AVAILABLE(item-estab) THEN DO: 
         ASSIGN tt-controle.pr-tot = movto-estoq.quantidade *
            IF movto-estoq.tipo-trans = 1 THEN 
               ((item-estab.val-unit-mat-m[1] + item-estab.val-unit-mob-m[1] + item-estab.val-unit-ggf-m[1]) * -1 ) 
            ELSE 
               (item-estab.val-unit-mat-m[1] + item-estab.val-unit-mob-m[1] + item-estab.val-unit-ggf-m[1]).          
   
      END.
   END.

   RUN pi-conta-contabil.
END PROCEDURE.

PROCEDURE pi-conta-contabil:
   FIND FIRST estabelec 
        WHERE estabelec.cod-estabel = movto-estoq.cod-estabel 
        NO-LOCK NO-ERROR.
   ASSIGN i-empresa = IF AVAILABLE(estabelec) THEN estabelec.ep-codigo ELSE "".

   ASSIGN c-conta = movto-estoq.ct-codigo
          sc-conta = movto-estoq.sc-codigo.

   ASSIGN v_des_ccusto   = ""
          v_des_cta_ctbl = "".


   RUN pi_busca_dados_cta_ctbl in h_api_cta (input        i-empresa,                 /* EMPRESA EMS2 */
                                             input        "",                        /* PLANO DE CONTAS */
                                             input-output c-conta,                   /* CONTA */
                                             input        today,                     /* DATA TRANSACAO */
                                             output       v_des_cta_ctbl,            /* DESCRICAO CONTA */
                                             output       v_num_tip_cta_ctbl,        /* TIPO DA CONTA */
                                             output       v_num_sit_cta_ctbl,        /* SITUA°€O DA CONTA */
                                             output       v_ind_finalid_cta,         /* FINALIDADES DA CONTA */
                                             output table tt_log_erro).              /* ERROS */


   ASSIGN tt-controle.desc-conta = v_des_cta_ctbl.




   run pi_busca_dados_ccusto in h_api_ccust (input  i-empresa,                       /* EMPRESA EMS2 */
                                             input  "",                              /* CODIGO DO PLANO CCUSTO */
                                             input  sc-conta,                        /* CCUSTO */
                                             input  today,                           /* DATA DE TRANSACAO */
                                             output v_des_ccusto,                    /* DESCRICAO DO CCUSTO */
                                             output table tt_log_erro).              /* ERROS */

   ASSIGN tt-controle.desc-sc-codigo = v_des_ccusto.
END PROCEDURE.


PROCEDURE pi-excel:
   ASSIGN i-cont-linha = 1.

   FOR EACH tt-controle
            NO-LOCK:
            
            
            
      RUN pi-acompanhar IN h-acomp("Montando Excel : " + STRING(i-cont-linha)).

            
      IF i-cont-linha = 1 THEN DO:
              ASSIGN ch-excel:range("A" + STRING(i-cont-linha)) = "Codigo do Item"
                     ch-excel:range("B" + STRING(i-cont-linha)) = "Descri‡Æo do Item"
                     ch-excel:range("C" + STRING(i-cont-linha)) = "UN"
                     ch-excel:range("D" + STRING(i-cont-linha)) = "Est"
                     ch-excel:range("E" + STRING(i-cont-linha)) = "Conta Cont bil"
                     ch-excel:range("F" + STRING(i-cont-linha)) = "Descri‡Æo"
                     ch-excel:range("G" + STRING(i-cont-linha)) = "Centro de Custo"
                     ch-excel:range("H" + STRING(i-cont-linha)) = "Descri‡Æo"
                     ch-excel:range("I" + STRING(i-cont-linha)) = "Quantidade"
                     ch-excel:range("J" + STRING(i-cont-linha)) = "Documento"
                     ch-excel:range("K" + STRING(i-cont-linha)) = "Valor"
                     ch-excel:range("L" + STRING(i-cont-linha)) = "Dt Trans"
                     ch-excel:range("M" + STRING(i-cont-linha)) = "Tipo Cont"
                     ch-excel:range("N" + STRING(i-cont-linha)) = "Esp Docto"
                     ch-excel:range("O" + STRING(i-cont-linha)) = "Tipo Trans".
                     
              ASSIGN i-cont-linha = i-cont-linha + 1.       
      
      END.
                 

     
              ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha)):NumberFormat = "@" 
                     ch-excel:Range( "B" + STRING(i-cont-linha)):NumberFormat = "@" 
                     ch-excel:Range( "C" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "D" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "E" + STRING(i-cont-linha)):NumberFormat = "@" 
                     ch-excel:Range( "F" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "G" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "H" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "I" + STRING(i-cont-linha)):NumberFormat = "#.##0,0000"
                     ch-excel:Range( "J" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "K" + STRING(i-cont-linha)):NumberFormat = "#.##0,0000"
                     ch-excel:Range( "L" + STRING(i-cont-linha)):NumberFormat = "dd/mm/aaaa;@"
                     ch-excel:Range( "M" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "N" + STRING(i-cont-linha)):NumberFormat = "@"
                     ch-excel:Range( "O" + STRING(i-cont-linha)):NumberFormat = "@".
                     

              ASSIGN ch-excel:range( "A" + STRING(i-cont-linha) ):value = tt-controle.it-codigo
                     ch-excel:range( "B" + STRING(i-cont-linha) ):value = tt-controle.descricao
                     ch-excel:range( "C" + STRING(i-cont-linha) ):value = tt-controle.un
                     ch-excel:range( "D" + STRING(i-cont-linha) ):value = tt-controle.cod-estabel
                     ch-excel:range( "E" + STRING(i-cont-linha) ):value = tt-controle.conta-contabil
                     ch-excel:range( "F" + STRING(i-cont-linha) ):value = tt-controle.desc-conta
                     ch-excel:range( "G" + STRING(i-cont-linha) ):value = tt-controle.sc-codigo
                     ch-excel:range( "H" + STRING(i-cont-linha) ):value = tt-controle.desc-sc-codigo
                     ch-excel:range( "I" + STRING(i-cont-linha) ):value = tt-controle.qtde-cons
                     ch-excel:range( "J" + STRING(i-cont-linha) ):value = tt-controle.nr-docto 
                     ch-excel:range( "K" + STRING(i-cont-linha) ):value = tt-controle.pr-tot
                     ch-excel:range( "L" + STRING(i-cont-linha) ):value = tt-controle.dt-trans
                     ch-excel:range( "M" + STRING(i-cont-linha) ):value = tt-controle.tipo-controle
                     ch-excel:range( "N" + STRING(i-cont-linha) ):value = tt-controle.desc-esp-docto
                     ch-excel:range( "O" + STRING(i-cont-linha) ):value = tt-controle.desc-tipo-trans.
                           
         
      ASSIGN i-cont-linha = i-cont-linha + 1.
   END.
END PROCEDURE.



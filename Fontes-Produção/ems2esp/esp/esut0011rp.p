/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESUT0011RP 12.01.19.001 }

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESUT0011rp MRE}
&ENDIF

{include/i_fnctrad.i}
/********************************************************************************
** Copyright DATASUL S.A. (1998)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*********************************************************************************/
/********************************************************************************
*              
*  ESUT0011RP.P - Remote Program da Importaá∆o de Documento     
*
*********************************************************************************
*                               ALTERAÄÂES                                      *
*********************************************************************************
* DATA ALTERAÄ«O: 23/05/2018
* AUTOR.........: WILLIANS M AMBROSIO - GRUPO DKP
* DESCRIÄ«O.....: APLICADA REGRA PARA RETIRAR O ENTER DO CABEÄALHO DO EXCEL
* REVIS«O.......: REV01
*********************************************************************************/
{utp/ut-glob.i}
{include/tt-edit.i}
{cdp/cd0666.i}

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arquivo          as char
    field arq-entrada1     as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD l-atualiza       AS LOGICAL.    

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEFINE TEMP-TABLE tt-item-contrat NO-UNDO LIKE item-contrat
       FIELDS preco-fornec-novo LIKE item-contrat.preco-fornec
       FIELDS r-rowid           AS ROWID
       FIELDS descricao         AS CHARACTER FORMAT "x(60)"
       FIELDS observacao        AS CHARACTER FORMAT "x(60)".

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

DEF VAR h-acomp          AS HANDLE NO-UNDO .

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}
/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}


def var c-serie-docto      like item-doc-est.serie-docto no-undo.
def var c-nro-docto        like item-doc-est.nro-docto no-undo.
def var c-cod-emitente     like item-doc-est.cod-emitente no-undo.
def var c-nat-operacao     like item-doc-est.nat-operacao no-undo.
def var c-cod-esp          like dupli-apagar.cod-esp no-undo.
def var c-parcela          like dupli-apagar.parcela no-undo.
def var i-seq              as integ no-undo initial 0.
def var i-seq-item         as integ no-undo initial 0.
def var c-char             as char  no-undo.

DEFINE BUFFER bf-hist-alter FOR hist-alter.


form tt-erro.cd-erro "-"   
     tt-erro.mensagem format "x(117)"
     with width 132 frame f-erros stream-io. 

{utp/ut-liter.i Erro *}
assign tt-erro.cd-erro:label in frame f-erros = trim(return-value).

{utp/ut-liter.i Descriá∆o *}
assign tt-erro.mensagem:label in frame f-erros = trim(return-value).     

def stream s-entrada.
def var c-linha          as character.
def var i-cont           as integer.
def var l-cabec          as logical.
def var l-num-automatica as log initial no.
def var l-erro           as log.
def var c-text-aux       as char no-undo.
def var i-seq-aux        as int no-undo init 0.
def var l-spp-nfe        as log no-undo.

/**** Inicio ****/   

/*********************  Chamada EPC TMS ***********************/ 
find first param-global no-lock no-error.
find first param-estoq  no-lock no-error.
find ems2cadme.empresa
    where empresa.ep-codigo = param-global.empresa-prin
    no-lock no-error.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0011RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.000"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Atualizaá∆o Preáo Item Contrato".

FORM HEADER 
     FILL("-", 212)        FORMAT "x(212)" SKIP 
     c-empresa 
     c-titulo-relat AT 090
     "Pagina:":U    AT 200 
     page-number    AT 208 FORMAT ">>>>9" SKIP 
     FILL("-", 192)        FORMAT "x(190)" 
     TODAY                 FORMAT "99/99/9999"
     "-" 
     STRING(TIME,"HH:MM:SS":U) SKIP 
     "Contrato     Seq Item             Descriá∆o                                                           Preáo Unit    Preáo Unit Final          Preáo Novo Observaá∆o" SKIP
     "--------- ------ ---------------- ------------------------------------------------------------ ----------------- ------------------- ------------------- -----------------------------------------------------------" SKIP
     
WITH  STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 212 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 212 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(212)"
  WITH STREAM-IO WIDTH 212 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/**************************************************************/

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Alteraá∆o_Preáos_Item_Contratos *}
run pi-inicializar in h-acomp (input  Return-value ).


RUN pi-importa-planilha.
RUN pi-cria-registros.
RUN pi-gera-log.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK".


PROCEDURE pi-importa-planilha:
   DEFINE VARIABLE ch-excel          AS COMPONENT-HANDLE  NO-UNDO.
   DEFINE VARIABLE i-cont-linha      AS INTEGER           NO-UNDO.
   DEFINE VARIABLE i-nr-contrato     AS INTEGER           NO-UNDO.
   DEFINE VARIABLE c-des-contrat     AS CHARACTER         NO-UNDO FORMAT "x(100)".
   DEFINE VARIABLE c-ind-sit         AS CHARACTER         NO-UNDO.
   /* BEGINS: REV02 */
   DEFINE VARIABLE cStringAux        AS CHARACTER         NO-UNDO.
   /* END: REV02 */
   EMPTY TEMP-TABLE tt-item-contrat.
   
   FILE-INFO:FILE-NAME = tt-param.arq-entrada1.

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:OPEN(FILE-INFO:FULL-PATHNAME).
   ch-Excel:Visible = FALSE.

   /* Navega na planilha e importa os dados */
   ASSIGN i-nr-contrato = 0
          c-des-contrat = ""
          c-ind-sit     = "".

   blk:
   DO i-cont-linha = 1 TO 10000:

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Nr Contrato:" THEN 
      DO:
         /* BEGINS: REV02 */
         ASSIGN cStringAux    = STRING(ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE)
                cStringAux    = REPLACE(cStringAux,CHR(13),"") 
                i-nr-contrato = INTEGER(cStringAux).
         /* END: REV02 */
         NEXT blk.
      END.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "des-contrat:" THEN 
      DO:
         /* BEGINS: REV02 */
         ASSIGN cStringAux    = STRING(ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE)
                cStringAux    = REPLACE(cStringAux,CHR(13),"") 
                c-des-contrat = STRING(cStringAux).
         /* END: REV02 */
         NEXT blk.
      END.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "c-ind-sit:" THEN DO:
         /* BEGINS: REV02 */
         ASSIGN cStringAux    = STRING(ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE)
                cStringAux    = REPLACE(cStringAux,CHR(13),"") 
                c-ind-sit     = STRING(cStringAux).
         /* END: REV02 */
         NEXT blk.
      END.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Seq It" THEN DO:
         NEXT blk.
      END.
  
      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "" OR 
         ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = ?  OR
         ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "?"  THEN DO:
         LEAVE blk.
      END.

      RUN pi-acompanhar IN h-acomp (INPUT "Importando : " +
                                    STRING(i-nr-contrato                                                      ) + "-" +
                                    STRING(INTEGER( ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE)) + "-" +
                                    STRING(         ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE)  + "-" +
                                    STRING(         ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE)  + "-" +
                                    STRING(DECIMAL (ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE)) + "-" +
                                    STRING(DECIMAL (ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE))
                                    ).

      CREATE tt-item-contrat.
      ASSIGN tt-item-contrat.num-seq-item      = INTEGER (ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE)
             tt-item-contrat.it-codigo         = ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE
             tt-item-contrat.descricao         = ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE
             tt-item-contrat.preco-fornec-novo = DECIMAL (ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE)
             tt-item-contrat.qtd-total         = DECIMAL (ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE)
             tt-item-contrat.nr-contrato       = i-nr-contrato.

      IF c-ind-sit <> "Emitido" THEN
         ASSIGN tt-item-contrat.observacao = "Situaá∆o do contrato n∆o permite alteraá∆o por esse programa : " + c-ind-sit.

      FIND FIRST item-contrat
           WHERE item-contrat.nr-contrato = tt-item-contrat.nr-contrato AND
                 item-contrat.it-codigo   = tt-item-contrat.it-codigo   AND
                 item-contrat.num-seq-item= tt-item-contrat.num-seq-item
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(item-contrat) THEN DO:
         ASSIGN tt-item-contrat.observacao = "Item do contrato n∆o encontrado : Item : " + tt-item-contrat.it-codigo + " Seq: " + STRING(tt-item-contrat.num-seq-item).
         NEXT blk.
      END.

      ASSIGN tt-item-contrat.preco-unit    = item-contrat.preco-unit
             tt-item-contrat.pre-unit-for  = item-contrat.pre-unit-for.

      IF tt-item-contrat.preco-fornec-novo = tt-item-contrat.pre-unit-for THEN
         ASSIGN tt-item-contrat.observacao = "Item : " + tt-item-contrat.it-codigo + " Seq: " + STRING(item-contrat.num-seq-item) + " n∆o ser† alterado.".
   END.


   /* all prompts will be shut off/on */
    /* this prevents unsaved check     */
    ch-Excel:DisplayAlerts = false.
    /* Exit Excel */
    ch-Excel:QUIT().
END PROCEDURE.


PROCEDURE pi-cria-registros:
   DISABLE TRIGGERS FOR LOAD OF item-contrat.

   FIND FIRST param-contrat 
        NO-LOCK NO-ERROR.

   FOR EACH tt-item-contrat
            WHERE tt-item-contrat.observacao = ""
            EXCLUSIVE-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Atualizando : " +
                                    STRING(tt-item-contrat.num-seq-item) + "-" +
                                    STRING(tt-item-contrat.it-codigo   ) + "-" +
                                    STRING(tt-item-contrat.descricao   ) + "-" +
                                    STRING(tt-item-contrat.preco-fornec) + "-" +
                                    STRING(tt-item-contrat.qtd-total   ) 
                                    ).


      IF AVAILABLE(param-contrat ) AND param-contrat.controle-aditivo THEN DO:
         FOR FIRST hist-alter 
             WHERE hist-alter.nr-contrato  = tt-item-contrat.nr-contrato  AND
                   hist-alter.num-seq-item = tt-item-contrat.num-seq-item AND 
                   hist-alter.num-seq-adit = 0 
             NO-LOCK:

              if  hist-alter.des-tipo-alter = "item-contrat.dat-base" 
              or  hist-alter.des-tipo-alter = "item-contrat.cod-cond-pag" 
              or  hist-alter.des-tipo-alter = "item-contrat.preco-fornec"  
              or  hist-alter.des-tipo-alter = "item-contrat.mo-codigo" 
              or  hist-alter.des-tipo-alter = "item-contrat.aliquota-ipi" 
              or  hist-alter.des-tipo-alter = "item-contrat.aliquota-icm" 
              or  hist-alter.des-tipo-alter = "item-contrat.aliquota-iss" 
              or  hist-alter.des-tipo-alter = "item-contrat.codigo-ipi" 
              or  hist-alter.des-tipo-alter = "item-contrat.codigo-icm" 
              or  hist-alter.des-tipo-alter = "item-contrat.val-frete" 
              or  hist-alter.des-tipo-alter = "item-contrat.perc-desconto" 
              or  hist-alter.des-tipo-alter = "item-contrat.taxa-financ" 
              or  hist-alter.des-tipo-alter = "item-contrat.val-taxa" 
              or  hist-alter.des-tipo-alter = "item-contrat.prazo-ent" 
              or  hist-alter.des-tipo-alter = "item-contrat.perc-multa-dia" 
              or  hist-alter.des-tipo-alter = "item-contrat.narrat-item"
              or  hist-alter.des-tipo-alter = "item-contrat.cod-comprado"
              or  hist-alter.des-tipo-alter = "item-contrat.contato" 
              or  hist-alter.des-tipo-alter = "item-contrat.dat-cotac"
              or  hist-alter.des-tipo-alter = "item-contrat.frequencia"
              or  hist-alter.des-tipo-alter = "item-contrat.frete" 
              or  hist-alter.des-tipo-alter = "item-contrat.log-libera" 
              or  hist-alter.des-tipo-alter = "item-contrat.nr-dias-taxa" 
              or  hist-alter.des-tipo-alter = "item-contrat.perc-multa-limite" 
              or  hist-alter.des-tipo-alter = "item-contrat.preco-base"
              or  hist-alter.des-tipo-alter = "item-contrat.pre-unit-for"
              or  hist-alter.des-tipo-alter = "item-contrat.preco-unit" 
              or  hist-alter.des-tipo-alter = "item-contrat.qtd-minima" 
              or  hist-alter.des-tipo-alter = "item-contrat.qtd-total" 
              or  hist-alter.des-tipo-alter = "item-contrat.val-fatur-minimo"
              or  hist-alter.des-tipo-alter = "item-contrat.val-total" 
              or  hist-alter.des-tipo-alter = "item-contrat.valor-descto" then do:
                {utp/ut-liter.i Item_no_Contrato}
                run utp/ut-msgs.p("msg",18113,trim(return-value)).

               ASSIGN tt-item-contrat.observacao = trim(return-value).
            END.
         END.
      END.

      IF tt-item-contrat.observacao = "" THEN DO:
         RUN pi-altera(1).
         RUN pi-altera(2).
      END.
   END.
END PROCEDURE.

PROCEDURE pi-altera:
   DEFINE INPUT PARAMETER ip-tipo AS INTEGER NO-UNDO.

   DEFINE VARIABLE c-des-tipo-alter AS CHARACTER NO-UNDO.
   DEFINE VARIABLE i-cont           AS INTEGER   NO-UNDO.
   DEFINE VARIABLE i-cont-alt       AS INTEGER   NO-UNDO.


   IF ip-tipo = 1 THEN
      ASSIGN c-des-tipo-alter = "item-contrat.pre-unit-for".
   ELSE
      ASSIGN c-des-tipo-alter = "item-contrat.preco-fornec".

   ASSIGN i-cont     = 1
          i-cont-alt = 1.

   blk:
   FOR EACH bf-hist-alter
            WHERE bf-hist-alter.nr-contrato   = tt-item-contrat.nr-contrato   AND
                  bf-hist-alter.num-seq-item  = tt-item-contrat.num-seq-item  
            BY bf-hist-alter.num-seq-alter DESCENDING:
      ASSIGN i-cont = bf-hist-alter.num-seq-alter + 1.
      LEAVE blk.
   END.

   blk1:
   FOR EACH bf-hist-alter
            WHERE bf-hist-alter.nr-contrato   = tt-item-contrat.nr-contrato   AND
                  bf-hist-alter.num-seq-item  = tt-item-contrat.num-seq-item  
            BY bf-hist-alter.nr-alter DESCENDING:
      ASSIGN i-cont-alt = bf-hist-alter.nr-alter + 1.
      LEAVE blk1.
   END.


   FIND FIRST contrato-for 
        WHERE contrato-for.nr-contrato = tt-item-contrat.nr-contrato
        NO-LOCK NO-ERROR.
   IF AVAILABLE(contrato-for) THEN DO:
      CREATE hist-alter.
      ASSIGN hist-alter.num-seq-adit        = 0       
             hist-alter.usuario             = c-seg-usuario       
             hist-alter.cod-emitente        = contrato-for.cod-emitente       
             hist-alter.nr-contrato         = contrato-for.nr-contrato       
             hist-alter.hra-alter           = STRING(TIME,"hh:mm:ss")       
             hist-alter.dat-alter           = TODAY       
             hist-alter.des-motivo-alter    = "Confirmaá∆o de Item"      
             hist-alter.it-codigo           = tt-item-contrat.it-codigo
             hist-alter.alter-destino       = STRING(tt-item-contrat.preco-fornec-novo)       
             hist-alter.num-seq-item        = tt-item-contrat.num-seq-item       
             hist-alter.nr-alter            = i-cont-alt        
             hist-alter.num-seq-anexo       = 0       
             hist-alter.num-seq-clausula    = 0       
             hist-alter.ind-dest-event      = 1       
             hist-alter.log-reaj-formula    = NO       
             hist-alter.log-reaj-pend       = tt-param.l-atualiza
             hist-alter.num-seq-alter       = i-cont       
             hist-alter.numero-ordem        = 0       
             hist-alter.num-seq-event       = 0       
             hist-alter.num-seq-medicao     = 0
             hist-alter.des-tipo-alter      = c-des-tipo-alter. 

      IF ip-tipo = 1 THEN
         ASSIGN hist-alter.alter-origem     = STRING(tt-item-contrat.preco-unit).
      ELSE
         ASSIGN hist-alter.alter-origem     = STRING(tt-item-contrat.pre-unit-for).

      ASSIGN tt-item-contrat.observacao = "Registro hist-alter criado com sucesso".
   END.
END PROCEDURE.

PROCEDURE pi-gera-log:
   VIEW FRAME f-cabec.
   VIEW FRAME f-rodape.

   FOR EACH tt-item-contrat
            NO-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Gerando LOG : " +
                                    STRING(tt-item-contrat.num-seq-item) + "-" +
                                    STRING(tt-item-contrat.it-codigo   ) + "-" +
                                    STRING(tt-item-contrat.descricao   ) + "-" +
                                    STRING(tt-item-contrat.preco-fornec) + "-" +
                                    STRING(tt-item-contrat.qtd-total   ) 
                                    ).

      PUT tt-item-contrat.nr-contrato       SPACE(1)
          tt-item-contrat.num-seq-item      SPACE(1)
          tt-item-contrat.it-codigo         SPACE(1)
          tt-item-contrat.descricao         SPACE(1)
          tt-item-contrat.preco-unit        SPACE(1)
          tt-item-contrat.pre-unit-for      SPACE(1)
          tt-item-contrat.preco-fornec-novo SPACE(1)
          tt-item-contrat.observacao        SPACE(1)
          SKIP.
  END.
END PROCEDURE.


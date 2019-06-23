/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESUT0013RP 2.06.00.000 } /*** 010114 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESUT0013rp MRE}
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
*  ESUT0013RP.P - Remote Program da Importaá∆o DE-PARA Fam°lia Comercial     
*
*********************************************************************************/
{utp/ut-glob.i}
{include/tt-edit.i}

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arquivo          as char
    field arq-entrada1     as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.    

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEFINE TEMP-TABLE tt-dados NO-UNDO 
       FIELDS ep-codigo    LIKE ems2cadme.empresa.ep-codigo
       FIELDS it-codigo    LIKE ITEM.it-codigo
       FIELDS fm-cod-com   LIKE ITEM.fm-cod-com
       FIELDS observacao   AS   CHARACTER FORMAT "x(80)".

{cdp/cd0666.i}
{utp/ut-glob.i}
{include/i-rpvar.i}
    

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def var h-acomp            as handle  no-undo.



form tt-erro.cd-erro "-"   
     tt-erro.mensagem format "x(117)"
     with width 132 frame f-erros stream-io. 

{utp/ut-liter.i Erro *}
assign tt-erro.cd-erro:label in frame f-erros = trim(return-value).

{utp/ut-liter.i Descriá∆o *}
assign tt-erro.mensagem:label in frame f-erros = trim(return-value).     

def stream s-entrada.
/* def var c-linha          as character.          */
/* def var i-cont           as integer.            */
/* def var l-cabec          as logical.            */
/* def var l-num-automatica as log initial no.     */
/* def var l-erro           as log.                */
/* def var c-text-aux       as char no-undo.       */
/* def var i-seq-aux        as int no-undo init 0. */
/* def var l-spp-nfe        as log no-undo.        */

/**** Inicio ****/   


/*********************  Chamada EPC TMS ***********************/ 
FIND ems2cadme.empresa 
     WHERE empresa.ep-codigo = i-ep-codigo-usuario
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0013RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.000"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Importaá∆o DE-PARA Fam°lia Comercial".

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
     "Emp Item             Familia  Observaá∆o " SKIP
     "--- ---------------- -------- --------------------------------------------------------------------------------" SKIP
      WITH STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 212 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 212 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(212)"
  WITH STREAM-IO WIDTH 212 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/**************************************************************/

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Alteraá∆o_Fam°lia_Comercial *}
run pi-inicializar in h-acomp (input  Return-value ).

/* include padr“o para output de relatÆrios */
{include/i-rpout.i}

RUN pi-importa-planilha.
RUN pi-cria-registros.
RUN pi-gera-log.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK".


PROCEDURE pi-importa-planilha:
   DEFINE VARIABLE ch-excel          AS COMPONENT-HANDLE  NO-UNDO.
   DEFINE VARIABLE i-cont-linha      AS INTEGER           NO-UNDO.
      
   EMPTY TEMP-TABLE tt-dados.
   
   FILE-INFO:FILE-NAME = tt-param.arq-entrada1.

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:OPEN(FILE-INFO:FULL-PATHNAME).
   ch-Excel:Visible = FALSE.

   /* Navega na planilha e importa os dados */
   

   blk:
   DO i-cont-linha = 1 TO 10000:
      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Empresa" THEN DO:
         NEXT blk.
      END.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "" THEN DO:
         LEAVE blk.
      END.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = ? THEN DO:
         LEAVE blk.
      END.

      RUN pi-acompanhar IN h-acomp (INPUT "Importando : " +
                                    STRING(ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE) + "-" +
                                    STRING(ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE) + "-" +
                                    STRING(ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE) 
                                    ).

      CREATE tt-dados.
      ASSIGN tt-dados.ep-codigo    = STRING(INTEGER(ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE))
             tt-dados.it-codigo    =        REPLACE(ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE,",0000000000","")
             tt-dados.fm-cod-com   = STRING(INTEGER(ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE)).

      FIND FIRST fam-comerc
           WHERE fam-comerc.fm-cod-com  = tt-dados.fm-cod-com
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(fam-comerc) THEN DO:
         ASSIGN tt-dados.observacao = "Fam°lia comercial n∆o cadastrada :  " + tt-dados.fm-cod-com.
         NEXT blk.
      END.

      FIND FIRST ITEM
           WHERE ITEM.it-codigo  = tt-dados.it-codigo
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(ITEM) THEN DO:
         ASSIGN tt-dados.observacao = "Item n∆o cadastrado :  " + tt-dados.it-codigo.
         NEXT blk.
      END.
/*       ELSE DO:                                                                                                                           */
/*          IF ITEM.fm-cod-com <> "" THEN DO:                                                                                               */
/*             ASSIGN tt-dados.observacao = "Item " +  tt-dados.it-codigo + " j† cadastrado na familia comercial   " + tt-dados.fm-cod-com. */
/*             NEXT blk.                                                                                                                    */
/*          END.                                                                                                                            */
/*       END.                                                                                                                               */


      IF ems2cadme.empresa.ep-codigo <> tt-dados.ep-codigo THEN DO:
         ASSIGN tt-dados.observacao = "Empresa do usuario (" + ems2cadme.empresa.ep-codig + ") difere da empresa da importaá∆o (" + tt-dados.ep-codigo + ")".
         NEXT blk.
      END.

   END.

   /* all prompts will be shut off/on */
   /* this prevents unsaved check     */
   ch-Excel:DisplayAlerts = false.
   /* Exit Excel */
   ch-Excel:QUIT().
END PROCEDURE.


PROCEDURE pi-cria-registros:
   DISABLE TRIGGERS FOR LOAD OF fam-comerc.

   FOR EACH tt-dados
            WHERE tt-dados.observacao = ""
            EXCLUSIVE-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Atualizando : "      +
                                    STRING(tt-dados.ep-codigo ) + "-" +
                                    STRING(tt-dados.it-codigo ) + "-" +
                                    STRING(tt-dados.fm-cod-com)
                                    ).


      FIND ITEM
           WHERE ITEM.it-codigo = tt-dados.it-codigo
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(ITEM) THEN DO:
         ASSIGN ITEM.fm-cod-com = tt-dados.fm-cod-com.

         ASSIGN tt-dados.observacao = "Item: " + ITEM.it-codigo + " | Familia: " + ITEM.fm-cod-com.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-gera-log:
   VIEW FRAME f-cabec.
   VIEW FRAME f-rodape.

   FOR EACH tt-dados
            NO-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Gerando LOG : "      +
                                    STRING(tt-dados.ep-codigo ) + "-" +
                                    STRING(tt-dados.it-codigo ) + "-" +
                                    STRING(tt-dados.fm-cod-com)
                                    ).

      PUT tt-dados.ep-codigo    SPACE(1)
          tt-dados.it-codigo    SPACE(1)
          tt-dados.fm-cod-com   SPACE(1)
          tt-dados.observacao   SPACE(1)
          SKIP.
  END.
END PROCEDURE.

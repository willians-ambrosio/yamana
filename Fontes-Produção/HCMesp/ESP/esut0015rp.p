/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESUT0015RP 2.06.00.000 } /*** 010114 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESUT0015rp MRE}
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
*  ESUT0015RP.P - Remote Program da Importaá∆o de Documento     
*
*********************************************************************************/
{utp/ut-glob.i}
{include/tt-edit.i}

DISABLE TRIGGERS FOR LOAD OF cargo.

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

DEFINE TEMP-TABLE tt-de-para NO-UNDO 
       FIELDS cdn_cargo_basic       LIKE cargo.cdn_cargo_basic
       FIELDS des_cargo             LIKE cargo.des_cargo
       FIELDS cdn_niv_hier_funcnal  LIKE niv_hier_funcnal.cdn_niv_hier_funcnal
       FIELDS des_niv_hier_funcnal  LIKE niv_hier_funcnal.des_niv_hier_funcnal
       FIELDS observacao            AS CHARACTER FORMAT "x(60)".

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
FIND FIRST mgcad.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0015RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.000"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Atualizaá∆o Niveis Hierarquicos - HCM".

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
     "Cargo  Descriá∆o                            Niv Descriá∆o                      Observaá∆o" SKIP
     "------ ------------------------------------ --- ------------------------------ ------------------------------------------------------------" SKIP
WITH  STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 212 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 212 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(212)"
  WITH STREAM-IO WIDTH 212 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/**************************************************************/

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Atualizaá∆o_Niveis_Hierarquicos_HCM *}
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

   EMPTY TEMP-TABLE tt-de-para.
   
   FILE-INFO:FILE-NAME = tt-param.arq-entrada1.

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:OPEN(FILE-INFO:FULL-PATHNAME).
   ch-Excel:Visible = FALSE.

   /* Navega na planilha e importa os dados */
   blk:
   DO i-cont-linha = 1 TO 10000:

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Cargo Bas" THEN DO:
         NEXT blk.
      END.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "" OR 
         ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = ?  OR
         ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "?"  THEN DO:
         LEAVE blk.
      END.

      RUN pi-acompanhar IN h-acomp (INPUT "Importando : " +
                                    STRING(INTEGER( ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE)) + "-" +
                                    STRING(         ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE)  + "-" +
                                    STRING(INTEGER( ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE)) + "-" +
                                    STRING(        (ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE))
                                    ).

      CREATE tt-de-para.
      ASSIGN tt-de-para.cdn_cargo_basic        = INTEGER (ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE)
             tt-de-para.des_cargo              =          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE
             tt-de-para.cdn_niv_hier_funcnal   = INTEGER (ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE)
             tt-de-para.des_niv_hier_funcnal   =          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE.
   END.


   /* all prompts will be shut off/on */
   /* this prevents unsaved check     */
   ch-Excel:DisplayAlerts = false.
   /* Exit Excel */
   ch-Excel:QUIT().
END PROCEDURE.


PROCEDURE pi-cria-registros:
   blk:
   FOR EACH tt-de-para
            EXCLUSIVE-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Atualizando : "             +
                                    STRING(tt-de-para.cdn_cargo_basic     ) + "-" +
                                    STRING(tt-de-para.des_cargo           ) + "-" +
                                    STRING(tt-de-para.cdn_niv_hier_funcnal) + "-" +
                                    STRING(tt-de-para.des_niv_hier_funcnal)
                                    ).


      FIND FIRST niv_hier_funcnal
           WHERE niv_hier_funcnal.cdn_niv_hier_funcnal = tt-de-para.cdn_niv_hier_funcnal
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(niv_hier_funcnal) THEN DO:
         ASSIGN tt-de-para.observacao = "Nivel hierarquico " + STRING(tt-de-para.cdn_niv_hier_funcnal) + " n∆o cadastrado".
         NEXT blk.
      END.

      FIND FIRST cargo
           WHERE cargo.cdn_cargo_basic = tt-de-para.cdn_cargo_basic
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE(cargo) THEN DO:
         ASSIGN tt-de-para.observacao = "Cargo " + STRING(tt-de-para.cdn_cargo_basic) + " n∆o cadastrado".
         NEXT blk.
      END.

      ASSIGN tt-de-para.observacao = "| Cargo " + STRING(tt-de-para.cdn_cargo_basic) + " | Nivel Anterior : " + STRING(cargo.cdn_niv_hier_funcnal) + " | Nivel atual: " + STRING(tt-de-para.cdn_niv_hier_funcnal) + " |". 

      ASSIGN cargo.cdn_niv_hier_funcnal = tt-de-para.cdn_niv_hier_funcnal. 
   END.
END PROCEDURE.

PROCEDURE pi-gera-log:
   VIEW FRAME f-cabec.
   VIEW FRAME f-rodape.

   FOR EACH tt-de-para
            NO-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Gerando LOG : " +
                                    STRING(tt-de-para.cdn_cargo_basic     ) + "-" +
                                    STRING(tt-de-para.des_cargo           ) + "-" +
                                    STRING(tt-de-para.cdn_niv_hier_funcnal) + "-" +
                                    STRING(tt-de-para.des_niv_hier_funcnal)
                                    ).

      PUT tt-de-para.cdn_cargo_basic      SPACE(1)
          tt-de-para.des_cargo            SPACE(1)
          tt-de-para.cdn_niv_hier_funcnal SPACE(1)
          tt-de-para.des_niv_hier_funcnal SPACE(1)
          tt-de-para.observacao           SPACE(1)
          SKIP.
  END.
END PROCEDURE.

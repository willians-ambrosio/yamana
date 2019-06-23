/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESUT0009RP 12.1.13.000 } /*** 010114 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESUT0009rp MRE}
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
*  ESUT0009RP.P - Remote Program da Importa‡Æo de Documento     
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
    field hora-exec        as integer
    FIELD l-sobrepoe       AS LOGICAL.    

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEFINE TEMP-TABLE tt_unid_lotac NO-UNDO 
       FIELD cod_unid_lotac LIKE unid_lotac.cod_unid_lotac 
       FIELD des_unid_lotac LIKE unid_lotac.des_unid_lotac
       FIELDS r-rowid AS ROWID
       FIELDS descricao AS CHARACTER FORMAT "x(200)".

/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init YES NO-UNDO.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def {1} temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

/*Tratamento criado para que, em rotinas criticas nas quais esta include s½ ² chamada para definir a tt-erro, seja possivel
  fazer com que n’o seja definida a frame nem seja chamado o tratamento de tradu»’o, pois isto degrada performance.*/
&if '{&excludeFrameDefinition}' = 'yes' &then 
&else
                                              
    form
        space(04)
        tt-erro.cd-erro 
        space (02)
        c-mensagem-cb
        with width 132 no-box down stream-io frame f-consiste.
    
    run utp/ut-trfrrp.p (input frame f-consiste:handle).
    
    {utp/ut-liter.i Mensagem}
    assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).
    
    {utp/ut-liter.i Descri»’o}
    assign c-mensagem-cb:label in frame f-consiste = trim(return-value).

&endif

{utp/ut-glob.i}
{include/i-rpvar.i}
    

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def var h-acomp            as handle  no-undo.

def var c-serie-docto      like item-doc-est.serie-docto no-undo.
def var c-nro-docto        like item-doc-est.nro-docto no-undo.
def var c-cod-emitente     like item-doc-est.cod-emitente no-undo.
def var c-nat-operacao     like item-doc-est.nat-operacao no-undo.
def var c-cod-esp          like dupli-apagar.cod-esp no-undo.
def var c-parcela          like dupli-apagar.parcela no-undo.
def var i-seq              as integ no-undo initial 0.
def var i-seq-item         as integ no-undo initial 0.
def var c-char             as char  no-undo.

form tt-erro.cd-erro "-"   
     tt-erro.mensagem format "x(117)"
     with width 132 frame f-erros stream-io. 

{utp/ut-liter.i Erro *}
assign tt-erro.cd-erro:label in frame f-erros = trim(return-value).

{utp/ut-liter.i Descri‡Æo *}
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
FIND FIRST MGCAD.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0009RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.002"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "HCM"
	   c-titulo-relat   = "Importa‡Æo de Unidades de Lota‡Æo".

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
     "C¢digo               Descri‡Æo                                Observa‡Æo " SKIP
     "-------------------- ---------------------------------------- ----------------------------------------------------------------------" SKIP
     
WITH  STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 132 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 132 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(132)"
  WITH STREAM-IO WIDTH 132 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/**************************************************************/

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Importa‡Æo_de_Unidades_de_Lota‡Æo *}
run pi-inicializar in h-acomp (input  Return-value ).

/* include padrÒo para output de relat®rios */
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

   EMPTY TEMP-TABLE tt_unid_lotac.
   

   FILE-INFO:FILE-NAME = tt-param.arq-entrada1.

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:OPEN(FILE-INFO:FULL-PATHNAME).
   ch-Excel:Visible = FALSE.

   /* Navega na planilha e importa os dados */
   blk:
   DO i-cont-linha = 1 TO 10000:
      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Unid Lota‡Æo:" OR
         ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "c-final:"      OR
         ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Unid Lotac"    THEN
         NEXT blk.

      IF ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "" OR 
         ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = ? THEN
         LEAVE blk.

      RUN pi-acompanhar IN h-acomp (INPUT "Importando : " +
                                    ENTRY(1,ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE,',') + "-" +
                                    STRING(ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE)).

      CREATE tt_unid_lotac.
      ASSIGN tt_unid_lotac.cod_unid_lotac = ENTRY(1,ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE,',')
             tt_unid_lotac.des_unid_lotac = ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE.
   END.


   /* all prompts will be shut off/on */
    /* this prevents unsaved check     */
    ch-Excel:DisplayAlerts = false.
    /* Exit Excel */
    ch-Excel:QUIT().
END PROCEDURE.


PROCEDURE pi-cria-registros:
   DISABLE TRIGGERS FOR LOAD OF unid_lotac.

   FOR EACH tt_unid_lotac:

      RUN pi-acompanhar IN h-acomp (INPUT "Atualizando : " +
                                    STRING(tt_unid_lotac.cod_unid_lotac) + "-" +
                                    STRING(tt_unid_lotac.des_unid_lotac)).

      FIND FIRST unid_lotac
           WHERE unid_lotac.cod_unid_lotac = tt_unid_lotac.cod_unid_lotac NO-ERROR.
      IF NOT AVAIL unid_lotac THEN DO:
          CREATE unid_lotac.
          ASSIGN unid_lotac.cod_unid_lotac = tt_unid_lotac.cod_unid_lotac
                 unid_lotac.des_unid_lotac = tt_unid_lotac.des_unid_lotac
                 unid_lotac.log_livre_2    = YES
                 tt_unid_lotac.descricao = "Unidade de Lota‡Æo " + STRING(unid_lotac.cod_unid_lotac) + " criada com suceso.".
      END.
      ELSE DO:
          IF tt-param.l-sobrepoe 
              THEN ASSIGN tt_unid_lotac.descricao   = "Unidade de Lota‡Æo " + STRING(unid_lotac.cod_unid_lotac) + " alterada descri‡Æo de " + unid_lotac.des_unid_lotac + " para " + tt_unid_lotac.des_unid_lotac
                          unid_lotac.des_unid_lotac = tt_unid_lotac.des_unid_lotac.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-gera-log:
  VIEW FRAME f-cabec.
  VIEW FRAME f-rodape.

  FOR EACH tt_unid_lotac NO-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Gerando LOG : " +
                                    STRING(tt_unid_lotac.cod_unid_lotac) + "-" +
                                    STRING(tt_unid_lotac.des_unid_lotac)).

      PUT tt_unid_lotac.cod_unid_lotac SPACE(1)
          tt_unid_lotac.des_unid_lotac SPACE(1)
          tt_unid_lotac.descricao SKIP.
  END.
END PROCEDURE.

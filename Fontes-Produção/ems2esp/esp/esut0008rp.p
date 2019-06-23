/********************************************************************************************
**  Programa: ESUT0008
**  Data....: 08 de Setembro de 2016
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Relat¢rio LOG de Elimina‡Æo de Pedidos de Compra
**  Versão..: 2.06.00.001 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versão */
{include/i-prgvrs.i ESUT0008RP 2.06.00.001}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* definiîÒo das temp-tables para recebimento de par±metros */
define temp-table tt-param no-undo
    field destino              as integer
    field arquivo              as char format "x(35)"
    field usuario              as char format "x(12)"
    field data-exec            as date
    field hora-exec            as integer
    field classifica           as integer
    field desc-classifica      as char format "x(40)"
    field modelo-rtf           as char format "x(35)"
    field l-habilitaRtf        as LOG
    FIELD num-pedido-ini       LIKE ordem-compra.num-pedido
    FIELD num-pedido-fim       LIKE ordem-compra.num-pedido
    FIELD cod-emitente-ini     LIKE pedido-compr.cod-emitente
    FIELD cod-emitente-fim     LIKE pedido-compr.cod-emitente
    FIELD data-pedido-ini      LIKE pedido-compr.data-pedido
    FIELD data-pedido-fim      LIKE pedido-compr.data-pedido
    FIELD data-ini             AS   DATE FORMAT "99/99/9999"
    FIELD data-fim             AS   DATE FORMAT "99/99/9999"
    FIELD cod-usuario-ini      AS   CHARACTER FORMAT "x(12)"
    FIELD cod-usuario-fim      AS   CHARACTER FORMAT "x(12)".
    
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

DEFINE STREAM st-excel.
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

DEFINE VARIABLE i-cont AS INTEGER NO-UNDO.

&IF "{&mguni_version}" >= "2.071" &THEN
def new global shared var i-ep-codigo-usuario  LIKE ems2cadme.empresa.ep-codigo format "x(03)" no-undo.
&ELSE
def new global shared var i-ep-codigo-usuario  as integer format ">>9" no-undo.
&ENDIF
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

{include/i-rpvar.i}



/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST  ems2cadme.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0008RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.001"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Relat¢rio LOG de Elimina‡Æo de Pedidos de Compra".

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

/* include padrão para output de relat©rios */
{include/i-rpout.i}

RUN pi-impressao.

/* fechamento do output do relat©rio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
procedure pi-impressao:
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
   DEFINE VARIABLE c-tipo           AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-ordem          AS    CHARACTER             NO-UNDO FORMAT "x(300)".

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".csv".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-pedido         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado data               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado hora               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-usuario        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-ped-benef      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado natureza           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado data-pedido        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado situacao           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-emitente       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado end-entrega        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado end-cobranca       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado frete              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-transp         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado via-transp         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-cond-pag       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado responsavel        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-mensagem       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado impr-pedido        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado comentarios        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado mot-elimina        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nome-ass           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cargo-ass          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado emergencial        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-prox-ped        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado contr-forn         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-processo        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado compl-entrega      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado l-tipo-ped         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado l-classificacao    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado l-ind-prof         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-importador       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-situacao         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-cod-tabela       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-moeda            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-cod-forma        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-cod-via          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-prazo            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-descr-merc       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-cod-porto        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado de-vl-fob          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-embalagem        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado c-observacao       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado i-exportador       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado desc-forma         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado desc-via           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado de-vl-frete-i      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado ind-orig-entrada   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado ind-via-envio      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nro-proc-entrada   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nro-proc-saida     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nro-proc-alteracao 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-maq-origem     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-processo-mp    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado num-id-documento   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-contrato        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-estabel        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado check-sum          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado gera-edi           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-estab-gestor   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-emit-terc      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado nr-ped-venda       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cod-entrega        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado endereco_text      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado endereco           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado bairro             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cidade             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado estado             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado pais               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado cep                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado jurisdicao         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   {utp/ut-field.i ems5_esp es-pedido-compr-eliminado local-entrega      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)"  ";".
   
   PUT STREAM st-excel SKIP.
       
   blk1:
   FOR EACH es-pedido-compr-eliminado
            WHERE es-pedido-compr-eliminado.num-pedido   >= tt-param.num-pedido-ini   AND
                  es-pedido-compr-eliminado.num-pedido   <= tt-param.num-pedido-fim   AND
                  es-pedido-compr-eliminado.cod-emitente >= tt-param.cod-emitente-ini AND
                  es-pedido-compr-eliminado.cod-emitente <= tt-param.cod-emitente-fim AND
                  es-pedido-compr-eliminado.data-pedido  >= tt-param.data-pedido-ini  AND
                  es-pedido-compr-eliminado.data-pedido  <= tt-param.data-pedido-fim  AND
                  es-pedido-compr-eliminado.data         >= tt-param.data-ini         AND
                  es-pedido-compr-eliminado.data         <= tt-param.data-fim         AND
                  es-pedido-compr-eliminado.cod-usuario  >= tt-param.cod-usuario-ini  AND
                  es-pedido-compr-eliminado.cod-usuario  <= tt-param.cod-usuario-fim  
            NO-LOCK:

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp ("Listando: " + STRING(i-cont)).

      PUT STREAM st-excel
          es-pedido-compr-eliminado.num-pedido            ";"
          es-pedido-compr-eliminado.data                  ";"
          es-pedido-compr-eliminado.hora                  ";"
          es-pedido-compr-eliminado.cod-usuario           ";"
          es-pedido-compr-eliminado.num-ped-benef         ";"
          es-pedido-compr-eliminado.natureza              ";"
          es-pedido-compr-eliminado.data-pedido           ";"
          es-pedido-compr-eliminado.situacao              ";"
          es-pedido-compr-eliminado.cod-emitente          ";"
          es-pedido-compr-eliminado.end-entrega           ";"
          es-pedido-compr-eliminado.end-cobranca          ";"
          es-pedido-compr-eliminado.frete                 ";"
          es-pedido-compr-eliminado.cod-transp            ";"
          es-pedido-compr-eliminado.via-transp            ";"
          es-pedido-compr-eliminado.cod-cond-pag          ";"
          es-pedido-compr-eliminado.responsavel           ";"
          es-pedido-compr-eliminado.cod-mensagem          ";"
          es-pedido-compr-eliminado.impr-pedido           ";"
          es-pedido-compr-eliminado.comentarios           ";"
          es-pedido-compr-eliminado.mot-elimina           ";"
          es-pedido-compr-eliminado.nome-ass              ";"
          es-pedido-compr-eliminado.cargo-ass             ";"
          es-pedido-compr-eliminado.emergencial           ";"
          es-pedido-compr-eliminado.nr-prox-ped           ";"
          es-pedido-compr-eliminado.contr-forn            ";"
          es-pedido-compr-eliminado.nr-processo           ";"
          es-pedido-compr-eliminado.compl-entrega         ";"
          es-pedido-compr-eliminado.l-tipo-ped            ";"
          es-pedido-compr-eliminado.l-classificacao       ";"
          es-pedido-compr-eliminado.l-ind-prof            ";"
          es-pedido-compr-eliminado.i-importador          ";"
          es-pedido-compr-eliminado.i-situacao            ";"
          es-pedido-compr-eliminado.c-cod-tabela          ";"
          es-pedido-compr-eliminado.i-moeda               ";"
          es-pedido-compr-eliminado.i-cod-forma           ";"
          es-pedido-compr-eliminado.i-cod-via             ";"
          es-pedido-compr-eliminado.c-prazo               ";"
          es-pedido-compr-eliminado.c-descr-merc          ";"
          es-pedido-compr-eliminado.i-cod-porto           ";"
          es-pedido-compr-eliminado.de-vl-fob             ";"
          es-pedido-compr-eliminado.c-embalagem           ";"
          es-pedido-compr-eliminado.c-observacao          ";"
          es-pedido-compr-eliminado.i-exportador          ";"
          es-pedido-compr-eliminado.desc-forma            ";"
          es-pedido-compr-eliminado.desc-via              ";"
          es-pedido-compr-eliminado.de-vl-frete-i         ";"
          es-pedido-compr-eliminado.ind-orig-entrada      ";"
          es-pedido-compr-eliminado.ind-via-envio         ";"
          es-pedido-compr-eliminado.nro-proc-entrada      ";"
          es-pedido-compr-eliminado.nro-proc-saida        ";"
          es-pedido-compr-eliminado.nro-proc-alteracao    ";"
          es-pedido-compr-eliminado.cod-maq-origem        ";"
          es-pedido-compr-eliminado.num-processo-mp       ";"
          es-pedido-compr-eliminado.num-id-documento      ";"
          es-pedido-compr-eliminado.nr-contrato           ";"
          es-pedido-compr-eliminado.cod-estabel           ";"
          es-pedido-compr-eliminado.check-sum             ";"
          es-pedido-compr-eliminado.gera-edi              ";"
          es-pedido-compr-eliminado.cod-estab-gestor      ";"
          es-pedido-compr-eliminado.cod-emit-terc         ";"
          es-pedido-compr-eliminado.nr-ped-venda          ";"
          es-pedido-compr-eliminado.cod-entrega           ";"
          es-pedido-compr-eliminado.endereco_text         ";"
          es-pedido-compr-eliminado.endereco              ";"
          es-pedido-compr-eliminado.bairro                ";"
          es-pedido-compr-eliminado.cidade                ";"
          es-pedido-compr-eliminado.estado                ";"
          es-pedido-compr-eliminado.pais                  ";"
          es-pedido-compr-eliminado.cep                   ";"
          es-pedido-compr-eliminado.jurisdicao            ";"
          es-pedido-compr-eliminado.local-entrega         ";".

      PUT STREAM st-excel SKIP.                                                                                       
   END.                                                                                                   

   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


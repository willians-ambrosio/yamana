/*****************************************************************************
**       Programa: RELT0127.p
**       Data....: 30/03/12
**       Autor...: DATASUL S.A.
**       Objetivo: PEDIDOS POR COMPRADOR
**       Vers∆o..: 1.00.000 - adm
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "RELT0127".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "12.1.13.003").

DEFINE VARIABLE d-taxa-moeda        LIKE cotacao.cotacao[1]     NO-UNDO.
DEFINE VARIABLE c-desc-moeda        LIKE moeda.descricao        NO-UNDO.
DEFINE VARIABLE c-requisitante      LIKE usuar_mestre.cod_usuar NO-UNDO.
DEFINE VARIABLE c-nome-requisitante LIKE usuar_mestre.nom_usuar NO-UNDO.
DEFINE VARIABLE c-comprador         LIKE comprador.nome         NO-UNDO.
DEFINE VARIABLE c-nome-comprador    LIKE comprador.nome         NO-UNDO.

DEFINE VARIABLE de-preco         AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-preco-unit    AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-valor-ipi     AS DECIMAL FORMAT ">>>>>>,>>9.99999":U       INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-preco-orig    AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-aliquota-ipi  AS DECIMAL FORMAT "->>,>>9.99":U             INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-total-pedido  AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-valor-item    AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-valor-item-un AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.

DEFINE VARIABLE l-emergencial    AS LOGICAL FORMAT "Sim/N∆o" NO-UNDO.

DEFINE VARIABLE de-saldo         LIKE saldo-estoq.qtidade-atu NO-UNDO.
DEFINE VARIABLE de-qt-inicial    LIKE saldo-estoq.qtidade-atu NO-UNDO.
DEFINE VARIABLE de-qt-final      LIKE saldo-estoq.qtidade-atu NO-UNDO.

DEFINE BUFFER bf-ordem-compra FOR ordem-compra.
DEFINE BUFFER bf-prazo-compra FOR prazo-compra.

def var de-val-orig         as decimal   no-undo format ">>>>>>>>>>>>>>9,99".
def var date-data           as date      no-undo.
def var da-data             as date      no-undo.
def new global shared var i-moeda-g as integer initial 0           no-undo.
def new global shared var l-conv-g  as logical initial yes         no-undo.
def new global shared var da-conv-g as date    format "99/99/9999" no-undo.

def  var l-preco-bruto as logical no-undo.

def new global shared var i-moeda-g as integer form ">9" init 0 label "Moeda" no-undo.

def new global shared var l-conv-g  as logical view-as radio-set horizontal radio-buttons "Cotacao",yes,"Atual",no no-undo.

def new global shared var da-conv-g as date format "99/99/9999"
                                label "Data de Conversao" init today no-undo.


/****************** DefiniÁ„o de Tabelas Tempor·rias do RelatÛrio **********************/
{include/tt-edit.i}
{include/pi-edit.i}
define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato-arquivo      as INTEGER
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field i-cod-emitente-ini like ordem-compra.cod-emitente
    field i-cod-emitente-fim like ordem-compra.cod-emitente
    field c-cod-comprado-ini like ordem-compra.cod-comprado
    field c-cod-comprado-fim like ordem-compra.cod-comprado
    field da-data-pedido-ini like pedido-compr.data-pedido
    field da-data-pedido-fim like pedido-compr.data-pedido
    field c-cod-estabel-ini like pedido-compr.cod-estabel
    field c-cod-estabel-fim like pedido-compr.cod-estabel
.
/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

def new shared var i-cod-emitente-ini like ordem-compra.cod-emitente format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-emitente-fim like ordem-compra.cod-emitente format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var c-cod-comprado-ini like ordem-compra.cod-comprado format "X(12)" initial "" no-undo.
def new shared var c-cod-comprado-fim like ordem-compra.cod-comprado format "X(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var da-data-pedido-ini like pedido-compr.data-pedido format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-data-pedido-fim like pedido-compr.data-pedido format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var c-cod-estabel-ini like pedido-compr.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim like pedido-compr.cod-estabel format "x(3)" initial "ZZZ" no-undo.

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Definiáao de Vari†veis Campo Calculado do Relat¢rio **********************/ 

def var descricao as CHARACTER FORMAT "x(2000)".
def var c-desc-item as CHARACTER FORMAT "x(80)".
def var vlr_inicial as decimal label "Vlr Inic".
def var Vlr_total as decimal label "Total OC Decto".
def var Vlr_Ttal_fch as decimal label "Vlr Total".

DEFINE STREAM st-csv.

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var h-FunctionLibrary    as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
def var v-cont-registro      as int    no-undo.
def var v-des-retorno        as char   no-undo.
def var v-des-local-layout   as char   no-undo.

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form ordem-compra.cod-emitente column-label "Fornec" format ">>>>>>>>9" at 001
     ordem-compra.it-codigo column-label "Item" format "X(16)" at 011
     Vlr_Ttal_fch column-label "Vlr Total" format ">,>>>,>>9.99" at 028
     ordem-compra.preco-unit column-label "Preáo Unit" format ">,>>>,>>9.99" at 041
     ordem-compra.perc-descto column-label "% Desc" format ">9.99999" at 055
     ordem-compra.valor-descto column-label "Vl Desconto" format ">>>,>>>,>>9.9999" at 064
     ordem-compra.dat-ordem column-label "Data Ordem" format "99/99/9999" at 081
     pedido-compr.emergencial column-label "Emergencial" format "Sim/N∆o" at 092
     ordem-compra.cod-comprado column-label "Comprador" format "X(12)" at 104
     ordem-compra.num-pedido column-label "Pedido" format ">>>>>,>>9" at 117 skip
     ordem-compra.qt-solic column-label "Qtde" format ">,>>>,>>9.99" at 001
     Vlr_total column-label "Total OC Decto" format ">,>>>,>>9.99" at 018
     vlr_inicial column-label "Vlr Inic" format ">,>>>,>>9.99" at 033
     ordem-compra.numero-ordem column-label "Ordem" format "zzzzz9,99" at 046
     ordem-compra.ct- column-label "Conta Cont†bil" format "x(17)" at 056
     ordem-compra.data-pedido column-label "Data Pedido" format "99/99/9999" at 074
     descricao column-label "Narrativa" format "x(30)" at 086
     with down width 132 no-box stream-io frame f-relat-09-132.

DEFINE VARIABLE c-simbolo AS CHARACTER NO-UNDO FORMAT "x(2)".

DEFINE VARIABLE de-aberto AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Aberto" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE de-atraso AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Atraso" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE de-total AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-qtd-aberto AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Qtd Aberto" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-qtd-ordens AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Qtd Ordens" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

def var da-date       as   date                    no-undo.
def var i-qdt-ordens  as   integer format ">>>9"   no-undo.
def var i-qdt-aberto  as   integer format ">>>9"   no-undo.
def var l-aberto      as   logical                 no-undo.

create tt-param.
raw-transfer raw-param to tt-param.


FUNCTION fcTotalPedido RETURN DECIMAL (INPUT ip-num-pedido AS INTEGER):

    ASSIGN de-total = 0.


   for each bf-ordem-compra use-index pedido 
       where bf-ordem-compra.num-pedido = ip-num-pedido 
         and bf-ordem-compra.situacao <> 4 no-lock:

        assign i-qtd-ordens = i-qtd-ordens + 1
               l-aberto = no.

        for each bf-prazo-compra use-index ordem 
           where bf-prazo-compra.numero-ordem = bf-ordem-compra.numero-ordem no-lock:

              IF bf-prazo-compra.situacao = 4 THEN
                  NEXT.
              
              if l-conv-g then 
                assign da-date = bf-ordem-compra.data-cotacao.
              else 
                assign da-date = da-conv-g.             
              
              if bf-ordem-compra.mo-codigo = i-moeda-g then 
                 assign de-preco-unit = bf-ordem-compra.preco-unit.
              else
                 run cdp/cd0812.p (input bf-ordem-compra.mo-codigo,
                                   input i-moeda-g,
                                   input bf-ordem-compra.preco-unit,
                                   input da-date,
                                   output de-preco-unit).

              if de-preco-unit = ? then 
                 assign de-preco-unit = 0.
              
              assign de-total = de-total + bf-prazo-compra.quantidade * de-preco-unit.

              if  bf-prazo-compra.quant-saldo > 0
              and (      bf-prazo-compra.situacao <> 4 
                    and  bf-prazo-compra.situacao <> 6) then
                assign l-aberto = yes
                       de-aberto = de-aberto + bf-prazo-compra.quant-saldo
                                 * de-preco-unit.

            /*Situaªío:1- Nío confirmada, 2- Confirmada, 3- Cotada, 4- Eliminada, 5- Em cotaªío, 6- Recebida */
            if  bf-prazo-compra.data-entrega < TODAY 
            AND bf-prazo-compra.situacao = 2   THEN 
                assign de-atraso = de-atraso + bf-prazo-compra.quant-saldo
                                 * de-preco-unit.
        end.
        if l-aberto then
           assign i-qtd-aberto = i-qtd-aberto + 1.
   end.

   RETURN de-total.
END FUNCTION.

/*
def var de-preco-unit like bf-ordem-compra.preco-unit no-undo.
def var da-date       as   date                    no-undo.
def var i-qtd-ordens  as   integer format ">>>9"   no-undo.
def var i-qdt-aberto  as   integer format ">>>9"   no-undo.
def var l-aberto      as   logical                 no-undo.
DEFINE VARIABLE de-total AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
      NO-UNDO.

DEFINE VARIABLE de-aberto AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
     NO-UNDO.

DEFINE VARIABLE de-atraso AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99":U INITIAL 0 
      NO-UNDO.

    ASSIGN de-total = 0.
       
    for each bf-ordem-compra use-index pedido 
       where bf-ordem-compra.num-pedido = ip-num-pedido 
         and bf-ordem-compra.situacao <> 4 no-lock:

        assign i-qtd-ordens = i-qtd-ordens + 1
               l-aberto = no.

        for each bf-prazo-compra use-index ordem 
           where bf-prazo-compra.numero-ordem = bf-ordem-compra.numero-ordem no-lock:

              IF bf-prazo-compra.situacao = 4 THEN
                  NEXT.
              
              assign da-date = bf-ordem-compra.data-cotacao.
              
              if bf-ordem-compra.mo-codigo = i-moeda-g then 
                 assign de-preco-unit = bf-ordem-compra.preco-unit.
              else
                 run cdp/cd0812.p (input bf-ordem-compra.mo-codigo,
                                   input i-moeda-g,
                                   input bf-ordem-compra.preco-unit,
                                   input da-date,
                                   output de-preco-unit).

              if de-preco-unit = ? then 
                 assign de-preco-unit = 0.
              
              assign de-total = de-total + bf-prazo-compra.quantidade * de-preco-unit.

              if  bf-prazo-compra.quant-saldo > 0
              and (      bf-prazo-compra.situacao <> 4 
                    and  bf-prazo-compra.situacao <> 6) then
                assign l-aberto = yes
                       de-aberto = de-aberto + bf-prazo-compra.quant-saldo
                                 * de-preco-unit.

            /*Situaªío:1- Nío confirmada, 2- Confirmada, 3- Cotada, 4- Eliminada, 5- Em cotaªío, 6- Recebida */
            if  bf-prazo-compra.data-entrega < TODAY 
            AND bf-prazo-compra.situacao = 2   THEN 
                assign de-atraso = de-atraso + bf-prazo-compra.quant-saldo
                                 * de-preco-unit.
        end.
        if l-aberto then
           assign i-qdt-aberto = i-qdt-aberto + 1.
   end.

   RETURN de-total.

END.
*/

def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.
define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.
define new shared stream str-rp.

DEFINE VARIABLE i-line-count AS INTEGER INIT 1  NO-UNDO.
DEFINE VARIABLE c-arq-excel  AS CHARACTER       NO-UNDO.
DEFINE VARIABLE chExcel2     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWBook2     AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWSheet2    AS COM-HANDLE      NO-UNDO.

DEF VAR chworkbook AS COM-HANDLE NO-UNDO.
DEF VAR c-caminho AS CHAR NO-UNDO.

assign c-programa     = "RELT0127"
       c-versao       = "2.06"
       c-revisao      = ".00.001"
       c-titulo-relat = "PEDIDOS POR COMPRADOR"
       c-sistema      = "EMS".


find first mguni.empresa NO-LOCK
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.
form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabper.
run grapi/gr2004.p.
form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.


end. /* tt-param.formato = 2 */


run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       i-cod-emitente-ini = tt-param.i-cod-emitente-ini
       i-cod-emitente-fim = tt-param.i-cod-emitente-fim
       c-cod-comprado-ini = tt-param.c-cod-comprado-ini
       c-cod-comprado-fim = tt-param.c-cod-comprado-fim
       da-data-pedido-ini = tt-param.da-data-pedido-ini
       da-data-pedido-fim = tt-param.da-data-pedido-fim
       c-cod-estabel-ini = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim = tt-param.c-cod-estabel-fim
.

def var l-imprime as logical no-undo.

assign l-imprime = no.
if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

IF tt-param.formato-arquivo = 2 THEN DO:
   /*###########################__CSV-INICIO__###############################*/
   RUN criaPlanilhaCSV.
   RUN printColumnLabelCSV.
   RUN printDados.
   RUN finalizaPlanilhaCSV.
   /*###########################__CSV-FIM__#################################*/
END.
ELSE DO:
   /*###########################__EXCEL-INICIO__###############################*/
   RUN criaPlanilha.
   RUN printColumnLabel.
   RUN printDados.
   RUN finalizaPlanilha.
   /*###########################__EXCEL-FIM__#################################*/
/* gr9020a.p */
END.




IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.


/******************************** PROCEDURES  EXCEL *******************************************/
PROCEDURE printDados:
   DEFINE VARIABLE c-ordem AS CHARACTER NO-UNDO.
   DEFINE VARIABLE i-aux   AS DECIMAL   NO-UNDO.

   FOR EACH pedido-compr 
            WHERE pedido-compr.cod-estabel >= c-cod-estabel-ini  AND   
                  pedido-compr.cod-estabel <= c-cod-estabel-fim  AND  
                  pedido-compr.data-pedido >= da-data-pedido-ini AND   
                  pedido-compr.data-pedido <= da-data-pedido-fim
            NO-LOCK,
            EACH ordem-compra 
                 WHERE ordem-compra.num-pedido    = pedido-compr.num-pedido AND 
                       ordem-compra.cod-comprado >= c-cod-comprado-ini      AND  
                       ordem-compra.cod-comprado <= c-cod-comprado-fim      AND 
                       ordem-compra.cod-emitente >= i-cod-emitente-ini      AND  
                       ordem-compra.cod-emitente <= i-cod-emitente-fim      AND 
                       pedido-compr.nr-contrato  = 0                        AND 
                       ordem-compra.situacao  <> 4
            NO-LOCK
            BREAK BY ordem-compra.dat-ordem:

      ASSIGN c-comprador      = STRING(ordem-compra.cod-comprado, "99999999999")
             c-nome-comprador = "".

      FIND FIRST comprador 
           WHERE comprador.cod-comprado = ordem-compra.cod-comprado
          NO-LOCK NO-ERROR.
      IF AVAILABLE(comprador) THEN
         ASSIGN c-nome-comprador = comprador.nome.
      
      FIND FIRST ITEM
           WHERE ITEM.it-codigo = ordem-compra.it-codigo
           NO-LOCK NO-ERROR.

      FIND FIRST item-uni-estab
           WHERE item-uni-estab.it-codigo   = ordem-compra.it-codigo AND
                 item-uni-estab.cod-estabel = ordem-compra.cod-estabel
           NO-LOCK NO-ERROR.
      
      FIND FIRST emitente 
           WHERE emitente.cod-emitente = pedido-compr.cod-emitente
           NO-LOCK NO-ERROR.

      FIND FIRST cotacao-item OF ordem-compra
           WHERE cotacao-item.cot-aprovada
           NO-LOCK NO-ERROR.

      FOR FIRST moeda FIELDS( mo-codigo descricao )
                WHERE moeda.mo-codigo = cotacao-item.mo-codigo 
                NO-LOCK:
      END.
      IF NOT AVAILABLE(moeda) THEN 
         NEXT.

      ASSIGN d-taxa-moeda = 1
             c-desc-moeda = moeda.descricao.

      ASSIGN c-simbolo = IF c-desc-moeda = "Real" THEN "R$" ELSE "".

      ASSIGN de-total-pedido = fcTotalPedido(pedido-compr.num-pedido).
      
      IF ordem-compra.mo-codigo <> 0 THEN DO:
         RUN cdp/cd0812.p (INPUT  cotacao-item.mo-codigo, /*moeda origem*/
                           INPUT  0,                      /*moeda destino*/
                           INPUT  1,                      /*valor origem*/
                           INPUT  pedido-compr.data-pedido,            /*data conversao*/
                           OUTPUT d-taxa-moeda).          /*valor destino*/
         /******* ******* *******/
      END.

      FIND FIRST it-requisicao
           WHERE it-requisicao.nr-requisicao = ordem-compra.nr-requisicao AND
                 it-requisicao.it-codigo     = ordem-compra.it-codigo     AND
                 it-requisicao.sequencia     = ordem-compra.sequencia
           NO-LOCK NO-ERROR.
      IF AVAILABLE(it-requisicao) THEN DO:
         FIND FIRST usuar_mestre
              WHERE usuar_mestre.cod_usuar = it-requisicao.nome-abrev
              NO-LOCK NO-ERROR.

         ASSIGN c-requisitante      = usuar_mestre.cod_usuar
                c-nome-requisitante = usuar_mestre.nom_usuar.

         FIND LAST  doc-pend-aprov
              WHERE doc-pend-aprov.nr-requisicao = it-requisicao.nr-requisicao    
              NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST usuar_mestre
              WHERE usuar_mestre.cod_usuar = ordem-compra.requisitante
              NO-LOCK NO-ERROR.
         IF AVAILABLE(usuar_mestre) THEN
            ASSIGN c-requisitante      = usuar_mestre.cod_usuar
                   c-nome-requisitante = usuar_mestre.nom_usuar.
      END.

      FOR FIRST es-it-depto
                WHERE es-it-depto.it-codigo = item.it-codigo
                NO-LOCK,
                FIRST es-depto
                      WHERE es-depto.codigo = es-it-depto.cod-depto
                      NO-LOCK:
      END.

      RUN pi-precos.

      RUN pi-saldo-item (INPUT  ITEM.it-codigo,
                         INPUT  "",
                         INPUT  "zzzzzz",
                         INPUT ordem-compra.data-emissao,
                         OUTPUT de-qt-inicial,
                         OUTPUT de-qt-final).

      ASSIGN de-saldo = de-qt-final.

      ASSIGN de-valor-item    = IF AVAILABLE(cotacao-item) THEN cotacao-item.preco-fornec ELSE ordem-compra.preco-unit
             de-valor-item-un = de-valor-item + de-valor-ipi - IF AVAILABLE(cotacao-item)   THEN cotacao-item.valor-descto ELSE 0.
      
      ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1.

      RUN pi-acompanhar IN h-acomp(INPUT STRING(v-num-reg-lidos)).
      
      ASSIGN Vlr_Ttal_fch = (ordem-compra.valor-descto + ordem-compra.preco-unit ) * ordem-compra.qt-solic 
             Vlr_total    = ordem-compra.preco-unit * ordem-compra.qt-solic
             vlr_inicial  = ordem-compra.preco-unit + ordem-compra.valor-descto .
      /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/
      
      ASSIGN c-ordem = STRING(ordem-compra.numero-ordem, "zzzzz9,99").

      ASSIGN descricao = "".

      run pi-print-editor(ordem-compra.narrativa, LENGTH(ordem-compra.narrativa)).
        for each tt-editor no-lock:
            if  tt-editor.conteudo <> "" then
                assign descricao = descricao + tt-editor.conteudo.
        end.
      
      descricao = REPLACE(descricao,chr(8),"").
      descricao = REPLACE(descricao,chr(13),"").
      descricao = REPLACE(descricao,";",":").
      descricao = REPLACE(descricao,"'","").
      descricao = REPLACE(descricao,'"',"").
      descricao = REPLACE(descricao,CHR(10),"").
      descricao = REPLACE(descricao,CHR(34),"").

      ASSIGN c-desc-item = "".

      IF AVAILABLE(ITEM) THEN DO:
         run pi-print-editor(ITEM.desc-item, LENGTH(ITEM.desc-item)).
         for each tt-editor no-lock:
            if  tt-editor.conteudo <> "" then
                assign c-desc-item = c-desc-item + tt-editor.conteudo.
         end.
      
         c-desc-item = REPLACE(c-desc-item,chr(8),"").
         c-desc-item = REPLACE(c-desc-item,chr(13),"").
         c-desc-item = REPLACE(c-desc-item,";",":").
         c-desc-item = REPLACE(c-desc-item,"'","").
         c-desc-item = REPLACE(c-desc-item,'"',"").
         c-desc-item = REPLACE(c-desc-item,CHR(10),"").
         c-desc-item = REPLACE(c-desc-item,CHR(34),"").
      END.
                                                                                            
      RUN pi-acompanhar in h-acomp (input "Pedido..: " + STRING(ordem-compra.num-pedido)).  
                                                                                            
      ASSIGN i-aux    = DEC(ordem-compra.conta-contabil).                                   
      
      IF tt-param.formato-arquivo = 2 THEN
         RUN pi-dados-csv.
      ELSE
         RUN pi-dados-excel.
   END. /* for each pedido-compr NO-LOCK */
END PROCEDURE.

PROCEDURE criaPlanilha:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Criaá∆o da Planilha Excel
------------------------------------------------------------------------------*/

    CREATE 'Excel.Application' chExcel2.
    chWBook2 = chExcel2:Workbooks:Add().
    chWSheet2 = chWBook2:Sheets:Item(1).

END PROCEDURE. /*- criaPlanilha -*/



PROCEDURE printColumnLabel: 
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Gera label e cor do cabeáalho das colunas dos Projetos de Investimento
------------------------------------------------------------------------------*/

   chWSheet2:Range("C1:AP1"):select.
   chExcel2:SELECTION:Merge.

   ASSIGN i-line-count = 1.

   ASSIGN chWSheet2:Range("C" + string(i-line-count) + ":AP" + string(i-line-count)):VALUE = String("PEDIDOS POR COMPRADOR - ") + string(TODAY, "99/99/9999")
          chWSheet2:Range("C" + string(i-line-count) + ":AP" + string(i-line-count)):FONT:Bold = TRUE
          chWSheet2:Range("C" + string(i-line-count) + ":AP" + string(i-line-count)):FONT:SIZE = 14.

   ASSIGN i-line-count = i-line-count + 2.

   ASSIGN chWSheet2:Range("A"  + string(i-line-count)):VALUE = "Fornecedor"
          chWSheet2:Range("B"  + string(i-line-count)):VALUE = "Nome Abrev Fornec"                 
          chWSheet2:Range("C"  + string(i-line-count)):VALUE = "Familia"
          chWSheet2:Range("D"  + string(i-line-count)):VALUE = "Familia Cml"
          chWSheet2:Range("E"  + string(i-line-count)):VALUE = "Item"
          chWSheet2:Range("F"  + string(i-line-count)):VALUE = "Descriá∆o item"                 
          chWSheet2:Range("G"  + string(i-line-count)):VALUE = "Preáo"                 
          chWSheet2:Range("H"  + string(i-line-count)):VALUE = "Valor IPI"                 
          chWSheet2:Range("I"  + string(i-line-count)):VALUE = "Valor Total"                 
          chWSheet2:Range("J"  + string(i-line-count)):VALUE = "Valor Desc"
          chWSheet2:Range("K"  + string(i-line-count)):VALUE = "Moeda"
          chWSheet2:Range("L"  + string(i-line-count)):VALUE = "Cotac∆o Dt Pedido"
          chWSheet2:Range("M"  + string(i-line-count)):VALUE = "Preáo Convertido"                 
          chWSheet2:Range("N"  + string(i-line-count)):VALUE = "Data Ordem"
          chWSheet2:Range("O"  + string(i-line-count)):VALUE = "Emergencial"
          chWSheet2:Range("P"  + string(i-line-count)):VALUE = "Requisitante"                 
          chWSheet2:Range("Q"  + string(i-line-count)):VALUE = "Nome Requisitante"                 
          chWSheet2:Range("R"  + string(i-line-count)):VALUE = "Comprador"
          chWSheet2:Range("S"  + string(i-line-count)):VALUE = "Nome Comprador"
          chWSheet2:Range("T"  + string(i-line-count)):VALUE = "Pedido"
          chWSheet2:Range("U"  + string(i-line-count)):VALUE = "Qtde"
          chWSheet2:Range("V"  + string(i-line-count)):VALUE = "Valor Total do pedido"
          chWSheet2:Range("W"  + string(i-line-count)):VALUE = "Total OC Decto"
          chWSheet2:Range("X"  + string(i-line-count)):VALUE = "Ordem"
          chWSheet2:Range("Y"  + string(i-line-count)):VALUE = "Data Pedido"
          chWSheet2:Range("Z"  + string(i-line-count)):VALUE = "Narrativa"
          chWSheet2:Range("AA" + string(i-line-count)):VALUE = "Nß Requisiá∆o"
          chWSheet2:Range("AB" + string(i-line-count)):VALUE = "Grupo de Estoque"
          chWSheet2:Range("AC" + string(i-line-count)):VALUE = "Tipo do Item"
          chWSheet2:Range("AD" + string(i-line-count)):VALUE = "Situaá∆o do Item"
          chWSheet2:Range("AE" + string(i-line-count)):VALUE = "Tipo Demanda"
          chWSheet2:Range("AF" + string(i-line-count)):VALUE = "Tipo Controle do Item"
          chWSheet2:Range("AG" + string(i-line-count)):VALUE = "Departamento"
          chWSheet2:Range("AH" + string(i-line-count)):VALUE = "Nro Ordem Investimento"
          chWSheet2:Range("AI" + string(i-line-count)):VALUE = "Ponto de Encomenda do Item"
          chWSheet2:Range("AJ" + string(i-line-count)):VALUE = "MÇdia de Consumo"
          chWSheet2:Range("AK" + string(i-line-count)):VALUE = "Saldo do Item em estoque"
          chWSheet2:Range("AL" + string(i-line-count)):VALUE = "Tempo Fornec"
          chWSheet2:Range("AM" + string(i-line-count)):VALUE = "Tempo Fabric"
          chWSheet2:Range("AN" + string(i-line-count)):VALUE = "Tempo Compras"
          chWSheet2:Range("AO" + string(i-line-count)):VALUE = "Prazo de entrega do pedido"
          chWSheet2:Range("AP" + string(i-line-count)):VALUE = "Estabelecimento".

   chWSheet2:Range("A" + string(i-line-count) + ":AP" + string(i-line-count)):SELECT.
   chWSheet2:Range("A" + string(i-line-count) + ":AP" + string(i-line-count)):FONT:Bold = TRUE.
   chWSheet2:Range("A" + string(i-line-count) + ":AP" + string(i-line-count)):FONT:SIZE = 12.

   ASSIGN i-line-count = i-line-count + 1.
END PROCEDURE. /*- printColumnLabel -*/

PROCEDURE pi-formato:
    DEFINE INPUT PARAMETER ip-range                  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-formato                AS CHARACTER NO-UNDO.
    
    chWSheet2:Range(ip-range):NumberFormat = ip-formato.
END PROCEDURE.

PROCEDURE finalizaPlanilha:
   ASSIGN c-caminho = SESSION:TEMP-DIRECTORY + 'RELT01AA_' + REPLACE (STRING(TODAY,'99/99/9999'),'/','') + REPLACE (STRING(TIME,'hh:mm:ss'),':','') + ".xlsx".

   chExcel2:COLUMNS("A:AP"):EntireColumn:AUTOFIT().

   chExcel2:VISIBLE = YES.

   if search(c-caminho) <> ? then
     os-delete value(c-caminho).

   chExcel2:ActiveWorkbook:SaveAs(c-caminho,,,,,,).

   IF VALID-HANDLE(chExcel2) THEN
       RELEASE OBJECT chExcel2.

   IF VALID-HANDLE(chWBook2) THEN
       RELEASE OBJECT chWBook2.

   IF VALID-HANDLE(chWSheet2) THEN
       RELEASE OBJECT chWSheet2.
END PROCEDURE.

PROCEDURE criaPlanilhaCSV:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Criaá∆o da Planilha Excel
------------------------------------------------------------------------------*/
    ASSIGN c-caminho = SESSION:TEMP-DIRECTORY + 'RELT01AA_' + REPLACE (STRING(TODAY,'99/99/9999'),'/','') + REPLACE (STRING(TIME,'hh:mm:ss'),':','') + ".csv".

    OUTPUT STREAM st-csv TO VALUE(c-caminho) NO-CONVERT NO-MAP.

END PROCEDURE. /*- criaPlanilha -*/

PROCEDURE printColumnLabelCSV: 
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Gera label e cor do cabeáalho das colunas dos Projetos de Investimento
------------------------------------------------------------------------------*/
   PUT STREAM st-csv
       "Fornecedor"                  ";"
       "Nome Abrev Fornec"           ";"
       "Familia"                     ";"
       "Familia Cml"                 ";"
       "Item"                        ";"
       "Descriá∆o item"              ";"
       "Preáo"                       ";"
       "Valor IPI"                   ";"
       "Valor Total"                 ";"
       "Valor Desc"                  ";"
       "Moeda"                       ";"
       "Cotac∆o Dt Pedido"           ";"
       "Preáo Convertido"            ";"
       "Data Ordem"                  ";"
       "Emergencial"                 ";"
       "Requisitante"                ";"
       "Nome Requisitante"           ";"
       "Comprador"                   ";"
       "Nome Comprador"              ";"
       "Pedido"                      ";"
       "Qtde"                        ";"
       "Valor Total do pedido"       ";"
       "Total OC Decto"              ";"
       "Ordem"                       ";"
       "Data Pedido"                 ";"
       "Narrativa"                   ";"
       "Nß Requisiá∆o"               ";"
       "Grupo de Estoque"            ";"
       "Tipo do Item"                ";"
       "Situaá∆o do Item"            ";"
       "Tipo Demanda"                ";"
       "Tipo Controle do Item"       ";"
       "Departamento"                ";"
       "Nro Ordem Investimento"      ";"
       "Ponto de Encomenda do Item"  ";"
       "MÇdia de Consumo"            ";"
       "Saldo do Item em estoque"    ";"
       "Tempo Fornec"                ";"
       "Tempo Fabric"                ";"
       "Tempo Compras"               ";"
       "Prazo de entrega do pedido"  ";"
       "Estabelecimento"             ";"
       SKIP.
END PROCEDURE. /*- printColumnLabel -*/


PROCEDURE finalizaPlanilhaCSV:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Finaliza Planilha Excel
------------------------------------------------------------------------------*/
   OUTPUT STREAM st-csv CLOSE.

   IF i-num-ped-exec-rpw = 0 THEN
      DOS SILENT START excel.exe VALUE(c-caminho).
        
END PROCEDURE.

PROCEDURE pi-dados-excel:
   ASSIGN chWSheet2:Range("A"  + string(i-line-count)):VALUE = STRING(pedido-compr.cod-emitente)                                                                         /*  "Fornecedor                " */
          chWSheet2:Range("B"  + string(i-line-count)):VALUE = IF AVAILABLE(emitente) THEN emitente.nome-abrev ELSE ""                                                   /*  "Nome Abrev Fornec         " */                 
          chWSheet2:Range("C"  + string(i-line-count)):VALUE = IF AVAILABLE(ITEM) THEN ITEM.fm-codigo  ELSE ""                                                           /*  "Familia                   " */
          chWSheet2:Range("D"  + string(i-line-count)):VALUE = IF AVAILABLE(ITEM) THEN ITEM.fm-cod-com ELSE ""                                                           /*  "Familia Cml               " */
          chWSheet2:Range("E"  + string(i-line-count)):VALUE = ordem-compra.it-codigo                                                                                    /*  "Item                      " */
          chWSheet2:Range("F"  + string(i-line-count)):VALUE = c-desc-item                                                                                               /*  "Descriá∆o item            " */                 
          chWSheet2:Range("G"  + string(i-line-count)):VALUE = de-valor-item                                                                                             /*  "Preáo                     " */                 
          chWSheet2:Range("H"  + string(i-line-count)):VALUE = de-valor-ipi                                                                                              /*  "Valor IPI                 " */                 
          chWSheet2:Range("I"  + string(i-line-count)):VALUE = de-valor-item-un                                                                                          /*  "Valor Total               " */                 
          chWSheet2:Range("J"  + string(i-line-count)):VALUE = cotacao-item.valor-descto                                                                                 /*  "Valor Desc                " */
          chWSheet2:Range("K"  + string(i-line-count)):VALUE = c-desc-moeda                                                                                              /*  "Moeda                     " */
          chWSheet2:Range("L"  + string(i-line-count)):VALUE = d-taxa-moeda                                                                                              /*  "Cotac∆o Dt Pedido         " */
          chWSheet2:Range("M"  + string(i-line-count)):VALUE = ROUND(de-valor-item-un * d-taxa-moeda,2)                                                                  /*  "Preáo Convertido          " */                 
          chWSheet2:Range("N"  + string(i-line-count)):VALUE = ordem-compra.data-emissao                                                                                 /*  "Data Ordem                " */
          chWSheet2:Range("O"  + string(i-line-count)):VALUE = IF pedido-compr.emergencial = TRUE THEN "Sim" ELSE "N∆o"                                                  /*  "Emergencial               " */
          chWSheet2:Range("P"  + string(i-line-count)):VALUE = c-requisitante                                                                                            /*  "Requisitante              " */                 
          chWSheet2:Range("Q"  + string(i-line-count)):VALUE = c-nome-requisitante                                                                                       /*  "Nome Requisitante         " */                 
          chWSheet2:Range("R"  + string(i-line-count)):VALUE = c-comprador                                                                                               /*  "Comprador                 " */
          chWSheet2:Range("S"  + string(i-line-count)):VALUE = c-nome-comprador                                                                                          /*  "Nome Comprador            " */
          chWSheet2:Range("T"  + string(i-line-count)):VALUE = ordem-compra.num-pedido                                                                                   /*  "Pedido                    " */
          chWSheet2:Range("U"  + string(i-line-count)):VALUE = ordem-compra.qt-solic                                                                                     /*  "Qtde                      " */
          chWSheet2:Range("V"  + string(i-line-count)):VALUE = de-total-pedido                                                                                           /*  "Valor Total do pedido     " */
          chWSheet2:Range("W"  + string(i-line-count)):VALUE = ROUND((de-valor-item-un - cotacao-item.valor-descto) * ordem-compra.qt-solic,2)                           /*  "Total OC Decto            " */
          chWSheet2:Range("X"  + string(i-line-count)):VALUE = ordem-compra.numero-ordem                                                                                 /*  "Ordem                     " */
          chWSheet2:Range("Y"  + string(i-line-count)):VALUE = ordem-compra.data-pedido                                                                                  /*  "Data Pedido               " */
          chWSheet2:Range("Z"  + string(i-line-count)):VALUE = descricao                                                                                                 /*  "Narrativa                 " */
          chWSheet2:Range("AA" + string(i-line-count)):VALUE = ordem-compra.nr-requisicao                                                                                /*  "Nß Requisiá∆o             " */
          chWSheet2:Range("AB" + string(i-line-count)):VALUE = ITEM.ge-codigo                                                                                            /*  "Grupo de Estoque          " */
          chWSheet2:Range("AC" + string(i-line-count)):VALUE = STRING(ENTRY(ITEM.ind-serv-mat,{ininc/i08in172.i 03}),"x(30)")                                            /*  "Tipo do Item              " */
          chWSheet2:Range("AD" + string(i-line-count)):VALUE = STRING(ENTRY(ITEM.cod-obsoleto,{ininc/i17in172.i 03}),"x(30)")                                            /*  "Situaá∆o do Item          " */
          chWSheet2:Range("AE" + string(i-line-count)):VALUE = STRING(ENTRY(ITEM.demanda     ,{ininc/i02in122.i 03}),"x(30)")                                            /*  "Tipo Demanda              " */
          chWSheet2:Range("AF" + string(i-line-count)):VALUE = STRING(ENTRY(ITEM.tipo-contr  ,{ininc/i09in122.i 03}),"x(30)")                                            /*  "Tipo Controle do Item     " */
          chWSheet2:Range("AG" + string(i-line-count)):VALUE = IF AVAILABLE (es-depto) THEN es-depto.descricao ELSE ""                                                   /*  "Departamento              " */
          chWSheet2:Range("AH" + string(i-line-count)):VALUE = ordem-compra.num-ord-inv                                                                                  /*  "Nro Ordem Investimento    " */
          chWSheet2:Range("AI" + string(i-line-count)):VALUE = item-uni-estab.ponto-encomenda                                                                            /*  "Ponto de Encomenda do Item" */
          chWSheet2:Range("AJ" + string(i-line-count)):VALUE = item-uni-estab.consumo-prev                                                                               /*  "MÇdia de Consumo          " */
          chWSheet2:Range("AK" + string(i-line-count)):VALUE = de-saldo                                                                                                  /*  "Saldo do Item em estoque  " */
          chWSheet2:Range("AL" + string(i-line-count)):VALUE = item-uni-estab.res-for-comp                                                                               /*  "Tempo Fornec              " */
          chWSheet2:Range("AM" + string(i-line-count)):VALUE = item-uni-estab.ressup-fabri                                                                               /*  "Tempo Fabric              " */
          chWSheet2:Range("AN" + string(i-line-count)):VALUE = item-uni-estab.res-int-comp                                                                               /*  "Tempo Compras             " */
          chWSheet2:Range("AO" + string(i-line-count)):VALUE = IF AVAILABLE(cotacao-item) THEN cotacao-item.prazo-entreg ELSE 0                                          /*  "Prazo de entrega do pedido" */
          chWSheet2:Range("AP" + string(i-line-count)):VALUE = IF AVAILABLE(pedido-compr) THEN pedido-compr.end-entrega  ELSE "" .                                       /*  "Estabelecimento           " */


   RUN pi-formato("A"  + string(i-line-count) ,"@"). /*  "Fornecedor                " */
   RUN pi-formato("B"  + string(i-line-count) ,"@"). /*  "Nome Abrev Fornec         " */
   RUN pi-formato("C"  + string(i-line-count) ,"@"). /*  "Familia                   " */
   RUN pi-formato("D"  + string(i-line-count) ,"@"). /*  "Familia Cml               " */
   RUN pi-formato("E"  + string(i-line-count) ,"@"). /*  "Item                      " */
   RUN pi-formato("F"  + string(i-line-count) ,"@"). /*  "Descriá∆o item            " */
   RUN pi-formato("K"  + string(i-line-count) ,"@"). /*  "Moeda                     " */
   RUN pi-formato("O"  + string(i-line-count) ,"@"). /*  "Emergencial               " */
   RUN pi-formato("P"  + string(i-line-count) ,"@"). /*  "Requisitante              " */
   RUN pi-formato("Q"  + string(i-line-count) ,"@"). /*  "Nome Requisitante         " */
   RUN pi-formato("R"  + string(i-line-count) ,"@"). /*  "Comprador                 " */
   RUN pi-formato("S"  + string(i-line-count) ,"@"). /*  "Nome Comprador            " */
   RUN pi-formato("T"  + string(i-line-count) ,"@"). /*  "Pedido                    " */
   RUN pi-formato("X"  + string(i-line-count) ,"@"). /*  "Ordem                     " */
   RUN pi-formato("Z"  + string(i-line-count) ,"@"). /*  "Narrativa                 " */
   RUN pi-formato("AA" + string(i-line-count) ,"@"). /*  "Nß Requisiá∆o             " */
   RUN pi-formato("AB" + string(i-line-count) ,"@"). /*  "Grupo de Estoque          " */
   RUN pi-formato("AC" + string(i-line-count) ,"@"). /*  "Tipo do Item              " */
   RUN pi-formato("AD" + string(i-line-count) ,"@"). /*  "Situaá∆o do Item          " */
   RUN pi-formato("AE" + string(i-line-count) ,"@"). /*  "Tipo Demanda              " */
   RUN pi-formato("AF" + string(i-line-count) ,"@"). /*  "Tipo Controle do Item     " */
   RUN pi-formato("AG" + string(i-line-count) ,"@"). /*  "Departamento              " */
   RUN pi-formato("AH" + string(i-line-count) ,"@"). /*  "Nro Ordem Investimento    " */
   RUN pi-formato("AP" + string(i-line-count) ,"@"). /*  "Estabelecimento           " */


   RUN pi-formato("N"  + string(i-line-count) ,"dd/mm/aaaa;@"    ). /*  "Data Ordem                " */
   RUN pi-formato("Y"  + string(i-line-count) ,"dd/mm/aaaa;@"    ). /*  "Data Pedido               " */

   RUN pi-formato("U"  + string(i-line-count) ,"###.###.##0,0000"). /*  "Qtde                      " */
   RUN pi-formato("AI" + string(i-line-count) ,"###.###.##0,0000"). /*  "Ponto de Encomenda do Item" */
   RUN pi-formato("AJ" + string(i-line-count) ,"###.###.##0,0000"). /*  "MÇdia de Consumo          " */
   RUN pi-formato("AK" + string(i-line-count) ,"###.###.##0,0000"). /*  "Saldo do Item em estoque  " */
   RUN pi-formato("AL" + string(i-line-count) ,"###.###.##0"     ). /*  "Tempo Fornec              " */
   RUN pi-formato("AM" + string(i-line-count) ,"###.###.##0"     ). /*  "Tempo Fabric              " */
   RUN pi-formato("AN" + string(i-line-count) ,"###.###.##0"     ). /*  "Tempo Compras             " */
   RUN pi-formato("AO" + string(i-line-count) ,"###.###.##0"     ). /*  "Prazo de entrega do pedido" */

   IF c-desc-moeda = "Real" THEN DO:
      RUN pi-formato-moeda("G"  + string(i-line-count),5).             /*  "Preáo                     " */
      RUN pi-formato-moeda("H"  + string(i-line-count),5).             /*  "Valor IPI                 " */
      RUN pi-formato-moeda("I"  + string(i-line-count),2).             /*  "Valor Total               " */
      RUN pi-formato-moeda("J"  + string(i-line-count),4).             /*  "Valor Desc                " */
      RUN pi-formato-moeda("L"  + string(i-line-count),10).            /*  "Cotac∆o Dt Pedido         " */
      RUN pi-formato-moeda("M"  + string(i-line-count),5).             /*  "Preáo Convertido          " */
      RUN pi-formato-moeda("V"  + string(i-line-count),2).             /*  "Valor Total do pedido     " */
      RUN pi-formato-moeda("W"  + string(i-line-count),2).             /*  "Total OC Decto            " */
   END.
   ELSE DO:
      RUN pi-formato("G"  + string(i-line-count) ,"#.##0,00000").      /*  "Preáo                     " */
      RUN pi-formato("H"  + string(i-line-count) ,"#.##0,00000").      /*  "Valor IPI                 " */
      RUN pi-formato("I"  + string(i-line-count) ,"#.##0,00").         /*  "Valor Total               " */
      RUN pi-formato("J"  + string(i-line-count) ,"#.##0,0000").       /*  "Valor Desc                " */
      RUN pi-formato("L"  + string(i-line-count) ,"#.##0,0000000000"). /*  "Cotac∆o Dt Pedido         " */
      RUN pi-formato("M"  + string(i-line-count) ,"#.##0,00000").      /*  "Preáo Convertido          " */
      RUN pi-formato("V"  + string(i-line-count) ,"#.##0,00").         /*  "Valor Total do pedido     " */
      RUN pi-formato("W"  + string(i-line-count) ,"#.##0,00").         /*  "Total OC Decto            " */
   END.

   ASSIGN i-line-count = i-line-count + 1.
END PROCEDURE.

PROCEDURE pi-formato-moeda:
    DEFINE INPUT PARAMETER ip-range                  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-casas-decimais         AS INTEGER   NO-UNDO.
    
    CASE ip-casas-decimais:
         WHEN  0 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0".
         WHEN  1 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,0".
         WHEN  2 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,00".
         WHEN  3 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,000".
         WHEN  4 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,0000".
         WHEN  5 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,00000".
         WHEN  6 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,000000".
         WHEN  7 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,0000000".
         WHEN  8 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,00000000".
         WHEN  9 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,000000000".
         WHEN 10 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,0000000000".
         WHEN 11 THEN chWSheet2:Range(ip-range):NumberFormat = "R$ #.##0,00000000000".
    END CASE.

   /* chWSheet2:Range(ip-range):Style = "Currency".*/
END PROCEDURE.

PROCEDURE pi-dados-csv:
   PUT STREAM st-csv
       STRING(pedido-compr.cod-emitente)                                                                        ";"    /*  "Fornecedor                " */
       IF AVAILABLE(emitente) THEN emitente.nome-abrev ELSE ""   FORMAT "x(12)"                                 ";"    /*  "Nome Abrev Fornec         " */                 
       IF AVAILABLE(ITEM) THEN ITEM.fm-codigo  ELSE ""                                                          ";"    /*  "Familia                   " */
       IF AVAILABLE(ITEM) THEN ITEM.fm-cod-com ELSE ""                                                          ";"    /*  "Familia Cml               " */
       "'" ordem-compra.it-codigo                                                                               ";"    /*  "Item                      " */
       c-desc-item                                                                                              ";"    /*  "Descriá∆o item            " */                 
       c-simbolo + STRING(de-valor-item)     FORMAT "x(25)"                                                     ";"    /*  "Preáo                     " */                 
       c-simbolo + STRING(de-valor-ipi)      FORMAT "x(25)"                                                     ";"    /*  "Valor IPI                 " */                 
       c-simbolo + STRING(de-valor-item-un)  FORMAT "x(25)"                                                     ";"    /*  "Valor Total               " */                 
       c-simbolo + STRING(cotacao-item.valor-descto)       FORMAT "x(25)"                                       ";"    /*  "Valor Desc                " */
       c-desc-moeda                                                                                             ";"    /*  "Moeda                     " */
       d-taxa-moeda                                                                                             ";"    /*  "Cotac∆o Dt Pedido         " */
       c-simbolo + STRING(ROUND(de-valor-item-un * d-taxa-moeda,2))   FORMAT "x(25)"                            ";"    /*  "Preáo Convertido          " */                 
       ordem-compra.data-emissao                                                                                ";"    /*  "Data Ordem                " */
       IF pedido-compr.emergencial = TRUE THEN "Sim" ELSE "N∆o"                                                 ";"    /*  "Emergencial               " */
       c-requisitante                                                                                           ";"    /*  "Requisitante              " */                 
       c-nome-requisitante                                                                                      ";"    /*  "Nome Requisitante         " */                 
       c-comprador                                                                                              ";"    /*  "Comprador                 " */
       c-nome-comprador                                                                                         ";"    /*  "Nome Comprador            " */
       ordem-compra.num-pedido                                                                                  ";"    /*  "Pedido                    " */
       ordem-compra.qt-solic                                                                                    ";"    /*  "Qtde                      " */
       c-simbolo + STRING(de-total-pedido)     FORMAT "x(25)"                                                                 ";"    /*  "Valor Total do pedido     " */
       c-simbolo + STRING(ROUND((de-valor-item-un  -  cotacao-item.valor-descto)  * ordem-compra.qt-solic,2)) FORMAT "x(25)"  ";"    /*  "Total OC Decto            " */
       ordem-compra.numero-ordem                                                                                ";"    /*  "Ordem                     " */
       ordem-compra.data-pedido                                                                                 ";"    /*  "Data Pedido               " */
       descricao                                                                                                ";"    /*  "Narrativa                 " */
       ordem-compra.nr-requisicao                                                                               ";"    /*  "Nß Requisiá∆o             " */
       ITEM.ge-codigo                                                                                           ";"    /*  "Grupo de Estoque          " */
       ENTRY(ITEM.ind-serv-mat,{ininc/i08in172.i 03})               FORMAT "x(20)"                              ";"    /*  "Tipo do Item              " */
       ENTRY(ITEM.cod-obsoleto,{ininc/i17in172.i 03})               FORMAT "x(20)"                              ";"    /*  "Situaá∆o do Item          " */
       ENTRY(ITEM.demanda     ,{ininc/i02in122.i 03})               FORMAT "x(20)"                              ";"    /*  "Tipo Demanda              " */
       ENTRY(ITEM.tipo-contr  ,{ininc/i09in122.i 03})               FORMAT "x(20)"                              ";"    /*  "Tipo Controle do Item     " */
       IF AVAILABLE (es-depto) THEN es-depto.descricao ELSE ""      FORMAT "x(50)"                              ";"    /*  "Departamento              " */
       ordem-compra.num-ord-inv                                                                                 ";"    /*  "Nro Ordem Investimento    " */
       item-uni-estab.ponto-encomenda                                                                           ";"    /*  "Ponto de Encomenda do Item" */
       item-uni-estab.consumo-prev                                                                              ";"    /*  "MÇdia de Consumo          " */
       de-saldo                                                                                                 ";"    /*  "Saldo do Item em estoque  " */
       item-uni-estab.res-for-comp                                                                              ";"    /*  "Tempo Fornec              " */
       item-uni-estab.ressup-fabri                                                                              ";"    /*  "Tempo Fabric              " */
       item-uni-estab.res-int-comp                                                                              ";"    /*  "Tempo Compras             " */
       IF AVAILABLE(cotacao-item) THEN cotacao-item.prazo-entreg ELSE 0                                         ";"    /*  "Prazo de entrega do pedido" */
       IF AVAILABLE(pedido-compr) THEN pedido-compr.end-entrega  ELSE ""                                        ";"    /*  "Estabelecimento           " */
       SKIP.
END PROCEDURE.

PROCEDURE pi-precos:
   assign de-preco-unit   = 0
          de-preco        = 0
          de-aliquota-ipi = 0
          de-valor-ipi    = 0
          de-preco-orig   = 0.

   if  ordem-compra.situacao <> 1 and ordem-compra.situacao <> 5 then do:
       assign da-data = if l-conv-g then ordem-compra.data-cotacao
                                    else da-conv-g.
       if  l-preco-bruto = no then
           assign de-valor-ipi = (ordem-compra.preco-unit
                               *  ordem-compra.aliquota-ipi)
                               / (100 + ordem-compra.aliquota-ipi).
       else do:
           if  ordem-compra.perc-descto > 0 then
               assign de-val-orig = (ordem-compra.preco-unit * 100)
                                  / (100 - ordem-compra.perc-descto).
           else
               assign de-val-orig = ordem-compra.preco-unit.
           assign de-valor-ipi = (de-val-orig
                               * ordem-compra.aliquota-ipi)
                               / (100 + ordem-compra.aliquota-ipi).
       end.
       assign de-preco      = ordem-compra.preco-unit - de-valor-ipi
              de-preco-unit = ordem-compra.preco-unit
              de-preco-orig = ordem-compra.preco-orig.
       if  ordem-compra.mo-codigo <> i-moeda-g then do:
           run cdp/cd0812.p (input  ordem-compra.mo-codigo,
                             input  i-moeda-g,
                             input  de-preco,
                             input  da-data,
                             output de-preco).
           run cdp/cd0812.p (input  ordem-compra.mo-codigo,
                             input  i-moeda-g,
                             input  de-valor-ipi,
                             input  da-data,
                             output de-valor-ipi).
           run cdp/cd0812.p (input  ordem-compra.mo-codigo,
                             input  i-moeda-g,
                             input  de-preco-unit,
                             input  da-data,
                             output de-preco-unit).
           run cdp/cd0812.p (input  ordem-compra.mo-codigo,
                             input  i-moeda-g,
                             input  de-preco-orig,
                             input  da-data,
                             output de-preco-orig).
       end.
       if  de-preco        = ? then de-preco        = 0.
       if  de-aliquota-ipi = ? 
       or  de-preco        = 0 then de-aliquota-ipi = 0.
       if  de-valor-ipi    = ? then de-valor-ipi    = 0.
       if  de-preco-unit   = ? then de-preco-unit   = 0.
       if  de-preco-orig   = ? then de-preco-orig   = 0.
   end.
   else
       assign de-preco-unit   = 0
              de-preco        = 0
              de-aliquota-ipi = 0
              de-valor-ipi    = 0
              de-preco-orig   = 0.
END PROCEDURE.

PROCEDURE pi-saldo-item:
   DEFINE INPUT  PARAMETER ip-it-codigo       LIKE ITEM.it-codigo          NO-UNDO.
   DEFINE INPUT  PARAMETER ip-cod-estabel-ini LIKE estabelec.cod-estabel   NO-UNDO.
   DEFINE INPUT  PARAMETER ip-cod-estabel-fim LIKE estabelec.cod-estabel   NO-UNDO.
   DEFINE INPUT  PARAMETER ip-data            AS   DATE                    NO-UNDO.
   DEFINE OUTPUT PARAMETER de-qtidade-ini     LIKE saldo-estoq.qtidade-atu NO-UNDO.
   DEFINE OUTPUT PARAMETER de-qtidade-fin     LIKE saldo-estoq.qtidade-atu NO-UNDO.


   /* calcula o saldo inicial e final do item. */
   assign de-qtidade-ini = 0
          de-qtidade-fin = 0.
            
   for each saldo-estoq fields (saldo-estoq.qtidade-atu)
        where saldo-estoq.it-codigo    = ip-it-codigo 
          and saldo-estoq.cod-estabel  >= ip-cod-estabel-ini
          and saldo-estoq.cod-estabel  <= ip-cod-estabel-fim
        no-lock:       
        assign de-qtidade-ini = de-qtidade-ini + saldo-estoq.qtidade-atu
               de-qtidade-fin = de-qtidade-fin + saldo-estoq.qtidade-atu.
    end.

    /* Calcula o saldo inicial e final -------------------------------------------*/
    find first saldo-estoq
        where saldo-estoq.it-codigo    = ip-it-codigo
          and saldo-estoq.cod-estabel >= ip-cod-estabel-ini
          and saldo-estoq.cod-estabel <= ip-cod-estabel-fim
        no-lock no-error.

    if not avail saldo-estoq then do:
       assign de-qtidade-ini = 0
              de-qtidade-fin = 0.
    end.    
    else do:
        for each item-estab fields (cod-estabel)
            where item-estab.it-codigo = ip-it-codigo
              and item-estab.cod-estabel >= ip-cod-estabel-ini 
              and item-estab.cod-estabel <= ip-cod-estabel-fim 
            no-lock
            with 1 down:

            for each movto-estoq fields (movto-estoq.tipo-trans
                                         movto-estoq.dt-trans
                                         movto-estoq.quantidade) use-index item-data
                where movto-estoq.cod-estabel  = item-estab.cod-estabel
                  and movto-estoq.it-codigo    = ip-it-codigo
                  and movto-estoq.dt-trans    >= ip-data
                no-lock:

                if movto-estoq.tipo-trans = 1 then do:         /* Movimento de Entrada */
                    /* Se a data for maior que o perodo especificado pelo usuòrio,
                       altera a quantidade do saldo inicial/final ---------------------*/
                    if movto-estoq.dt-trans > ip-data then
                        assign
                            de-qtidade-fin = de-qtidade-fin - movto-estoq.quantidade
                            de-qtidade-ini = de-qtidade-ini - movto-estoq.quantidade.
                    else
                        assign de-qtidade-ini = de-qtidade-ini - movto-estoq.quantidade.
                end.
                else do:
                    if movto-estoq.dt-trans > ip-data then
                       assign de-qtidade-fin = de-qtidade-fin + movto-estoq.quantidade
                              de-qtidade-ini = de-qtidade-ini + movto-estoq.quantidade.
                    else
                      assign de-qtidade-ini = de-qtidade-ini + movto-estoq.quantidade.
                end.
            end.
        end.
    end.
END PROCEDURE.
/* fim do programa */

/******************************************************************************
**       Programa: RELT0128.p
**       Data....: 03/01/08
**       Autor...: DATASUL S.A.
**       Objetivo: PEDIDOS POR COMPRADOR - CT
**       VersÆo..: 1.00.000 - 01575407
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/
/******************************************************************************
**       Programa: RELT0128.p
**       Data....: 10/04/2012
**       Autor...: Thiago Coutinho
**       Objetivo: Correcao de Erros para valores negativos.
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "RELT0128".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

   DEFINE VARIABLE c-arquivo         AS CHARACTER         NO-UNDO.
DEFINE VARIABLE c-comprador         LIKE comprador.nome         NO-UNDO.
DEFINE VARIABLE c-nome-comprador    LIKE comprador.nome         NO-UNDO.

   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.

DEFINE VARIABLE de-preco         AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-preco-unit    AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-valor-ipi     AS DECIMAL FORMAT ">>>>>>,>>9.99999":U       INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-preco-orig    AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-aliquota-ipi  AS DECIMAL FORMAT "->>,>>9.99":U             INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-total-pedido  AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-valor-item    AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.
DEFINE VARIABLE de-valor-item-un AS DECIMAL FORMAT ">>>>>>>,>>9.99999":U      INITIAL 0 NO-UNDO.

DEFINE VARIABLE l-emergencial    AS LOGICAL FORMAT "Sim/NÆo" NO-UNDO.

DEFINE VARIABLE de-saldo         LIKE saldo-estoq.qtidade-atu NO-UNDO.
DEFINE VARIABLE de-qt-inicial    LIKE saldo-estoq.qtidade-atu NO-UNDO.
DEFINE VARIABLE de-qt-final      LIKE saldo-estoq.qtidade-atu NO-UNDO.

DEFINE BUFFER bf-ordem-compra FOR ordem-compra.
DEFINE BUFFER bf-prazo-compra FOR prazo-compra.

run grapi/gr2013.p (input c-prog-gerado, input "12.1.13.000").

/****************** Definicao de Tabelas Temporárias do Relatario **********************/

define temp-table tt-raw-digita
    field raw-digita as raw.


define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    FIELD formato-arquivo      AS INTEGER
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field da-dat-ordem-ini like ordem-compra.dat-ordem
    field da-dat-ordem-fim like ordem-compra.dat-ordem
    field i-cod-emitente-ini like ordem-compra.cod-emitente
    field i-cod-emitente-fim like ordem-compra.cod-emitente
    field c-cod-comprado-ini like ordem-compra.cod-comprado
    field c-cod-comprado-fim like ordem-compra.cod-comprado
    field c-cod-estabel-ini like pedido-compr.cod-estabel
    field c-cod-estabel-fim like pedido-compr.cod-estabel
    FIELD i-tipo            AS   INTEGER
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
def new global shared var v_cdn_empres_usuar     like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren     like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.

def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var da-dat-ordem-ini like ordem-compra.dat-ordem format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dat-ordem-fim like ordem-compra.dat-ordem format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var i-cod-emitente-ini like ordem-compra.cod-emitente format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-emitente-fim like ordem-compra.cod-emitente format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var c-cod-comprado-ini like ordem-compra.cod-comprado format "X(12)" initial "" no-undo.
def new shared var c-cod-comprado-fim like ordem-compra.cod-comprado format "X(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var c-cod-estabel-ini like pedido-compr.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim like pedido-compr.cod-estabel format "x(3)" initial "ZZZ" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

DEFINE VARIABLE de-quant-saldo      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-quantidade       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-quantidade-aloc  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-quant-receb      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-total            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE descricao           AS CHARACTER   NO-UNDO.

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

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

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

/* ">,>>>,>>9.99" */

form ordem-compra.cod-emitente column-label "Fornec" format ">>>>>>>>9"          
     ordem-compra.it-codigo column-label "Item" format "X(16)"                   
     ordem-compra.preco-unit column-label "Pre‡o Unit" format ">,>>>,>>9.99"     
     de-quantidade column-label "Qtde"                                                      
     de-total column-label "Vlr Total"                                                
     ordem-compra.perc-descto column-label "% Desc" format ">9.99999"      
     ordem-compra.cod-comprado column-label "Comprador" format "X(12)"     
     ordem-compra.dat-ordem column-label "Data Ordem" format "99/99/9999"  
     pedido-compr.emergencial column-label "Emergencial" format "Sim/NÆo"  
     ordem-compra.nr-contrato column-label "Contrato" format ">>>>>>>>9"   
     ordem-compra.num-pedido column-label "Pedido" format ">>>>>,>>9"      
     ordem-compra.qt-solic column-label "Qtde" format ">>>,>>>,>>9.99"
     ordem-compra.numero-ordem column-label "Ordem" format "zzzzz9,99"     
     ordem-compra.conta-contabil column-label "Conta Cont bil" format "x(17)" 
     ordem-compra.data-pedido column-label "Data Pedido" format "99/99/9999"  
     descricao column-label "Narratia" format "x(30)"                                    
     with down width 333 no-box stream-io frame f-relat-09-132.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


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

assign c-programa     = "RELT0128"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "PEDIDOS POR COMPRADOR - CT"
       c-sistema      = "EMS".


find first mguni.empresa no-lock
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

DEFINE STREAM st-csv.


end. /* tt-param.formato = 2 */

/*
run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).
                    */

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       da-dat-ordem-ini = tt-param.da-dat-ordem-ini
       da-dat-ordem-fim = tt-param.da-dat-ordem-fim
       i-cod-emitente-ini = tt-param.i-cod-emitente-ini
       i-cod-emitente-fim = tt-param.i-cod-emitente-fim
       c-cod-comprado-ini = tt-param.c-cod-comprado-ini
       c-cod-comprado-fim = tt-param.c-cod-comprado-fim
       c-cod-estabel-ini = tt-param.c-cod-estabel-ini
       c-cod-estabel-fim = tt-param.c-cod-estabel-fim
.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

IF tt-param.formato-arquivo = 1 THEN DO:
   RUN pi-abre-excel.
   RUN pi-dados.
   RUN pi-encerra-excel.
END.
ELSE DO:
   RUN pi-abre-csv.
   RUN pi-dados.
   RUN pi-encerra-csv.
END.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.


PROCEDURE pi-abre-csv:
   ASSIGN tt-param.arquivo =  SESSION:TEMP-DIRECTORY + 'RELT01AB_' + REPLACE (STRING(TODAY,'99/99/9999'),'/','') + REPLACE (STRING(TIME,'hh:mm:ss'),':','') + ".csv".
    
   FILE-INFO:FILE-NAME = tt-param.arquivo.
   
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
   
   OUTPUT STREAM st-csv TO VALUE (tt-param.arquivo) NO-CONVERT.
   
   PUT STREAM st-csv UNFORMAT
       "Emitente;"
       "Nome Abrev Fornec;"
       "Familia;"
       "Familia Cml;"
       "Item;"
       "Descri‡Æo item;"
       "Preco Unit;"
       "Qtde;"
       "Total;"
       "Perc Descto;"
       "Cod Comprador;"
       "Nome Comprador;"
       "Dat Ordem;"
       "Emergencial;"
       "Num. Contrato;"
       "Tp Contrato;"
       "Num. Pedido;"
       "Num. Ordem;"
       "Data Pedido;"
       "Narrativa;"
       "N§ Requisi‡Æo;"
       "Grupo de Estoque;"
       "Tipo do Item;"
       "Situa‡Æo do Item;"
       "Tipo Demanda;"
       "Tipo Controle do Item;"
       "Departamento;"
       "Nro Ordem Investimento;"
       "Ponto de Encomenda do Item;"
       "M‚dia de Consumo;"
       "Saldo do Item em estoque;"
       "Tempo Fornec;"
       "Tempo Fabric;"
       "Tempo Compras;"
       "Prazo de entrega do pedido;"
       "Estabelecimento;"
       SKIP.
END PROCEDURE.

PROCEDURE pi-dados:
    assign v-num-reg-lidos = 0.
   for each ordem-compra no-lock
       where ordem-compra.cod-comprado >= c-cod-comprado-ini and 
             ordem-compra.cod-comprado <= c-cod-comprado-fim and
             ordem-compra.cod-emitente >= i-cod-emitente-ini and 
             ordem-compra.cod-emitente <= i-cod-emitente-fim and
             ordem-compra.dat-ordem >= da-dat-ordem-ini and 
             ordem-compra.dat-ordem <= da-dat-ordem-fim and
             /*ordem-compra.it-codigo  <= "99990001" AND*/
             ordem-compra.situacao   <> 4,
       EACH item-contrat
            where  item-contrat.nr-contrato   = ordem-compra.nr-contrato
            and    item-contrat.num-seq-item = ordem-compra.num-seq-item
            AND    ((tt-param.i-tipo = 4) OR (item-contrat.ind-tipo-control = tt-param.i-tipo))
            no-lock,
       EACH ITEM OF ordem-compra NO-LOCK,
       each pedido-compr no-lock
           where ordem-compra.num-pedido = pedido-compr.num-pedido AND
                 pedido-compr.cod-estabel >= c-cod-estabel-ini and 
                 pedido-compr.cod-estabel <= c-cod-estabel-fim AND
                 pedido-compr.nr-contrato  <> 0
       break by ordem-compra.nr-contrato:
   
       assign v-num-reg-lidos = v-num-reg-lidos + 1.
       run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
   
       assign de-quant-saldo     = 0
              de-quantidade      = 0
              de-quantidade-aloc = 0
              de-quant-receb     = 0.
       for each prazo-compra
           where prazo-compra.numero-ordem = ordem-compra.numero-ordem no-lock:
   
           assign de-quantidade      = de-quantidade + prazo-compra.quantidade
                  de-quantidade-aloc = de-quantidade-aloc + prazo-compra.dec-1
                  de-quant-receb     = de-quant-receb + prazo-compra.quant-receb.
   
           if  prazo-compra.situacao <> 4 then 
               assign de-quant-saldo = de-quant-saldo + prazo-compra.quant-saldo.
       end.
   
       FIND FIRST emitente
            WHERE emitente.cod-emitente = pedido-compr.cod-emitente
            NO-LOCK NO-ERROR.
   
       ASSIGN de-total = de-quantidade * ordem-compra.preco-unit.
   
       FOR FIRST es-it-depto
                   WHERE es-it-depto.it-codigo = item.it-codigo
                   NO-LOCK,
                   FIRST es-depto
                         WHERE es-depto.codigo = es-it-depto.cod-depto
                         NO-LOCK:
       END.
   
       FIND FIRST item-uni-estab
            WHERE item-uni-estab.it-codigo   = ordem-compra.it-codigo AND
                  item-uni-estab.cod-estabel = ordem-compra.cod-estabel
            NO-LOCK NO-ERROR.
   
       FIND FIRST cotacao-item OF ordem-compra
            WHERE cotacao-item.cot-aprovada
            NO-LOCK NO-ERROR.
   
       RUN pi-saldo-item (INPUT  ITEM.it-codigo,
                          INPUT  "",
                          INPUT  "zzzzzz",
                          INPUT ordem-compra.data-emissao,
                          OUTPUT de-qt-inicial,
                          OUTPUT de-qt-final).
   
       descricao = REPLACE(ordem-compra.narrativa,chr(10)," ").
       descricao = REPLACE(descricao,chr(10)," ").
       descricao = REPLACE(descricao,chr(11)," ").
       descricao = REPLACE(descricao,chr(12)," ").
       descricao = REPLACE(descricao,chr(13)," ").
       descricao = REPLACE(descricao,";"," ").
   
       ASSIGN c-comprador      = STRING(ordem-compra.cod-comprado, "99999999999")
              c-nome-comprador = "".
   
       FIND FIRST comprador 
            WHERE comprador.cod-comprado = ordem-compra.cod-comprado
            NO-LOCK NO-ERROR.
       IF AVAILABLE(comprador) THEN
          ASSIGN c-nome-comprador = comprador.nome.

       IF tt-param.formato-arquivo = 1 THEN
          RUN pi-dados-excel.
       ELSE
          RUN pi-dados-csv.
   END.
END PROCEDURE.

PROCEDURE pi-dados-csv:
   PUT STREAM st-csv UNFORMAT
       ordem-compra.cod-emitente                                               ";"  /* "Emitente;"                    */
       emitente.nome-abrev                                                     ";"  /* "Nome Abrev Fornec;"           */
       ITEM.fm-codigo                                                          ";"  /* "Familia;"                     */
       ITEM.fm-cod-com                                                         ";"  /* "Familia Cml;"                 */
       ITEM.it-codigo                                                          ";"  /* "Item;"                        */
       ITEM.desc-item                                                          ";"  /* "Descri‡Æo item;"              */
       ordem-compra.preco-unit                                                 ";"  /* "Preco Unit;"                  */
       de-quantidade                                                           ";"  /* "Qtde;"                        */
       de-total                                                                ";"  /* "Total;"                       */
       ordem-compra.perc-descto                                                ";"  /* "Perc Descto;"                 */
       c-comprador                                                             ";"  /* "Cod Comprador;"               */
       c-nome-comprador                                                        ";"  /* "Nome Comprador;"              */
       ordem-compra.dat-ordem                                                  ";"  /* "Dat Ordem;"                   */
       string(pedido-compr.emergencial,"SIM/NÇO")                              ";"  /* "Emergencial;"                 */
       ordem-compra.nr-contrato                                                ";"  /* "Num. Contrato;"               */
       {ininc/i04in582.i 04 item-contrat.ind-tipo-control}                     ";"  /* "Tp Contrato;"                 */
       ordem-compra.num-pedido                                                 ";"  /* "Num. Pedido;"                 */
       ordem-compra.numero-ordem                                               ";"  /* "Num. Ordem;"                  */
       ordem-compra.data-pedido                                                ";"  /* "Data Pedido;"                 */
       descricao                                                               ";"  /* "Narrativa;"                   */
       ordem-compra.nr-requisicao                                              ";"  /* "N§ Requisi‡Æo;"               */
       ITEM.ge-codigo                                                          ";"  /* "Grupo de Estoque;"            */
       ENTRY(ITEM.ind-serv-mat,{ininc/i08in172.i 03})          FORMAT "x(20)"  ";"  /* "Tipo do Item;"                */
       ENTRY(ITEM.cod-obsoleto,{ininc/i17in172.i 03})          FORMAT "x(20)"  ";"  /* "Situa‡Æo do Item;"            */
       ENTRY(ITEM.demanda     ,{ininc/i02in122.i 03})          FORMAT "x(20)"  ";"  /* "Tipo Demanda;"                */
       ENTRY(ITEM.tipo-contr  ,{ininc/i09in122.i 03})          FORMAT "x(20)"  ";"  /* "Tipo Controle do Item;"       */
       IF AVAILABLE (es-depto) THEN es-depto.descricao ELSE "" FORMAT "x(50)"  ";"  /* "Departamento;"                */
       ordem-compra.num-ord-inv                                                ";"  /* "Nro Ordem Investimento;"      */
       item-uni-estab.ponto-encomenda                                          ";"  /* "Ponto de Encomenda do Item;"  */
       item-uni-estab.consumo-prev                                             ";"  /* "M‚dia de Consumo;"            */
       de-saldo                                                                ";"  /* "Saldo do Item em estoque;"    */
       item-uni-estab.res-for-comp                                             ";"  /* "Tempo Fornec;"                */
       item-uni-estab.ressup-fabri                                             ";"  /* "Tempo Fabric;"                */
       item-uni-estab.res-int-comp                                             ";"  /* "Tempo Compras;"               */
       ordem-compra.prazo-entreg                                               ";"  /* "Prazo de entrega do pedido;"  */
       pedido-compr.end-entrega                                                ";"  /* "Estabelecimento;".            */
       SKIP.

END PROCEDURE.

PROCEDURE pi-encerra-csv:
   OUTPUT CLOSE.

   IF i-num-ped-exec-rpw = 0 THEN
      DOS SILENT START excel.exe VALUE(tt-param.arquivo).
END PROCEDURE.



/* fim do programa */
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
                    /* Se a data for maior que o perðodo especificado pelo usu˜rio,
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
   ch-excel:ActiveSheet:NAME = "RELT01AB".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:columns( "A"):NumberFormat = "@"               /* "Emitente;"                    */
          ch-excel:columns( "B"):NumberFormat = "@"               /* "Nome Abrev Fornec;"           */
          ch-excel:columns( "C"):NumberFormat = "@"               /* "Familia;"                     */
          ch-excel:columns( "D"):NumberFormat = "@"               /* "Familia Cml;"                 */
          ch-excel:columns( "E"):NumberFormat = "@"               /* "Item;"                        */
          ch-excel:columns( "F"):NumberFormat = "@"               /* "Descri‡Æo item;"              */
          ch-excel:columns( "G"):NumberFormat = "#.##0,00000"     /* "Preco Unit;"                  */
          ch-excel:columns( "H"):NumberFormat = "#.##0,0000"      /* "Qtde;"                        */
          ch-excel:columns( "I"):NumberFormat = "#.##0,0000"      /* "Total;"                       */
          ch-excel:columns( "J"):NumberFormat = "#.##0,00000"     /* "Perc Descto;"                 */
          ch-excel:columns( "K"):NumberFormat = "@"               /* "Cod Comprador;"               */
          ch-excel:columns( "L"):NumberFormat = "@"               /* "Nome Comprador;"              */
          ch-excel:columns( "M"):NumberFormat = "dd/mm/aaaa;@"    /* "Dat Ordem;"                   */
          ch-excel:columns( "N"):NumberFormat = "@"               /* "Emergencial;"                 */
          ch-excel:columns( "O"):NumberFormat = "@"               /* "Num. Contrato;"               */
          ch-excel:columns( "P"):NumberFormat = "@"               /* "Tp Contrato;"                 */
          ch-excel:columns( "Q"):NumberFormat = "@"               /* "Num. Pedido;"                 */   
          ch-excel:columns( "R"):NumberFormat = "@"               /* "Num. Ordem;"                  */   
          ch-excel:columns( "S"):NumberFormat = "dd/mm/aaaa;@"    /* "Data Pedido;"                 */   
          ch-excel:columns( "T"):NumberFormat = "@"               /* "Narrativa;"                   */
          ch-excel:columns( "U"):NumberFormat = "@"               /* "N§ Requisi‡Æo;"               */
          ch-excel:columns( "V"):NumberFormat = "@"               /* "Grupo de Estoque;"            */
          ch-excel:columns( "W"):NumberFormat = "@"               /* "Tipo do Item;"                */
          ch-excel:columns( "X"):NumberFormat = "@"               /* "Situa‡Æo do Item;"            */
          ch-excel:columns( "Y"):NumberFormat = "@"               /* "Tipo Demanda;"                */
          ch-excel:columns( "Z"):NumberFormat = "@"               /* "Tipo Controle do Item;"       */
          ch-excel:columns("AA"):NumberFormat = "@"               /* "Departamento;"                */
          ch-excel:columns("AB"):NumberFormat = "@"               /* "Nro Ordem Investimento;"      */
          ch-excel:columns("AC"):NumberFormat = "#.##0,0000"      /* "Ponto de Encomenda do Item;"  */
          ch-excel:columns("AD"):NumberFormat = "#.##0,0000"      /* "M‚dia de Consumo;"            */
          ch-excel:columns("AE"):NumberFormat = "#.##0,0000"      /* "Saldo do Item em estoque;"    */
          ch-excel:columns("AF"):NumberFormat = "#.##0"           /* "Tempo Fornec;"                */
          ch-excel:columns("AG"):NumberFormat = "#.##0"           /* "Tempo Fabric;"                */
          ch-excel:columns("AH"):NumberFormat = "#.##0"           /* "Tempo Compras;"               */
          ch-excel:columns("AI"):NumberFormat = "#.##0"           /* "Prazo de entrega do pedido;"  */
          ch-excel:columns("AJ"):NumberFormat = "@"               /* "Estabelecimento;".            */
     .

   ASSIGN ch-excel:Range( "A"  + STRING(i-cont-linha,'99999')):VALUE = "Emitente"                  
          ch-excel:Range( "B"  + STRING(i-cont-linha,'99999')):VALUE = "Nome Abrev Fornec"         
          ch-excel:Range( "C"  + STRING(i-cont-linha,'99999')):VALUE = "Familia"                   
          ch-excel:Range( "D"  + STRING(i-cont-linha,'99999')):VALUE = "Familia Cml"               
          ch-excel:Range( "E"  + STRING(i-cont-linha,'99999')):VALUE = "Item"                      
          ch-excel:Range( "F"  + STRING(i-cont-linha,'99999')):VALUE = "Descri‡Æo item"            
          ch-excel:Range( "G"  + STRING(i-cont-linha,'99999')):VALUE = "Preco Unit"                
          ch-excel:Range( "H"  + STRING(i-cont-linha,'99999')):VALUE = "Qtde"                      
          ch-excel:Range( "I"  + STRING(i-cont-linha,'99999')):VALUE = "Total"                     
          ch-excel:Range( "J"  + STRING(i-cont-linha,'99999')):VALUE = "Perc Descto"               
          ch-excel:Range( "K"  + STRING(i-cont-linha,'99999')):VALUE = "Cod Comprador"             
          ch-excel:Range( "L"  + STRING(i-cont-linha,'99999')):VALUE = "Nome Comprador"            
          ch-excel:Range( "M"  + STRING(i-cont-linha,'99999')):VALUE = "Dat Ordem"                 
          ch-excel:Range( "N"  + STRING(i-cont-linha,'99999')):VALUE = "Emergencial"               
          ch-excel:Range( "O"  + STRING(i-cont-linha,'99999')):VALUE = "Num. Contrato"             
          ch-excel:Range( "P"  + STRING(i-cont-linha,'99999')):VALUE = "Tp Contrato"               
          ch-excel:Range( "Q"  + STRING(i-cont-linha,'99999')):VALUE = "Num. Pedido"               
          ch-excel:Range( "R"  + STRING(i-cont-linha,'99999')):VALUE = "Num. Ordem"                
          ch-excel:Range( "S"  + STRING(i-cont-linha,'99999')):VALUE = "Data Pedido"               
          ch-excel:Range( "T"  + STRING(i-cont-linha,'99999')):VALUE = "Narrativa"                 
          ch-excel:Range( "U"  + STRING(i-cont-linha,'99999')):VALUE = "N§ Requisi‡Æo"             
          ch-excel:Range( "V"  + STRING(i-cont-linha,'99999')):VALUE = "Grupo de Estoque"          
          ch-excel:Range( "W"  + STRING(i-cont-linha,'99999')):VALUE = "Tipo do Item"              
          ch-excel:Range( "X"  + STRING(i-cont-linha,'99999')):VALUE = "Situa‡Æo do Item"          
          ch-excel:Range( "Y"  + STRING(i-cont-linha,'99999')):VALUE = "Tipo Demanda"              
          ch-excel:Range( "Z"  + STRING(i-cont-linha,'99999')):VALUE = "Tipo Controle do Item"     
          ch-excel:Range( "AA" + STRING(i-cont-linha,'99999')):VALUE = "Departamento"              
          ch-excel:Range( "AB" + STRING(i-cont-linha,'99999')):VALUE = "Nro Ordem Investimento"    
          ch-excel:Range( "AC" + STRING(i-cont-linha,'99999')):VALUE = "Ponto de Encomenda do Item"
          ch-excel:Range( "AD" + STRING(i-cont-linha,'99999')):VALUE = "M‚dia de Consumo"          
          ch-excel:Range( "AE" + STRING(i-cont-linha,'99999')):VALUE = "Saldo do Item em estoque"  
          ch-excel:Range( "AF" + STRING(i-cont-linha,'99999')):VALUE = "Tempo Fornec"              
          ch-excel:Range( "AG" + STRING(i-cont-linha,'99999')):VALUE = "Tempo Fabric"              
          ch-excel:Range( "AH" + STRING(i-cont-linha,'99999')):VALUE = "Tempo Compras"             
          ch-excel:Range( "AI" + STRING(i-cont-linha,'99999')):VALUE = "Prazo de entrega do pedido"
          ch-excel:Range( "AJ" + STRING(i-cont-linha,'99999')):VALUE = "Estabelecimento".           


   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-dados-excel:  
   ASSIGN ch-excel:Range( "A"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.cod-emitente                                                 /* "Emitente;"                    */
          ch-excel:Range( "B"  + STRING(i-cont-linha,'99999')):VALUE = emitente.nome-abrev                                                       /* "Nome Abrev Fornec;"           */
          ch-excel:Range( "C"  + STRING(i-cont-linha,'99999')):VALUE = ITEM.fm-codigo                                                            /* "Familia;"                     */
          ch-excel:Range( "D"  + STRING(i-cont-linha,'99999')):VALUE = ITEM.fm-cod-com                                                           /* "Familia Cml;"                 */
          ch-excel:Range( "E"  + STRING(i-cont-linha,'99999')):VALUE = ITEM.it-codigo                                                            /* "Item;"                        */
          ch-excel:Range( "F"  + STRING(i-cont-linha,'99999')):VALUE = ITEM.desc-item                                                            /* "Descri‡Æo item;"              */
          ch-excel:Range( "G"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.preco-unit                                                   /* "Preco Unit;"                  */
          ch-excel:Range( "H"  + STRING(i-cont-linha,'99999')):VALUE = de-quantidade                                                             /* "Qtde;"                        */
          ch-excel:Range( "I"  + STRING(i-cont-linha,'99999')):VALUE = de-total                                                                  /* "Total;"                       */
          ch-excel:Range( "J"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.perc-descto                                                  /* "Perc Descto;"                 */
          ch-excel:Range( "K"  + STRING(i-cont-linha,'99999')):VALUE = c-comprador                                                               /* "Cod Comprador;"               */
          ch-excel:Range( "L"  + STRING(i-cont-linha,'99999')):VALUE = c-nome-comprador                                                          /* "Nome Comprador;"              */
          ch-excel:Range( "M"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.dat-ordem                                                    /* "Dat Ordem;"                   */
          ch-excel:Range( "N"  + STRING(i-cont-linha,'99999')):VALUE = string(pedido-compr.emergencial,"SIM/NÇO")                                /* "Emergencial;"                 */
          ch-excel:Range( "O"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.nr-contrato                                                  /* "Num. Contrato;"               */
          ch-excel:Range( "P"  + STRING(i-cont-linha,'99999')):VALUE = {ininc/i04in582.i 04 item-contrat.ind-tipo-control}                       /* "Tp Contrato;"                 */
          ch-excel:Range( "Q"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.num-pedido                                                   /* "Num. Pedido;"                 */
          ch-excel:Range( "R"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.numero-ordem                                                 /* "Num. Ordem;"                  */
          ch-excel:Range( "S"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.data-pedido                                                  /* "Data Pedido;"                 */
          ch-excel:Range( "T"  + STRING(i-cont-linha,'99999')):VALUE = descricao                                                                 /* "Narrativa;"                   */
          ch-excel:Range( "U"  + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.nr-requisicao                                                /* "N§ Requisi‡Æo;"               */
          ch-excel:Range( "V"  + STRING(i-cont-linha,'99999')):VALUE = ITEM.ge-codigo                                                            /* "Grupo de Estoque;"            */
          ch-excel:Range( "W"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(ITEM.ind-serv-mat,{ininc/i08in172.i 03})                            /* "Tipo do Item;"                */
          ch-excel:Range( "X"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(ITEM.cod-obsoleto,{ininc/i17in172.i 03})                            /* "Situa‡Æo do Item;"            */
          ch-excel:Range( "Y"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(ITEM.demanda     ,{ininc/i02in122.i 03})                            /* "Tipo Demanda;"                */
          ch-excel:Range( "Z"  + STRING(i-cont-linha,'99999')):VALUE = ENTRY(ITEM.tipo-contr  ,{ininc/i09in122.i 03})                            /* "Tipo Controle do Item;"       */
          ch-excel:Range( "AA" + STRING(i-cont-linha,'99999')):VALUE = IF AVAILABLE (es-depto) THEN es-depto.descricao ELSE ""                   /* "Departamento;"                */
          ch-excel:Range( "AB" + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.num-ord-inv                                                  /* "Nro Ordem Investimento;"      */
          ch-excel:Range( "AC" + STRING(i-cont-linha,'99999')):VALUE = item-uni-estab.ponto-encomenda                                            /* "Ponto de Encomenda do Item;"  */
          ch-excel:Range( "AD" + STRING(i-cont-linha,'99999')):VALUE = item-uni-estab.consumo-prev                                               /* "M‚dia de Consumo;"            */
          ch-excel:Range( "AE" + STRING(i-cont-linha,'99999')):VALUE = de-saldo                                                                  /* "Saldo do Item em estoque;"    */
          ch-excel:Range( "AF" + STRING(i-cont-linha,'99999')):VALUE = item-uni-estab.res-for-comp                                               /* "Tempo Fornec;"                */
          ch-excel:Range( "AG" + STRING(i-cont-linha,'99999')):VALUE = item-uni-estab.ressup-fabri                                               /* "Tempo Fabric;"                */
          ch-excel:Range( "AH" + STRING(i-cont-linha,'99999')):VALUE = item-uni-estab.res-int-comp                                               /* "Tempo Compras;"               */
          ch-excel:Range( "AI" + STRING(i-cont-linha,'99999')):VALUE = ordem-compra.prazo-entreg                                                 /* "Prazo de entrega do pedido;"  */
          ch-excel:Range( "AJ" + STRING(i-cont-linha,'99999')):VALUE = pedido-compr.end-entrega                                                  /* "Estabelecimento;".            */
              .

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   ch-excel:Range("A1:AJ" + string(i-cont-linha - 1)):autofilter(,,,).

   ch-excel:Range("A1:AJ" + string(i-cont-linha - 1)):select.

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

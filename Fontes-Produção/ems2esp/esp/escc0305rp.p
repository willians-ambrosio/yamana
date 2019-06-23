/***************************************************************************************
** Programa : escc0305rp.p
** Objetivo : Emissao de Pedido de Compra
** Autor    : Helder Prado Bustamante
** Empresa  : Datasul S∆o Paulo - GWA
** Data     : 20 de Janeiro de 2006PrintOut
***************************************************************************************/
def buffer moeda       for ems2cadme.moeda.

/*************** Eliana Martins - 24/02/2011 
Ajustado formataá∆o do campo CEP:
De:   c-cep,"99999-999"
PARA: c-cep,"99999999" ********************/


{include/i-rpvar.i}
{include/tt-edit.i}
{include/pi-edit.i}

{utp/ut-glob.i}

/*{utp/utapi019.i}*/
{utp/utapi009.i}

def new shared var c-dest-e-mail  as char format "x(400)"    no-undo.
def new shared var w-cod-emitente like emitente.cod-emitente no-undo.
def new shared var w-nome-abrev   like emitente.nome-abrev   no-undo.
def var h-acomp      as handle                               no-undo.
def var c-linha      as char format "x(2000)"                no-undo.
def var c-det-cabec  as char format "x(23)"                  no-undo.
def var c-prg-obj    as char                                 no-undo.
def var c-prg-vrs    as char                                 no-undo.
def var c-remetente  as char format "x(80)"                  no-undo.
def var c-cep        as char                                 no-undo.
def var c-moeda      as char format "x(40)"                  no-undo.
def var c-comprador  as char format "x(40)"                  no-undo.
def var c-cond-pag   as char format "x(40)"                  no-undo.
def var i-conta      as INT                                  no-undo.
def var i-linha-aux  as int                                  no-undo.
def var i-int-aux1   as int                                  no-undo.
def var i-int-aux2   as int                                  no-undo.

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.

DEFINE NEW GLOBAL SHARED VARIABLE l-exibe              AS LOGICAL NO-UNDO.

DEFINE VARIABLE  i-ep-codigo-usuario1 AS CHARACTER   NO-UNDO.


DEFINE VARIABLE c-uf-estab    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-uf-emitente AS CHARACTER   NO-UNDO.

DEF VAR i-conta-contabil AS CHAR FORMAT "x(16)".
DEF VAR c-ct-conta       AS CHAR FORMAT "x(08)".
DEF VAR c-sc-conta       AS CHAR FORMAT "x(08)".  
DEF VAR c-arq-modelo     AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR l-aprovado       AS LOG                  NO-UNDO.
DEF VAR i-ordem-invest   AS INT                  NO-UNDO.
DEF VAR c-data-emissao   AS CHAR                 NO-UNDO.                      
DEF VAR c-aprovador      AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR c-pendente       AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR c-rejeitado      AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR c-frete          AS CHAR                 NO-UNDO.
DEF VAR c-cod-aprov      AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR i-doc-pend-aprov AS INT                  NO-UNDO. 

/* Begins REV001 - Ajuste de Casas Decimais / 06/09/2018 */
DEFINE VARIABLE deSaldoQtde LIKE prazo-compra.qtd-sal-forn NO-UNDO.
/* End REV001 */
def temp-table tt-param no-undo
    field destino                    as int
    field arquivo                    as char format "x(35)"
    field usuario                    as char format "x(12)"
    field data-exec                  as date
    field hora-exec                  as int
    field classifica                 as int
    field desc-classifica            as char format "x(40)"
    field i-ini-num-pedido           as int  format "99999999"
    field i-fim-num-pedido           as int  format "99999999"
    field i-ini-numero-ordem         as int  format "99999999"
    field i-fim-numero-ordem         as int  format "99999999"
    field l-contrato                 AS LOG
    field l-narrativa-ord-compr      AS LOG
    field l-narrativa-item-ord-compr AS LOG
    field l-narrativa-pedido         AS LOG
    field l-descricao                AS LOG
    field l-envia-email              AS LOG
    FIELD i-idioma                   AS INT.

def temp-table tt-digita no-undo
    field nat-operacao as char format "X.XX-XXX" column-label "Nat Operacao"
    field denominacao  as char format "x(35)"    column-label "Denominacao"
    field tipo         as char format "x(30)"    column-label "Tipo"
    index id nat-operacao.



DEF TEMP-TABLE tt-beneficio-valid
    FIELD  cod-beneficio  AS INT
    FIELD  cod-estabel    AS CHAR
    FIELD  it-codigo      AS CHAR FORMAT "x(16)"
    FIELD  classe         AS CHAR
    FIELD  pri-icms       AS INT
    FIELD  pri-pis-cofins AS INT
    FIELD  tp-tax         AS CHAR
    FIELD  r-ben-row      AS ROWID.


DEF TEMP-TABLE tt-beneficio
    FIELD  cod-beneficio AS INT
    FIELD  it-codigo     AS CHAR.




DEF TEMP-TABLE tt-beneficio2
    FIELD  cod-beneficio AS INT
    FIELD  it-codigo     AS CHAR
    FIELD  cod-estabel   AS CHAR.






DEF TEMP-TABLE tt-msg
    FIELD cod-msg   AS INT
    FIELD it-codigo AS CHAR
    FIELD tp-msg    AS CHAR.


def temp-table tt-raw-digita
    field raw-digita as raw.

def buffer b-pedido-compr for pedido-compr.
DEF BUFFER b_usuar_mestre FOR usuar_mestre.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* {include/i-rpcab.i} */
/*                     */
/* {include/i-rpout.i} */

IF l-exibe THEN DO:
   run utp/ut-acomp.p persistent set h-acomp.
   run pi-inicializar in h-acomp(input string(c-titulo-relat)).
END.

assign c-programa = "ESCC0305RP"
       c-versao   = "2.04"
       c-revisao  = "00.000"
       c-sistema  = "COMPRAS".

/* variaveis utilizadas para a geraá∆o do EXCEL */
def new shared var c-planilha-envio as char       no-undo.
def var chExcelApplication          as com-handle no-undo.
def var chWorkbook                  as com-handle no-undo.
def var chWorkSheet                 as com-handle no-undo.
def var chActiveWorkbook            as com-handle no-undo.
def var c-planilha                  as char       no-undo.
def var c-range                     as char       no-undo.
def var i-celula                    as int        no-undo.
def var i-linha                     as int        no-undo.
def var i-conta-linha               as int        no-undo.
def var i-linha-ini                 as int        no-undo.
def var qtd-linha                   as int        no-undo.

DEFINE NEW GLOBAL SHARED VARIABLE c-arquivo AS CHARACTER NO-UNDO.

def var vl-tot-pedido like ordem-compra.preco-unit no-undo.


FIND FIRST trad_org_ext WHERE trad_org_ext.cod_tip_unid_organ = "998"                  
                        AND   trad_org_ext.cod_matriz_trad_org_ext = "ems2"            
                        AND   trad_org_ext.cod_unid_organ_ext = i-ep-codigo-usuario NO-LOCK NO-ERROR.    
IF AVAIL trad_org_ext THEN DO: 
    ASSIGN  i-ep-codigo-usuario1 =  trad_org_ext.cod_unid_organ.
END.
ELSE ASSIGN i-ep-codigo-usuario1 = i-ep-codigo-usuario.


for each pedido-compr no-lock
   where pedido-compr.num-pedido >= tt-param.i-ini-num-pedido
     and pedido-compr.num-pedido <= tt-param.i-fim-num-pedido:


    FIND emitente WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:
        ASSIGN c-uf-emitente = emitente.estado.
    END.

    IF l-exibe THEN
       run pi-acompanhar in h-acomp(input "Pedido : " + string(pedido-compr.num-pedido)).

    /*************** Daniel Nascimento - 08/03/2010 ********************/
    ASSIGN c-arq-modelo     = ""
           c-aprovador      = "" 
           c-pendente       = "" 
           c-rejeitado      = ""
           c-cod-aprov      = ""
           i-doc-pend-aprov = 1.
        
    IF tt-param.i-idioma = 1 THEN                             
      ASSIGN c-arq-modelo = "modelos\modeloESCC0305_PORT.xls".
    ELSE                                                      
      ASSIGN c-arq-modelo = "modelos\modeloESCC0305_ING.xls". 

    FOR EACH doc-pend-aprov NO-LOCK
       WHERE doc-pend-aprov.num-pedido  = pedido-compr.num-pedido
       AND  (doc-pend-aprov.ind-tip-doc = 4   /* Pedido             */
       OR    doc-pend-aprov.ind-tip-doc = 6): /* Pedido Emergencial */
    
      IF doc-pend-aprov.ind-situacao = 2 OR doc-pend-aprov.ind-situacao = 4 THEN /* Aprovado ou Reaprovado */
      DO:
        IF doc-pend-aprov.cod-aprov-altern <> "" THEN
          ASSIGN c-cod-aprov = doc-pend-aprov.cod-aprov-altern.
        ELSE
          ASSIGN c-cod-aprov = doc-pend-aprov.cod-aprov.

        FIND FIRST b_usuar_mestre NO-LOCK
             WHERE b_usuar_mestre.cod_usuario = c-cod-aprov NO-ERROR.
        IF AVAIL b_usuar_mestre THEN
          ASSIGN c-aprovador = c-aprovador + STRING(b_usuar_mestre.nom_usuario) + " / ".
      END.
      ELSE
      DO:
        /*******************************************************/
        CASE doc-pend-aprov.ind-situacao:

          WHEN 1 THEN /* Pendente */
          DO:
            IF tt-param.i-idioma = 1 THEN 
              ASSIGN c-arq-modelo = ""
                     c-arq-modelo = "modelos\modeloESCC0305_PORT_Pendente.xls".
            ELSE 
              ASSIGN c-arq-modelo = ""
                     c-arq-modelo = "modelos\modeloESCC0305_ING_Pending.xls".

            IF doc-pend-aprov.cod-aprov-altern <> "" THEN          
              ASSIGN c-cod-aprov = doc-pend-aprov.cod-aprov-altern.
            ELSE                                                   
              ASSIGN c-cod-aprov = doc-pend-aprov.cod-aprov.       
           
            FIND FIRST b_usuar_mestre NO-LOCK                                          
                 WHERE b_usuar_mestre.cod_usuario = c-cod-aprov NO-ERROR. 
            IF AVAIL b_usuar_mestre THEN                                               
              ASSIGN c-pendente = c-pendente + STRING(b_usuar_mestre.nom_usuario) + " / ".
          END.
         
          WHEN 3 THEN /* Rejeitado */
          DO:                                                                  
            IF tt-param.i-idioma = 1 THEN                                      
              ASSIGN c-arq-modelo = ""
                     c-arq-modelo = "modelos\modeloESCC0305_PORT_Rejeitado.xls".
            ELSE                                                               
              ASSIGN c-arq-modelo = ""
                     c-arq-modelo = "modelos\modeloESCC0305_ING_Rejected.xls".   

            IF doc-pend-aprov.cod-aprov-altern <> "" THEN          
              ASSIGN c-cod-aprov = doc-pend-aprov.cod-aprov-altern.
            ELSE                                                   
              ASSIGN c-cod-aprov = doc-pend-aprov.cod-aprov.       
                               
            FIND FIRST b_usuar_mestre NO-LOCK                                                                   
                 WHERE b_usuar_mestre.cod_usuario = c-cod-aprov NO-ERROR.                          
            IF AVAIL b_usuar_mestre THEN                                                                        
              ASSIGN c-rejeitado = c-rejeitado + STRING(b_usuar_mestre.nom_usuario) + " / ".
          END.
        END CASE.
      END.
    END.
    /* FIM DOC-PEND-APROV ***************************************************************/

    
    ASSIGN FILE-INFO:FILE-NAME = c-arq-modelo
           c-planilha          = FILE-INFO:FULL-PATHNAME                                                                              
           i-linha             = 1.                                                                                                         
    /*************** Fim - Daniel Nascimento - 08/03/2010 ********************/

    
    /* ASSIGN
        FILE-INFO:FILE-NAME = IF tt-param.i-idioma = 1 THEN "modelos\modeloESCC0305_PORT.xls" ELSE "modelos\modeloESCC0305_ING.xls"
        c-planilha          = FILE-INFO:FULL-PATHNAME
        i-linha       = 1. */

    run pi-planilha-envio.


   
    /* Inicio da Criaá∆o da Planilha */
    create "Excel.Application" chExcelApplication.
    assign chExcelApplication:visible = NO /* Nao mostra a planilha Excel na tela enquanto esta sendo criada */
           /*chWorkbook = chExcelApplication:Workbooks:ADD(c-planilha).*/ /* Abre planilha excel ja existente */
           chWorkbook  = chExcelApplication:Workbooks:add(c-planilha) /* Cria uma nova planilha excel */
           chWorkSheet = chExcelApplication:Sheets:item(1).

    assign i-conta-linha = 5.

    assign 
        c-range = "H" + string(i-linha)
        chExcelApplication:Range(c-range):value = string(pedido-compr.num-pedido)

        i-linha = i-linha + 3
        c-range = "H" + string(i-linha)
        chWorkSheet:Range(c-range):font:size    = 8
        chWorkSheet:Range(c-range):font:bold    = true
        chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Data Pedido" ELSE "Order Date"
        c-range = "I" + string(i-linha)
        chWorkSheet:Range(c-range):font:size    = 8
        chExcelApplication:Range(c-range):value = string(data-pedido,"99/99/9999")

        i-linha = i-linha + 2
        i-conta-linha = i-conta-linha + 2
        c-range = "A" + string(i-linha) + ":I" + string(i-linha + 4).

    chExcelApplication:Range(c-range):select.
    chExcelApplication:Range(c-range):Borders:LineStyle     = 1.
    chExcelApplication:Range(c-range):Borders:Weight        = 4.
    chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
    chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

    find first estabelec no-lock
         where estabelec.cod-estabel = pedido-compr.end-entrega no-error.
    find first emitente no-lock
         where emitente.cod-emitente = estabelec.cod-emitente no-error.

    assign c-cep = string(estabelec.cep).
                                                                                 
    ASSIGN c-data-emissao = STRING(DAY(TODAY),"99") + "/" + STRING(MONTH(TODAY),"99") + "/" + STRING(YEAR(TODAY),"9999").

    assign c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Raz∆o Social" ELSE "Company Name"
           
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = estabelec.nome
           
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Emiss∆o" ELSE "Issue Date"
           
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):Numberformat = "@"
           chExcelApplication:Range(c-range):value = c-data-emissao
           
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1              
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Endereáo de Entrega" ELSE "Delivery Adress"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = estabelec.endereco
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Bairro" ELSE "Country"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  estabelec.bairro ELSE estabelec.pais
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Cidade/UF" ELSE "City/State"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = trim(estabelec.cidade) + " - " + estabelec.estado
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Cep" ELSE "Zip Code"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = string(c-cep,"99999999")
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = "CNPJ"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = string(estabelec.cgc,"99.999.999/9999-99")
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = "Insc. Estadual"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = "'" + estabelec.ins-estadual
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Telefone" ELSE "Phone Number"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = emitente.telefone[1]
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Fax" ELSE "Faz Number"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = emitente.telefax.

    assign i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           c-range = "A" + string(i-linha) + ":I" + string(i-linha + 1).
    chExcelApplication:Range(c-range):select.
    chExcelApplication:Range(c-range):Borders:LineStyle = 1.
    chExcelApplication:Range(c-range):Borders:Weight    = 4.
    chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
    chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

    find first estabelec no-lock
         where estabelec.cod-estabel = pedido-compr.end-cobranca no-error.

    assign c-cep = string(estabelec.cep).

    assign c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Endereáo de Cobranáa" ELSE "Payment Adress"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = estabelec.endereco
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Bairro" ELSE "Country"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN estabelec.bairro ELSE estabelec.pais
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Cidade/UF" ELSE "City/State"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = trim(estabelec.cidade) + " - " + estabelec.estado
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Cep" ELSE "Zip Code"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = string(c-cep,"99999999").

    find first emitente no-lock
         where emitente.cod-emitente = pedido-compr.cod-emitente no-error.

    find first cont-emit no-lock
         where cont-emit.cod-emitente = emitente.cod-emitente no-error.

    assign i-linha = i-linha + 2
           i-conta-linha = i-conta-linha + 2
           c-range = "A" + string(i-linha) + ":I" + string(i-linha + 2).
           chExcelApplication:Range(c-range):select.
           chExcelApplication:Range(c-range):Borders:LineStyle = 1.
           chExcelApplication:Range(c-range):Borders:Weight    = 3.
           chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
           chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

    assign c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size     = 8
           chWorkSheet:Range(c-range):font:bold     = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Fornecedor" ELSE "Supplier"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = /* string(emitente.cod-emitente) + " - " + */
                                                     emitente.nome-emit
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "CNPJ" ELSE "Country"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  string(emitente.cgc,"99.999.999/9999-99") ELSE emitente.pais
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Insc.Estadual" ELSE "Company Code"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "'" + emitente.ins-estadual ELSE string(emitente.cgc,"99.999.999/9999-99")
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Contato" ELSE "Contact"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = if  avail cont-emit then cont-emit.nome else ""
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "Telefone" ELSE "Phone Number"
           
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = emitente.telefone[1]
           
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Cod. Fornec." ELSE "Supplier Code"
           
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = emitente.cod-emit.

    assign i-linha = i-linha + 2
           i-conta-linha = i-conta-linha + 2
           c-range = "A" + string(i-linha) + ":I" + string(i-linha + 3).
           chExcelApplication:Range(c-range):select.
           chExcelApplication:Range(c-range):Borders:LineStyle = 1.
           chExcelApplication:Range(c-range):Borders:Weight    = 3.
           chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
           chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

    find first transporte no-lock
         where transporte.cod-transp = pedido-compr.cod-transp no-error.

/*     assign c-moeda = string(pedido-compr.i-moeda). */

    FIND FIRST ordem-compra use-index pedido
         where ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK NO-ERROR.
   
    /* pega a moeda do item */ 
    find first moeda no-lock
         where moeda.mo-codigo = ordem-compra.mo-codigo /*pedido-compr.i-moeda*/ no-error. 
   
    if  avail moeda then
        assign c-moeda = /* c-moeda + " - " + */
                         trim(moeda.sigla) + " - " +
                         trim(moeda.descricao).

    assign c-comprador = pedido-compr.responsavel.
    find first usuar_mestre no-lock
         where usuar_mestre.cod_usuario = pedido-compr.responsavel no-error.
    if  avail usuar_mestre then
        assign c-comprador = /* c-comprador + " - " + */
                             trim(usuar_mestre.nom_usuario).

    assign c-cond-pag = "".
    if  pedido-compr.cod-cond-pag = 0 then
        assign c-cond-pag = "CONDIÄ«O ESPECIAL".
    else do:
/*        assign c-cond-pag = string(pedido-compr.cod-cond-pag). */
        find first cond-pagto no-lock
             where cond-pagto.cod-cond-pag = pedido-compr.cod-cond-pag no-error.
        if  avail cond-pagto then
            assign c-cond-pag = /* c-cond-pag + " - " + */
                                trim(cond-pagto.descricao).
    end.

    ASSIGN c-frete = IF pedido-compr.frete = 1 THEN "CIF" else "FOB".

    FIND LAST cotacao-item NO-LOCK                                     
        WHERE cotacao-item.numero-ordem = ordem-compra.numero-ordem    
        AND   cotacao-item.it-codigo    = ordem-compra.it-codigo       
        AND   cotacao-item.cod-emitente = ordem-compra.cod-emitente NO-ERROR.
    IF AVAIL cotacao-item THEN
    DO:
      IF SUBSTRING(cotacao-item.char-1,21,3) <> "" THEN
        ASSIGN c-frete = SUBSTRING(cotacao-item.char-1,21,3).
    END.

    assign c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Moeda" ELSE "Currency"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = c-moeda
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Frete" ELSE "Incoterms"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = c-frete.

    ASSIGN i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Transportador" ELSE "Freight"
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = if  avail transporte then transporte.nome-abrev else ""
           c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Contato" ELSE "Contact"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = if  avail transporte then transporte.telefone else "" /* Cliente definir† se este campo devera ser atualizado */ .
           
    ASSIGN i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Comprador" ELSE "Buyer"
           
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = c-comprador.

        FIND FIRST usuar-mater NO-LOCK
             WHERE usuar-mater.cod-usuario = pedido-compr.responsavel NO-ERROR.
        
    ASSIGN c-range = "G" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = IF AVAIL usuar-mater THEN usuar-mater.e-mail ELSE ""
           
           i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1
           
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Cond Pagamento" ELSE "Payment Conditions"
           
           c-range = "C" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chExcelApplication:Range(c-range):value = c-cond-pag.

    assign vl-tot-pedido = 0.

    IF tt-param.l-narrativa-pedido THEN
    DO:
        ASSIGN
            i-linha     = i-linha + 2
            i-linha-aux = i-linha
            c-range       = "A" + string(i-linha-aux)
            chWorkSheet:Range(c-range):font:size    = 8
            chWorkSheet:Range(c-range):font:bold    = TRUE
            chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Narrativa do Pedido:" ELSE "Pruchase order description:".

        run pi-print-editor(pedido-compr.comentarios, 110).
        for each tt-editor no-lock:
            if  tt-editor.conteudo <> "" then
                assign i-linha       = i-linha + 1
                       i-conta-linha = i-conta-linha + 1
                       c-range       = "A" + string(i-linha)
                       chWorkSheet:Range(c-range):font:size    = 8
                       chExcelApplication:Range(c-range):value = tt-editor.conteudo.
        end.

        assign
            c-range = "A" + string(i-linha-aux)     + ":I" + string(i-linha).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign i-linha = i-linha + 1
               i-conta-linha = i-conta-linha + 1.

    END.

    if  tt-param.l-contrato then do:
        for each ordem-compra use-index pedido
           where ordem-compra.num-pedido    = pedido-compr.num-pedido
             and ordem-compra.numero-ordem >= tt-param.i-ini-numero-ordem
             and ordem-compra.numero-ordem <= tt-param.i-fim-numero-ordem:

            IF l-exibe THEN
               run pi-acompanhar in h-acomp(input "Pedido/Ordem : " +
                                            string(pedido-compr.num-pedido) + " / " +
                                            string(ordem-compra.numero-ordem)).

            find contrato-for where contrato-for.nr-contrato = ordem-compra.nr-contrato
                     exclusive-lock no-error.
            
            /*if  avail contrato-for then do:
                    assign c-desc-contrato              = contrato-for.des-contrat.       
            
                    if  tt-param.i-param-c = 1
                    and ordem-compra.sit-ordem-contrat <> 2 then next.
                    else
                        if  tt-param.i-param-c = 2
                        and ordem-compra.sit-ordem-contrat <> 1 then next.
                        else      
                            if  tt-param.i-param-c = 3
                            and ordem-compra.sit-ordem-contrat = 3 then next.           
            */
              if  avail contrato-for then do:
                  assign contrato-for.ind-sit-contrat = 2.      
              end.

            run pi-imprime-ordens.

            if  avail contrato-for
            and ordem-compra.nr-contrato = contrato-for.nr-contrato
            and ordem-compra.sit-ordem-contrat = 1 then 
                assign ordem-compra.sit-ordem-contrat = 2.

            /* cida - 03/10 
            assign ordem-compra.ordem-emitida = yes. */

        end.
    end.
    else do:
        for each ordem-compra NO-LOCK use-index pedido
           where ordem-compra.num-pedido = pedido-compr.num-pedido:

            IF l-exibe THEN
               run pi-acompanhar in h-acomp(input "Pedido/Ordem : " +
                                            string(pedido-compr.num-pedido) + " / " +
                                            string(ordem-compra.numero-ordem)).


            run pi-imprime-ordens.
             
            /* cida - 03/10 
            assign ordem-compra.ordem-emitida = yes. */

        end.
    end.

    assign i-linha       = i-linha + 2
           i-conta-linha = i-conta-linha + 2
           i-linha-aux   = i-linha.

    assign /*i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1*/
           c-range = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:size    = 8
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "ê OBRIGAT‡RIO CONSTAR NÈMERO DESTE PEDIDO NA NOTA FISCAL" ELSE "SHOULD INSERT THE PURCHASE ORDER NUMBER ON THE INVOICE"
           c-range = "A" + string(i-linha) + ":I" + string(i-linha)
           chExcelApplication:Range(c-range):Interior:ColorIndex = 15.

    chExcelApplication:Range(c-range):merge.
    chExcelApplication:Range(c-range):HorizontalAlignment = 3.

    if  pedido-compr.cod-cond-pag = 0 then do:
        find first cond-especif no-lock
             where cond-especif.num-pedido = pedido-compr.num-pedido no-error.
        if  avail cond-especif then do:
            assign i-linha = i-linha + 1
                   i-conta-linha = i-conta-linha + 1
                   c-range = "A" + string(i-linha)
                   chWorkSheet:Range(c-range):font:size    = 8
                   chWorkSheet:Range(c-range):font:bold    = true
                   chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "CONDIÄ«O ESPECIAL" ELSE "SPECIAL CONDITION".

            do  i-conta = 1 to 12:
                if  cond-especif.data-pagto[i-conta]  <> ?  or
                    cond-especif.perc-pagto[i-conta]  <> 0  or
                    cond-especif.comentarios[i-conta] <> "" then do:
                    assign c-linha = string(cond-especif.data-pagto[i-conta]) + " " +
/*                                      string(cond-especif.perc-pagto[i-conta]) + " " + */
                                     trim(cond-especif.comentarios[i-conta]).

                    assign i-int-aux1 = length(c-linha)
                           i-int-aux2 = 0.

                    do while(i-int-aux1 > 0):
                        assign i-int-aux2 = i-int-aux2 + 1
                               i-int-aux1 = i-int-aux1 - 95.
                    end.
                    if  index(c-linha,chr(10)) <> 0 then
                        assign i-int-aux2 = i-int-aux2 + index(c-linha,chr(10)).

                    if  i-int-aux2 = 0 then
                        assign i-int-aux2 = 1.

                    assign i-linha       = i-linha + 1
                           i-conta-linha = i-conta-linha + 1
                           c-range       = "A" + string(i-linha) + ":I" + string(i-linha).
                           chExcelApplication:Range(c-range):merge.
                           chExcelApplication:Range(c-range):HorizontalAlignment = 1.
                           chExcelApplication:Range(c-range):WrapText = true.
                           chExcelApplication:Range(c-range):RowHeight = 12.75 * i-int-aux2.
                           chWorkSheet:Range(c-range):value = c-linha.
                end.
            end.
        end.
    end.

    assign c-range = "A" + string(i-linha-aux) + ":I" + string(i-linha).
    chExcelApplication:Range(c-range):select.
    chExcelApplication:Range(c-range):Borders:LineStyle     = 1.
    chExcelApplication:Range(c-range):Borders:Weight        = 4.
    chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
    chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.
    chExcelApplication:Range(c-range):font:size             = 8.
    chExcelApplication:Range(c-range):font:bold             = true.

    assign i-linha       = i-linha + 2
           i-conta-linha = i-conta-linha + 2
           c-range       = "A" + string(i-linha)
           chWorkSheet:Range(c-range):font:bold    = true
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Total Geral :" ELSE "Total:"
           c-range = "H" + string(i-linha)
           chWorkSheet:Range(c-range):font:bold    = true   
           /* chWorkSheet:Range(c-range):Numberformat = " #.##0,00;[Red] #.##0,00" */
           chWorkSheet:Range(c-range):Numberformat = "#.##0,00"
           chExcelApplication:Range(c-range):value = vl-tot-pedido.

    assign c-range = "H" + string(i-linha) + ":I" + string(i-linha).
    chExcelApplication:Range(c-range):merge.
    
    assign c-range = "A" + string(i-linha) + ":I" + string(i-linha). 
    chExcelApplication:Range(c-range):Borders:LineStyle     = 1.     
    chExcelApplication:Range(c-range):Borders:Weight        = 4.     
    chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142. 
    chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.  

    /*************** Daniel Nascimento - 08/03/2010 ********************/                                       
    assign i-linha       = i-linha + 2                                                                          
           i-conta-linha = i-conta-linha + 2
           c-range       = "A" + string(i-linha)                                                                
           chWorkSheet:Range(c-range):font:bold    = true                                                       
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Aprovado por:" ELSE "Approved By:"
           c-range = "C" + string(i-linha)                                                                      
           chWorkSheet:Range(c-range):font:bold    = FALSE                                                       
           chWorkSheet:Range(c-range):Numberformat = ""                                                         
           chExcelApplication:Range(c-range):value = c-aprovador.

    assign c-range = "C" + string(i-linha) + ":I" + string(i-linha).
           chExcelApplication:Range(c-range):merge.                        
                                                                    
    assign c-range = "A" + string(i-linha) + ":I" + string(i-linha).
           chExcelApplication:Range(c-range):Borders:LineStyle     = 1.    
           chExcelApplication:Range(c-range):Borders:Weight        = 3.    
           chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
           chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.


    ASSIGN i-linha       = i-linha + 1                                                                            
           i-conta-linha = i-conta-linha + 1
           c-range       = "A" + string(i-linha)                                                                  
           chWorkSheet:Range(c-range):font:bold    = TRUE                                                         
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Pendente por:" ELSE "Pending By:"
           c-range = "C" + string(i-linha)                                                                        
           chWorkSheet:Range(c-range):font:bold    = FALSE                                                         
           chWorkSheet:Range(c-range):Numberformat = ""                                                           
           chExcelApplication:Range(c-range):value = c-pendente.

    assign c-range = "C" + string(i-linha) + ":I" + string(i-linha).       
           chExcelApplication:Range(c-range):merge.                        
                                                                           
    assign c-range = "A" + string(i-linha) + ":I" + string(i-linha).       
           chExcelApplication:Range(c-range):Borders:LineStyle     = 1.    
           chExcelApplication:Range(c-range):Borders:Weight        = 3.    
           chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
           chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.
    
    ASSIGN i-linha       = i-linha + 1                                                                            
           i-conta-linha = i-conta-linha + 1
           c-range       = "A" + string(i-linha)                                                                  
           chWorkSheet:Range(c-range):font:bold    = true                                                         
           chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Rejeitado por:" ELSE "Rejected By:"
           c-range = "C" + string(i-linha)                                                                        
           chWorkSheet:Range(c-range):font:bold    = FALSE                                                         
           chWorkSheet:Range(c-range):Numberformat = ""                                                           
           chExcelApplication:Range(c-range):value = c-rejeitado.                                       

    assign c-range = "C" + string(i-linha) + ":I" + string(i-linha).                                            
           chExcelApplication:Range(c-range):merge.                                                                    
                                                                                                                
    assign c-range = "A" + string(i-linha) + ":I" + string(i-linha).                                            
           chExcelApplication:Range(c-range):Borders:LineStyle     = 1.                                                
           chExcelApplication:Range(c-range):Borders:Weight        = 3.                                                
           chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.                                            
           chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.                                            
                                                                                                                
    assign i-linha       = i-linha + 1                                                                          
           i-conta-linha = i-conta-linha + 1.                                                                   
    
    find first mensagem no-lock
         where mensagem.cod-mensage = pedido-compr.cod-mensagem no-error.
    if  avail mensagem then do:
        run pi-print-editor(texto-mensag, 115).
        for each tt-editor no-lock:
            assign i-linha       = i-linha + 1
                   i-conta-linha = i-conta-linha + 1
                   c-range       = "A" + string(i-linha)
                   chWorkSheet:Range(c-range):font:size    = 6
                   chExcelApplication:Range(c-range):value = tt-editor.conteudo.
        end.
    end.

    /*] A. Souza DSC */
     assign i-linha       = i-linha + 1      
            i-conta-linha = i-conta-linha + 1.

    FOR EACH tt-beneficio-valid:


       
         FIND es-ben-estab WHERE ROWID(es-ben-estab) = tt-beneficio-valid.r-ben-row NO-LOCK  NO-ERROR.

         
         IF  es-ben-estab.compras-fornecedor-icms <> 0 AND tt-beneficio-valid.tp-tax = "ICMS" THEN DO:                                    
             CREATE tt-msg.                                                                        
             ASSIGN tt-msg.cod-msg   = es-ben-estab.compras-fornecedor-icms
                    tt-msg.it-codigo = tt-beneficio-valid.it-codigo
                    tt-msg.tp-msg   = "ICMS".
         END.                                                                                      
         IF  es-ben-estab.compras-fornecedor-pis-cofins <> 0 AND tt-beneficio-valid.tp-tax = "PIS"  THEN DO:                              
             CREATE tt-msg.                                                                        
             ASSIGN tt-msg.cod-msg   = es-ben-estab.compras-fornecedor-pis-cofins
                    tt-msg.it-codigo = tt-beneficio-valid.it-codigo
                    tt-msg.tp-msg    = "PIS-COFINS".
         END.                                                                                      
    END.
     FOR EACH tt-msg BREAK BY tt-msg.cod-msg.
/*          IF FIRST-OF(tt-msg.cod-msg) THEN DO: */
             FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = tt-msg.cod-msg NO-LOCK NO-ERROR.
             IF AVAIL es-mensagem-ben THEN DO:



                 run pi-print-editor("INSTRUÄÂES TRIBUTARIAS" + "( " + TT-MSG.TP-MSG   + ")" + "  PARA O ITEM => " + tt-msg.it-codigo + CHR(10) +  string(es-mensagem-ben.cod-mensagem,"99") + " - " + es-mensagem-ben.desc-mensagem + " - " + es-mensagem-ben.narrativa , 115).

                 FOR EACH tt-editor:
                  assign i-linha       = i-linha + 1                                     
                         i-conta-linha = i-conta-linha + 1                               
                         c-range       = "A" + string(i-linha)                           
                         chWorkSheet:Range(c-range):font:size    = 6                     
                         chExcelApplication:Range(c-range):value = CAPS(tt-editor.conteudo).
                 END.
/*              END. */
         END.
         i-linha       = i-linha + 1.          
         i-conta-linha = i-conta-linha + 1.    


     END.
    assign i-linha       = i-linha + 2
           i-conta-linha = i-conta-linha + 1
           c-range       = "A" + string(i-linha).
           chExcelApplication:Range(c-range):select.

    chExcelApplication:ActiveSheet:protect("873255",true,true).

    /*chExcelApplication:workbooks:item(1):SaveAs(c-nome,,,,,,).*/

    chExcelApplication:DisplayAlerts = FALSE.

/*     os-command silent del value(c-planilha-envio). */


    chExcelApplication:workbooks:item(1):SaveAs(c-planilha-envio,,,,,,).

    ASSIGN c-arquivo = c-planilha-envio.

    chExcelApplication:range("A1"):select.

    chExcelApplication:ActiveSheet:Enableselection ='1'.
    chExcelApplication:DisplayAlerts = false.

    find first b-pedido-compr exclusive-lock
         where b-pedido-compr.num-pedido = pedido-compr.num-pedido no-error.
    if  avail b-pedido-compr then
        assign b-pedido-compr.situacao = 1.

    put "Planilhas Geradas:"
        c-planilha-envio format "x(60)"
        skip.

    chExcelApplication:range("A1"):select.

    if  tt-param.destino = 3 AND NOT tt-param.l-envia-email THEN

       assign chExcelApplication:visible = l-exibe. /* Mostra a planilha Excel na tela */
    else
        chExcelApplication:quit.

    release object chWorkSheet.
    release object chWorkbook.
    release object chExcelApplication.

    if  tt-param.l-envia-email then do:
        {include/i-rpclo.i}
        IF l-exibe THEN
           run pi-finalizar in h-acomp.

        assign c-dest-e-mail = "".
        for first cont-emit no-lock
            where cont-emit.cod-emitente = pedido-compr.cod-emitente:

            IF LOOKUP(cont-emit.e-mail,c-dest-e-mail,";") = 0 THEN
                ASSIGN
                    c-dest-e-mail = cont-emit.e-mail + ";".
        END.

        find first usuar-mater no-lock
             where usuar-mater.cod-usuario = c-seg-usuario no-error.
        if  avail usuar-mater then
            assign c-remetente = usuar-mater.e-mail.

        run esp/escc0305a.w.

        for each tt-envio:
            delete tt-envio.
        end.

        create tt-envio.

        IF tt-param.i-idioma = 1 THEN

        assign tt-envio.versao-integracao = 1
/*                tt-envio.servidor          = "172.26.52.10" /*"172.24.52.29"*/ */
/*                tt-envio.porta             = 25                                */
               tt-envio.destino           = c-dest-e-mail 
               tt-envio.remetente         = c-remetente 
               tt-envio.copia             = c-remetente
               tt-envio.assunto           = "Pedido de Compras Nro. " + STRING(pedido-compr.num-pedido)
               tt-envio.arq-anexo         = c-planilha-envio
               tt-envio.exchange          = no
               tt-envio.mensagem          = 
            "PEDIDO DE COMPRAS"               + CHR(10) +
            CHR(10) + CHR(10) +
            "Prezado(s) Senhor(es),"          + CHR(10) +
            CHR(10) +
            "A µrea de Compras e Contratos informa que sua proposta de preáos foi a ganhadora conforme pedido(s) anexo(s)." + CHR(10) +
            CHR(10) +
            "Solicitamos sua especial atená∆o quanto ao atendimento no prazo de entrega estipulado." + CHR(10) +
            CHR(10) +
            "OBS: ê OBRIGATORIO O NUMERO DO PEDIDO NA NOTA FISCAL." + CHR(10) +
            CHR(10) +
            "Favor acusar o recebimento deste pedido." + CHR(10) +
            CHR(10) +
            "Atenciosamente,"      + CHR(10) +
            CHR(10) +
            usuar-mater.nome                            + CHR(10) +
            "E-mail: "  + usuar-mater.e-mail            + CHR(10) +
            "Tel.: "    + usuar-mater.telefone[1]       + IF usuar-mater.ramal[1] <> "" THEN "Ramal: "  + usuar-mater.ramal[1] ELSE "" + CHR(10) +
            "Fax: "     + usuar-mater.telefax           + CHR(10).
        ELSE

            assign tt-envio.versao-integracao = 1
    /*                tt-envio.servidor          = "172.26.52.10" /*"172.24.52.29"*/ */
    /*                tt-envio.porta             = 25                                */
                   tt-envio.destino           = c-dest-e-mail 
                   tt-envio.remetente         = c-remetente 
                   tt-envio.copia             = c-remetente
                   tt-envio.assunto           = "Purchase Order: " + STRING(pedido-compr.num-pedido)
                   tt-envio.arq-anexo         = c-planilha-envio
                   tt-envio.exchange          = no
                   tt-envio.mensagem          = 
                "PURCHASE ORDER"                + CHR(10) +
                CHR(10) + CHR(10) +
                "Dears,"                   + CHR(10) +
                CHR(10) +
                "The Supply Chains Department inform that your prices proposal was the winner as the puchase order attached."   + CHR(10) +
                CHR(10) +
                "We ask for special attention accord to the delivery date." + CHR(10) +
                CHR(10) +
                "PS: SHOULD INSERT THE PURCHASE ORDER NUMBER ON THE INVOICE."                                + CHR(10) +
                CHR(10) +
                "Please warn us about the delivery of this order."  + CHR(10) +
                CHR(10) +
                "Best Regards,"                           + CHR(10) +
                CHR(10) +
                usuar-mater.nome                            + CHR(10) +
                "E-mail: "  + usuar-mater.e-mail            + CHR(10) +
                "Tel.: "    + usuar-mater.telefone[1]       + IF usuar-mater.ramal[1] <> "" THEN "Ramal: "  + usuar-mater.ramal[1] ELSE "" + CHR(10) +
                "Fax: "     + usuar-mater.telefax           + CHR(10).



        run utp/utapi009.p(input  table tt-envio,
                           output table tt-erros).

        if  return-value = "NOK" then do:
            for each tt-erros:
                run utp/ut-msgs.p(input "show",
                                  input 17006,
                                  input "Erro no envio de E-mail." + "~~" +
                                        "E-mail n∆o enviado - " + tt-erros.desc-erro).
            end.
        end.
        else
            run utp/ut-msgs.p(input "show",
                              input 17006,
                              input "E-mail enviado." + "~~"  +
                                    "Email enviado com Sucesso !!!").

        if  tt-param.destino = 3
        AND tt-param.l-envia-email then
        DO:
            create "Excel.Application" chExcelApplication.
            assign
                chExcelApplication:visible  = yes /* Mostra a planilha Excel na tela */
                chWorkbook                  = chExcelApplication:Workbooks:OPEN(c-planilha-envio). /* Cria uma nova planilha excel */
            release object chExcelApplication.
        END.

        IF l-exibe THEN DO:
           run utp/ut-acomp.p persistent set h-acomp.
           run pi-inicializar in h-acomp(input string(c-titulo-relat)).
        END.
    end.
end.

{include/i-rpclo.i}
IF l-exibe THEN
   run pi-finalizar in h-acomp.

return 'ok'.

procedure pi-planilha-envio:
    find first usuar_mestre no-lock
         where usuar_mestre.cod_usuario = c-seg-usuario no-error.

    if  avail usuar_mestre then do:
        assign c-planilha-envio = usuar_mestre.nom_dir_spool + "/" + 
                                  (if  usuar_mestre.nom_subdir_spool <> '' then
                                       usuar_mestre.nom_subdir_spool + '\'
                                  else '') + 'Pedido_' +
                                  trim(string(pedido-compr.num-pedido)) + '.xlsx'.
    end.
    else
        assign c-planilha-envio = session:TEMP-DIRECTORY + "\Pedido_" +
                                  trim(string(pedido-compr.num-pedido)) + ".xlsx".


    
    IF l-exibe = YES THEN
       ASSIGN c-planilha-envio = session:TEMP-DIRECTORY  + "PEDIDO_" + trim(string(pedido-compr.num-pedido)) + ".xlsx".
    ELSE
       ASSIGN c-planilha-envio = session:TEMP-DIRECTORY  + "PEDIDO_" + trim(string(pedido-compr.num-pedido)) + "_" +  REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".xlsx".

   /*ASSIGN
          c-planilha-envio = replace(c-planilha-envio,'\','/'). */

    if  search(c-planilha-envio) <> "" and
        search(c-planilha-envio) <> ? then
        dos silent del value(replace(c-planilha-envio,'/','\')).

    
end.

procedure pi-imprime-ordens:
    find first item no-lock
         where item.it-codigo = ordem-compra.it-codigo no-error.

    find last cotacao-item no-lock
        where cotacao-item.it-codigo    = ordem-compra.it-codigo
          and cotacao-item.numero-ordem = ordem-compra.numero-ordem
          and cotacao-item.cod-emitente = ordem-compra.cod-emitente no-error.

    for each prazo-compra no-lock
       where prazo-compra.numero-ordem = ordem-compra.numero-ordem:
       
        /* ordem recebida n∆o imprime - Ronaldo - 28/08 
           Ordem eliminada n∆o imprime - 02/10 */

        IF prazo-compra.situacao = 6 OR
           prazo-compra.situacao = 4 THEN
           NEXT. 

        
        assign i-linha          = i-linha + 2
               i-conta-linha    = i-conta-linha + 2
               i-linha-aux      = i-linha
               
               c-range = "A" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "O.C./PARCELA" ELSE "O.C./ITEM"
               c-range = "C" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chExcelApplication:Range(c-range):value = string(ordem-compra.numero-ordem) + "/" +
                                                         string(prazo-compra.parcela)
               c-range = "D" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "COD.ITEM" ELSE "ITEM CODE"
               c-range = "E" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chExcelApplication:Range(c-range):value = "'" + ordem-compra.it-codigo.

        if  ordem-compra.nr-contrato <> 0 then
            assign c-range = "G" + string(i-linha)
                   chWorkSheet:Range(c-range):font:size    = 6
                   chWorkSheet:Range(c-range):font:bold    = true
                   chExcelApplication:Range(c-range):value = "CONTRATO"
                   c-range = "H" + string(i-linha)
                   chWorkSheet:Range(c-range):font:size    = 7
                   chExcelApplication:Range(c-range):value = ordem-compra.nr-contrato.
        
        /* conta contabil */                                                          

        ASSIGN c-ct-conta = ordem-compra.ct-codigo
               c-sc-conta = ordem-compra.sc-codigo.

       
        assign i-linha = i-linha + 1
               i-conta-linha = i-conta-linha + 1
               
               c-range = "A" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = "CONTA CONTABIL"
               c-range = "C" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chExcelApplication:Range(c-range):value = string(c-ct-conta,"99999999") + "-" +
                                                         STRING(c-sc-conta,"99999999").
        
        ASSIGN c-range = "G" + string(i-linha)                                                                              
               chWorkSheet:Range(c-range):font:size    = 8                                                                  
               chWorkSheet:Range(c-range):font:bold    = true                                                               
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "Ordem Invest" ELSE "Order to Invest"
                                                                                                                            
               c-range = "H" + string(i-linha)                                                                              
               chWorkSheet:Range(c-range):font:size    = 8                                                                  
               chWorkSheet:Range(c-range):Numberformat = "###.###.###"                                                      
               chExcelApplication:Range(c-range):value = ordem-compra.num-ord-inv.

               /*c-range = "d" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chExcelApplication:Range(c-range):value = c-sc-conta.
               */

            
            /* string(ordem-compra.ct-codigo) + "." + string(ordem-compra.sc-codigo).*/


        if  tt-param.l-descricao
        or  tt-param.l-narrativa-item-ord-compr
        or  tt-param.l-narrativa-ord-compr      then
        do:
            assign i-linha = i-linha + 1
                   i-conta-linha = i-conta-linha + 1.
                   /*c-range = "D" + string(i-linha)
                   chExcelApplication:Range(c-range):value = "DESCRIÄ«O E NARRATIVA DO ITEM".*/

            if  tt-param.l-descricao then
                assign i-linha       = i-linha + 1
                       i-conta-linha = i-conta-linha + 1
                       c-range       = "B" + string(i-linha)
                       chWorkSheet:Range(c-range):font:size    = 8
                       chExcelApplication:Range(c-range):value = item.desc-item.

            if  tt-param.l-narrativa-item-ord-compr and
                avail item then do:
                assign i-linha       = i-linha + 1
                       i-conta-linha = i-conta-linha + 1.
                
                run pi-print-editor(item.narrativa, 110).
                for each tt-editor no-lock:
                    if  tt-editor.conteudo <> "" then
                        assign i-linha       = i-linha + 1
                               i-conta-linha = i-conta-linha + 1
                               c-range       = "B" + string(i-linha)
                               chWorkSheet:Range(c-range):font:size    = 8
                               chExcelApplication:Range(c-range):value = tt-editor.conteudo.
                end.
            end.

            if  tt-param.l-narrativa-ord-compr then do:
                assign i-linha       = i-linha + 1
                       i-conta-linha = i-conta-linha + 1.

                run pi-print-editor(ordem-compra.narrativa, 110).
                for each tt-editor no-lock:
                    if  tt-editor.conteudo <> "" then
                        assign i-linha       = i-linha + 1
                               i-conta-linha = i-conta-linha + 1
                               c-range       = "B" + string(i-linha)
                               chWorkSheet:Range(c-range):font:size    = 8
                               chExcelApplication:Range(c-range):value = tt-editor.conteudo.
                end.
            end.
        end.
        /**/

      
        assign c-range = "A" + string(i-linha-aux) + ":I" + string(i-linha + 1).
               chExcelApplication:Range(c-range):select.
               chExcelApplication:Range(c-range):Borders:LineStyle = 1.
               chExcelApplication:Range(c-range):Borders:Weight    = 3.
               chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
               chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign i-linha          = i-linha + 2
               i-conta-linha    = i-conta-linha + 2
               i-linha-aux      = i-linha
               c-range = "A" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "DT ENTREGA" ELSE "DELIVERY DATE"
               c-range = "B" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7
               chExcelApplication:Range(c-range):value = (prazo-compra.data-entrega)
               c-range = "D" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "PRECO UNIT." ELSE "UNIT PRICE"
               c-range = "E" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7
               /* chWorkSheet:Range(c-range):Numberformat = " #.##0,00;[Red] #.##0,00" */
               chWorkSheet:Range(c-range):Numberformat = "#.##0,00" 
               /*chExcelApplication:Range(c-range):value = ordem-compra.preco-unit */
               chExcelApplication:Range(c-range):value = /*** ordem-compra.preco-unit - 
                                                        ((ordem-compra.preco-unit * ordem-compra.aliquota-ipi) / 100) ****/
                                                        /*** a conta dever† ser assim --> pr-unit / (1 + (aliq / 100))****/
                                                        cotacao-item.pre-unit-for /*ordem-compra.preco-fornec ordem-compra.preco-unit sergio */ / ( 1 + (ordem-compra.aliquota-ipi / 100))
                                                       
               c-range = "G" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "% DESCONTO" ELSE "% DISCOUNT"
               c-range = "H" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7
               /* chWorkSheet:Range(c-range):Numberformat = " #.##0,00;[Red] #.##0,00" */
               chWorkSheet:Range(c-range):Numberformat = "#.##0,00" 
               chExcelApplication:Range(c-range):value = (ordem-compra.perc-descto)
               i-linha = i-linha + 1
               i-conta-linha = i-conta-linha + 1
               c-range = "A" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = "QTDE"
               c-range = "B" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7.

        ASSIGN deSaldoQtde = prazo-compra.qtd-sal-forn 
/*                chExcelApplication:Range(c-range):value = /*prazo-compra.quantidade*/ prazo-compra.qtd-sal-forn - ordem-compra.qt-acum-rec */
               chExcelApplication:Range(c-range):value = deSaldoQtde
               chExcelApplication:Range(c-range):Numberformat = "###.###.##0,0000"
               c-range = "D" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "IPI(%)" ELSE "TAX(%)"
               c-range = "E" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7
               chExcelApplication:Range(c-range):value = string(ordem-compra.aliquota-ipi)
               c-range = "G" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "VLR DESCONTO" ELSE "DISCOUNT VALUE"
               c-range = "H" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7

               /*chWorkSheet:Range(c-range):Numberformat = " #.##0,00;[Red] #.##0,00" */
               chWorkSheet:Range(c-range):Numberformat = "#.##0,00" 
               chExcelApplication:Range(c-range):value = ordem-compra.valor-descto
               i-linha = i-linha + 1
               i-conta-linha = i-conta-linha + 1
               
               /* unidade do item */
               c-range = "A" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN "UNIDADE" ELSE "UNIT"
               c-range = "B" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7
               chExcelApplication:Range(c-range):value = cotacao-item.UN /*item.un*/
               
               
               c-range = "F" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 6
               chWorkSheet:Range(c-range):font:bold    = true
               chExcelApplication:Range(c-range):value = IF tt-param.i-idioma = 1 THEN  "VLR TOTAL ITEM C/ IMPOSTOS" ELSE "TOTAL VALUE"
               c-range = "H" + string(i-linha)
               chWorkSheet:Range(c-range):font:size    = 7
               /* chWorkSheet:Range(c-range):Numberformat = " #.##0,00;[Red] #.##0,00" */
               chWorkSheet:Range(c-range):Numberformat = "#.##0,00"
               chExcelApplication:Range(c-range):value = (/*prazo-compra.quantidade*/ prazo-compra.qtd-sal-forn - ordem-compra.qt-acum-rec) * cotacao-item.pre-unit-for.

        /**/
        assign
            c-range = "A" + string(i-linha-aux) + ":C" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign
            c-range = "D" + string(i-linha-aux) + ":F" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign
            c-range = "G" + string(i-linha-aux)     + ":I" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.


        assign
            i-linha-aux = i-linha-aux + 1.

        assign
            c-range = "A" + string(i-linha-aux) + ":C" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign
            c-range = "D" + string(i-linha-aux) + ":F" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign
            c-range = "G" + string(i-linha-aux)     + ":I" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign
            i-linha-aux = i-linha-aux + 1.

        assign
            c-range = "A" + string(i-linha-aux) + ":C" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign
            c-range = "D" + string(i-linha-aux) + ":F" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.
    
        assign
            c-range = "F" + string(i-linha-aux)     + ":I" + string(i-linha-aux).
        chExcelApplication:Range(c-range):select.
        chExcelApplication:Range(c-range):Borders:LineStyle = 1.
        chExcelApplication:Range(c-range):Borders:Weight    = 3.
        chExcelApplication:Range(c-range):Borders(11):LineStyle = -4142.
        chExcelApplication:Range(c-range):Borders(12):LineStyle = -4142.

        assign vl-tot-pedido = vl-tot-pedido + (/*prazo-compra.quantidade*/ prazo-compra.qtd-sal-forn * cotacao-item.pre-unit-for).
    end.

    assign i-linha = i-linha + 1
           i-conta-linha = i-conta-linha + 1.

    /*Carlos A. Souza DSC - Verifica os beneficios do item*/

    FIND ext-item-cfa WHERE ext-item-cfa.it-codigo   = ordem-compra.it-codigo
                      AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario
                      NO-LOCK NO-ERROR.
    IF AVAIL ext-item-cfa THEN DO:


        FOR EACH  es-beneficio-cfa  WHERE es-beneficio-cfa.classe = ext-item-cfa.classe NO-LOCK:
            CREATE tt-beneficio.
            ASSIGN tt-beneficio.cod-beneficio = es-beneficio-cfa.cod-beneficio
                   tt-beneficio.it-codigo     = ordem-compra.it-codigo.

        END.

        FOR EACH  es-beneficio-ncm WHERE  es-beneficio-ncm.class-fisc-ini >= ITEM.class-fisc                                  
                                   AND    es-beneficio-ncm.class-fisc-fim <= ITEM.class-fisc NO-LOCK:                         
                                                                                                                              
             CREATE tt-beneficio.                                                                                             
              ASSIGN tt-beneficio.cod-beneficio = es-beneficio-ncm.cod-beneficio
                     tt-beneficio.it-codigo     = ordem-compra.it-codigo.
        END.



        FOR EACH estabelecimento
           WHERE estabelecimento.cod_empresa =  i-ep-codigo-usuario1 :
            FOR EACH es-cfa-emp 
               WHERE es-cfa-emp.classe      = ext-item-cfa.classe 
                 AND es-cfa-emp.cod-estabel = estabelecimento.cod_estab NO-LOCK:
                FOR EACH tt-beneficio:
                    FIND FIRST tt-beneficio2
                         WHERE tt-beneficio2.cod-beneficio = tt-beneficio.cod-beneficio
                           AND tt-beneficio2.it-codigo     = tt-beneficio.it-codigo    NO-ERROR.
                    IF NOT AVAIL(tt-beneficio2) THEN DO:
                        CREATE tt-beneficio2.
                        ASSIGN tt-beneficio2.cod-beneficio = tt-beneficio.cod-beneficio
                               tt-beneficio2.it-codigo     = tt-beneficio.it-codigo
                               tt-beneficio2.cod-estabel   = es-cfa-emp.cod-estabel.
                    END.
                    
                END.
            END.
        END.
    END.
        
        FOR EACH tt-beneficio2: 


            FIND ext-item-cfa WHERE ext-item-cfa.it-codigo   = tt-beneficio2.it-codigo
                              AND   ext-item-cfa.ep-codigo   = i-ep-codigo-usuario
                              NO-LOCK NO-ERROR.

            FIND estabelec WHERE estabelec.cod-estabel = tt-beneficio2.cod-estabel NO-LOCK NO-ERROR.
            IF AVAIL estabelec THEN DO:
                c-uf-estab = estabelec.estado.
            END.

            FOR EACH es-ben-estab WHERE es-ben-estab.cod-beneficio = tt-beneficio2.cod-beneficio  
                                    AND es-ben-estab.dt-ini-vald   <= TODAY                                                         
                                    AND es-ben-estab.dt-fim-vald   >= TODAY                                                         
                                    AND es-ben-estab.cod-estabel    = tt-beneficio2.cod-estabel 
                                    AND (es-ben-estab.icms-ben-icms-est   = YES OR
                            es-ben-estab.icms-ben-icms-inter = YES OR
                            es-ben-estab.icms-ben-icms-imp   = YES)
                      
                NO-LOCK  BY   es-ben-estab.icms-prior : 


                IF c-uf-estab = c-uf-emitente THEN DO:
                    IF es-ben-estab.icms-ben-icms-inter = YES THEN.
                    ELSE DO:
                       IF es-ben-estab.icms-ben-icms-imp   = YES THEN DO:

                           IF emitente.pais <> "brasil" THEN.
                           ELSE NEXT.

                       END.
                       ELSE NEXT.
                    END.

                    IF NOT es-ben-estab.icms-ben-icms-est THEN NEXT.

                END.
                ELSE DO:

                    IF es-ben-estab.icms-ben-icms-imp THEN DO:
                        IF emitente.pais = "brasil" THEN NEXT.
                    END.
                    ELSE DO:
                        IF es-ben-estab.icms-ben-icms-inter THEN DO:
                            IF emitente.pais <> "brasil" THEN NEXT.
                        END.
                    END.
                END.

                FIND tt-beneficio-valid WHERE    tt-beneficio-valid.cod-estabel    = es-ben-estab.cod-estabel 
                                        AND      tt-beneficio-valid.it-codigo      = ext-item-cfa.it-codigo
                                        AND      tt-beneficio-valid.classe         = ext-item-cfa.classe      
                                        AND      tt-beneficio-valid.tp-tax         = "ICMS" NO-LOCK NO-ERROR.  


                IF NOT AVAIL tt-beneficio-valid THEN DO:
                    CREATE tt-beneficio-valid.                                                                                            
                    ASSIGN tt-beneficio-valid.cod-beneficio  = es-ben-estab.cod-beneficio.                                                 
                           tt-beneficio-valid.cod-estabel    = es-ben-estab.cod-estabel.                                                   
                           tt-beneficio-valid.it-codigo      = tt-beneficio2.it-codigo.                                                     
                           tt-beneficio-valid.classe         = ext-item-cfa.classe.
                           tt-beneficio-valid.tp-tax         = "ICMS".
                           tt-beneficio-valid.pri-icms       = es-ben-estab.icms-prior.
                           tt-beneficio-valid.r-ben-row      = ROWID(es-ben-estab).                                                   
                                                                                                                                      
                END.
                ELSE DO:
                    IF es-ben-estab.pis-cofins-prioridade <  tt-beneficio-valid.pri-icms THEN DO:     
                     tt-beneficio-valid.pri-icms   = es-ben-estab.pis-cofins-prioridade.           
                     tt-beneficio-valid.r-ben-row  = ROWID(es-ben-estab).
                    END.
                END.                                                                      
            END.
            

            FOR EACH es-ben-estab WHERE es-ben-estab.cod-beneficio = tt-beneficio2.cod-beneficio
                                    AND es-ben-estab.dt-ini-vald   <= TODAY
                                    AND es-ben-estab.dt-fim-vald   >= TODAY
                                    AND es-ben-estab.cod-estabel    = tt-beneficio2.cod-estabel
                                    AND (es-ben-estab.pis-cofins-ben-pis-cof-compr-nac = YES OR
                                          es-ben-estab.pis-cofins-ben-pis-cof-imp = YES)
                                   BY    es-ben-estab.pis-cofins-prioridade :                      
          

                IF AVAIL(emitente) THEN DO:

                    IF emitente.pais <> "brasil" THEN DO:
                        IF es-ben-estab.pis-cofins-ben-pis-cof-imp <> TRUE THEN NEXT.
                    END.
                    ELSE DO:
                        IF es-ben-estab.pis-cofins-ben-pis-cof-compr-nac <> TRUE  THEN NEXT.
                    END.

                END.

                FIND tt-beneficio-valid WHERE    tt-beneficio-valid.cod-estabel    = es-ben-estab.cod-estabel
                                        AND      tt-beneficio-valid.it-codigo      = ext-item-cfa.it-codigo
                                        AND      tt-beneficio-valid.classe         = ext-item-cfa.classe
                                        AND      tt-beneficio-valid.tp-tax         = "PIS" NO-LOCK NO-ERROR.
           
           
                IF NOT AVAIL tt-beneficio-valid THEN DO:
                    CREATE tt-beneficio-valid.
                    ASSIGN tt-beneficio-valid.cod-beneficio  = es-ben-estab.cod-beneficio.
                           tt-beneficio-valid.cod-estabel    = es-ben-estab.cod-estabel.
                           tt-beneficio-valid.it-codigo      = tt-beneficio2.it-codigo.
                           tt-beneficio-valid.classe         = ext-item-cfa.classe.
                           tt-beneficio-valid.tp-tax         = "PIS".
                           tt-beneficio-valid.pri-icms       = es-ben-estab.pis-cofins-prioridade.
                           tt-beneficio-valid.r-ben-row      = ROWID(es-ben-estab).
           
                END.
                ELSE DO:
                    IF es-ben-estab.pis-cofins-prioridade <  tt-beneficio-valid.pri-icms THEN DO:
                        tt-beneficio-valid.pri-icms   = es-ben-estab.pis-cofins-prioridade.
                        tt-beneficio-valid.r-ben-row  = ROWID(es-ben-estab).
                    END.
           
                END.
           
            END.
        END.
        
        
    



        /*END.*/
















    


end.

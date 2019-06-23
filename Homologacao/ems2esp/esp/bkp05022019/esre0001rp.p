/*******************************************************************************
**
** Programa  : ESRE0001RP
**
** Objetivo  : Relat¢rio de BENS (RI)
**
** Autor     : Maurilio Macedo - 2019/01
**
** Versao    : 2.12.00.000 - Desenvolvimento Inicial
**
******************************************************************************/
{include/i-prgvrs.i ESRE0001RP 2.12.00.000}

/**********************  Defini‡Æo tabela de Parƒmetros **********************/
define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char
    field usuario           as char
    field data-exec         as date
    field hora-exec         as INTEGER
    field serie-docto-ini  like doc-fisico.cod-estabel 
    field serie-docto-fim  like doc-fisico.cod-estabel  
    field nro-docto-ini    like doc-fisico.nro-docto  
    field nro-docto-fim    like doc-fisico.nro-docto  
    field cod-emitente-ini like doc-fisico.cod-emitente
    field cod-emitente-fim like doc-fisico.cod-emitente      
    field dt-emissao-ini   like doc-fisico.dt-emissao
    field dt-emissao-fim   like doc-fisico.dt-emissao
    field dt-trans-ini      like doc-fisico.dt-trans
    field dt-trans-fim      like doc-fisico.dt-trans
    .

define temp-table tt-digita no-undo
    field it-codigo like ITEM.it-codigo
    index id it-codigo.
        
DEFINE temp-table tt-raw-digita
   field raw-digita      as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.    

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end. /* for each tt-raw-digita: */


/* Definicao dos Buffers */

/*-------------- Vari vel Excel ----------------*/
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.

/*------------------- Variaveis Documento --------------------*/
DEF VAR h-acomp    AS HANDLE NO-UNDO.
DEF VAR i-contador AS INT    NO-UNDO.
DEF VAR c-arquivo  AS CHAR   NO-UNDO.

/* In¡cio - Bloco Principal */
find first param-global NO-LOCK NO-ERROR.
find first param-compra NO-LOCK NO-ERROR.

DEFINE VARIABLE c-desc-item AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ct AS INTEGER     NO-UNDO.

{include/i-rpvar.i}

assign c-programa     = "ESRE0001"
       c-versao       = "2.12"
       c-revisao      = "00.000"
       c-empresa      = param-global.grupo
       c-sistema      = "Especifico"
       c-titulo-relat = "Relat¢rio de Documentos".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.  

RUN pi-inicializar IN h-acomp (INPUT "Aguarde, Gerando...").

{include/i-rpout.i}
{include/i-rpcab.i}
VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

FOR EACH doc-fisico NO-LOCK
   WHERE doc-fisico.dt-emissao   >= tt-param.dt-emissao-ini  
     AND doc-fisico.dt-emissao   <= tt-param.dt-emissao-fim
     AND doc-fisico.serie-docto  >= tt-param.serie-docto-ini       
     AND doc-fisico.serie-docto  <= tt-param.serie-docto-fim       
     AND doc-fisico.nro-docto    >= tt-param.nro-docto-ini 
     AND doc-fisico.nro-docto    <= tt-param.nro-docto-fim 
     AND doc-fisico.cod-emitente >= tt-param.cod-emitente-ini
     AND doc-fisico.cod-emitente <= tt-param.cod-emitente-fim
     AND doc-fisico.dt-trans     >= tt-param.dt-trans-ini
     AND doc-fisico.dt-trans     <= tt-param.dt-trans-fim:

    RUN pi-acompanhar IN h-acomp(STRING(doc-fisico.dt-emissao,"99/99/9999") + "-" +
                                 TRIM(doc-fisico.nro-docto)).

    FIND FIRST emitente NO-LOCK
         WHERE emitente.cod-emitente = doc-fisico.cod-emitente NO-ERROR.

    PUT "*----- ESTOQUE -----*" AT 56 SKIP (2)
        "Emitente.........: "        AT 01 doc-fisico.cod-emitente " - " emitente.nome-emit
        "Num‚ro da NF.....: "    AT 01 doc-fisico.nro-docto
        "S‚rie............: "           AT 01 doc-fisico.serie-docto
        "Data da EmissÆo..: " AT 01 doc-fisico.dt-emissao FORMAT "99/99/9999" 
        "Data da Transa‡Æo: " AT 01 doc-fisico.dt-emissao FORMAT "99/99/9999" SKIP(2)
        "Seq  Item      NARRATIVA                                       UND  DEP  LOCALIZ       CONTA CTB        NOME REQ           QUANT      LOTE/SRIE     " AT 01
        "---  --------  ----------------------------------------------  ---  ---  ------------  ---------------- -----------------  ---------  -------------  " AT 01.




        FOR EACH it-doc-fisico NO-LOCK OF doc-fisico,
           FIRST item NO-LOCK
           WHERE item.it-codigo = it-doc-fisico.it-codigo:

             c-desc-item = IF ITEM.narrativa = '' THEN ITEM.desc-item ELSE ITEM.narrativa.
             c-desc-item = REPLACE(c-desc-item,CHR(10),'').

             FIND FIRST ordem-compra NO-LOCK
                 WHERE ordem-compra.numero-ordem = it-doc-fisico.numero-ordem
                 NO-ERROR.

             PUT it-doc-fisico.sequencia FORMAT '>>9' AT 01
                 it-doc-fisico.it-codigo FORMAT 'x(08)' AT 06
                 substring(c-desc-item,1,46)             FORMAT 'x(46)' AT 16
                 ITEM.un               AT 64
                 .

             FIND FIRST rat-lote NO-LOCK OF it-doc-fisico NO-ERROR.

             IF AVAIL rat-lote THEN 
                 PUT rat-lote.cod-depos   AT 69
                     rat-lote.cod-localiz AT 74.

             IF AVAIL ordem-compra THEN DO:
                 FIND FIRST usuar_mestre NO-LOCK
                     WHERE usuar_mestre.cod_usuario = ordem-compra.usuario
                     NO-ERROR.
                 PUT ordem-compra.ct-codigo AT 88
                     usuar_mestre.nom_usuario FORMAT 'x(17)'  AT 105.
             END.
                 


             /*IF AVAIL rat-lote THEN 
                 PUT rat-lote.lote        AT 135.*/

             PUT SKIP.

             IF LENGTH(c-desc-item) > 46 THEN DO:
                 
                 REPEAT ct = 47 TO LENGTH(c-desc-item) BY 46:
                     PUT substring(c-desc-item,ct,46)  FORMAT 'x(46)' AT 16.

                 END.
             END.

             PUT "__________ _____________" AT 124.
        END.

        PUT  SKIP(2)
             "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
             .
        PAGE.
END.

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.
/* Final DO Programa */

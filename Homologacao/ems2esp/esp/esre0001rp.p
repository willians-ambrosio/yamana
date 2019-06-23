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
{pdf_inc.i "THIS-PROCEDURE"}
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
    FIELD pdf              AS LOG.

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
DEFINE VARIABLE c-pdf AS CHARACTER NO-UNDO.

{include/i-rpvar.i}

assign c-programa     = "ESRE0001"
       c-versao       = "2.12"
       c-revisao      = "00.000"
       c-empresa      = param-global.grupo
       c-sistema      = "Especifico"
       c-titulo-relat = "Contagem F¡sica de Itens".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.  

RUN pi-inicializar IN h-acomp (INPUT "Aguarde, Gerando...").
OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "Spdf.txt").
OUTPUT CLOSE.

{include/i-rpout.i}
{include/i-rpcab.i}
VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

IF tt-param.pdf THEN DO:

    ASSIGN c-pdf = "ESRE001" + STRING(TIME) + ".pdf".

    RUN pdf_new("Spdf",c-pdf).
    RUN pdf_set_orientation("Spdf","Landscape").
    RUN pdf_set_font ("Spdf","Courier",8).
    pdf_PageHeader ("Spdf", THIS-PROCEDURE, "PageHeader"). 
    pdf_PageFooter ("Spdf", THIS-PROCEDURE, "PageFooter"). 
    
END.

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
        "Emitente.........: "   AT 01 doc-fisico.cod-emitente " - " emitente.nome-emit
        "Num‚ro da NF.....: "   AT 01 doc-fisico.nro-docto
        "S‚rie............: "   AT 01 doc-fisico.serie-docto
        "Data da EmissÆo..: "   AT 01 doc-fisico.dt-emissao FORMAT "99/99/9999" 
        "Data da Transa‡Æo: "   AT 01 doc-fisico.dt-trans   FORMAT "99/99/9999" SKIP(2)

        "Seq  Item      NARRATIVA                                       UND  DEP  LOCALIZ       CONTA CTB        NOME REQ           QUANT      LOTE/SRIE     " AT 01
        "---  --------  ----------------------------------------------  ---  ---  ------------  ---------------- -----------------  ---------  -------------  " AT 01.


    IF tt-param.pdf THEN DO:

        RUN pdf_new_page("Spdf").
        RUN pdf_set_BottomMargin ("Spdf",40).

        RUN pi-imprime-pdf (INPUT "*----- ESTOQUE -----*", /* Informa‡Æo */
                            INPUT 76,   /* Coluna */
                            INPUT YES). /* skip */

        RUN pi-imprime-pdf (INPUT  "Emitente.........: ", /* Informa‡Æo */
                            INPUT 10, /* Coluna */
                            INPUT NO). /* skip */
        
        RUN pi-imprime-pdf (INPUT STRING(doc-fisico.cod-emitente) + " - " + emitente.nome-emit, /* Informa‡Æo */
                            INPUT 40, /* Coluna */
                            INPUT YES). /* skip */

        RUN pi-imprime-pdf (INPUT  "Num‚ro da NF.....: ", /* Informa‡Æo */
                            INPUT 10,   /* Coluna */
                            INPUT NO). /* skip */

        RUN pi-imprime-pdf (INPUT doc-fisico.nro-docto, /* Informa‡Æo */
                            INPUT 40, /* Coluna */
                            INPUT YES). /* skip */

        RUN pi-imprime-pdf (INPUT  "S‚rie............: ", /* Informa‡Æo */
                            INPUT 10, /* Coluna */
                            INPUT NO). /* skip */

        RUN pi-imprime-pdf (INPUT doc-fisico.serie-docto, /* Informa‡Æo */
                            INPUT 40, /* Coluna */
                            INPUT YES). /* skip */

        RUN pi-imprime-pdf (INPUT  "Data da EmissÆo..: ", /* Informa‡Æo */
                            INPUT 10, /* Coluna */
                            INPUT NO). /* skip */

        RUN pi-imprime-pdf (INPUT STRING(doc-fisico.dt-emissao,"99/99/9999"), /* Informa‡Æo */
                            INPUT 40, /* Coluna */
                            INPUT YES). /* skip */
        
        RUN pi-imprime-pdf (INPUT  "Data da Transa‡Æo: ", /* Informa‡Æo */
                            INPUT 10, /* Coluna */ 
                            INPUT NO).   

        RUN pi-imprime-pdf (INPUT STRING(doc-fisico.dt-trans,"99/99/9999"), /* Informa‡Æo */
                            INPUT 40, /* Coluna */
                            INPUT YES). /* skip */

        RUN pdf_skip IN h_PDFinc("Spdf").

        RUN pi-imprime-pdf (INPUT "Seq  Item      NARRATIVA                                       UND  DEP  LOCALIZ       CONTA CTB        NOME REQ           QUANT      LOTE/SRIE",
                            INPUT 10,
                            INPUT YES). /* skip */

        RUN pi-imprime-pdf (INPUT "---  --------  ----------------------------------------------  ---  ---  ------------  ---------------- -----------------  ---------  -------------",
                            INPUT 10,
                            INPUT YES). /* skip */
    END.

    FOR EACH it-doc-fisico NO-LOCK OF doc-fisico,
        FIRST item NO-LOCK
           WHERE item.it-codigo = it-doc-fisico.it-codigo:

             c-desc-item = IF ITEM.narrativa = '' THEN ITEM.desc-item ELSE ITEM.narrativa.
             c-desc-item = REPLACE(c-desc-item,CHR(10),'').

             FIND FIRST ordem-compra NO-LOCK
                 WHERE ordem-compra.numero-ordem = it-doc-fisico.numero-ordem
                 NO-ERROR.

             PUT it-doc-fisico.sequencia        FORMAT '>>9'    AT 01
                 it-doc-fisico.it-codigo        FORMAT 'x(08)'  AT 06
                 SUBSTRING(c-desc-item,1,46)    FORMAT 'x(46)'  AT 16
                 ITEM.un                                        AT 64.

             FIND FIRST rat-lote NO-LOCK OF it-doc-fisico NO-ERROR.

             IF AVAIL rat-lote THEN 
                 PUT rat-lote.cod-depos   AT 69
                     rat-lote.cod-localiz FORMAT "x(12)" AT 74.

             IF AVAIL ordem-compra THEN DO:
                 FIND FIRST usuar_mestre NO-LOCK
                     WHERE usuar_mestre.cod_usuario = ordem-compra.requisitante
                     NO-ERROR.
                 PUT ordem-compra.ct-codigo FORMAT "x(16)"    AT 88
                     usuar_mestre.nom_usuario FORMAT 'x(17)'  AT 105.
             END.

             IF tt-param.pdf THEN DO:

                 RUN pi-imprime-pdf (INPUT STRING(it-doc-fisico.sequencia,">>9"), /* Informa‡Æo */
                                     INPUT 10, /* Coluna */
                                     INPUT NO). /* skip */

                 RUN pi-imprime-pdf (INPUT it-doc-fisico.it-codigo, /* Informa‡Æo */
                                     INPUT 15, /* Coluna */
                                     INPUT NO). /* skip */

                 RUN pi-imprime-pdf (INPUT SUBSTRING(c-desc-item,1,46), /* Informa‡Æo */
                                     INPUT 25, /* Coluna */
                                     INPUT NO). /* skip */

                 RUN pi-imprime-pdf (INPUT ITEM.un, /* Informa‡Æo */
                                     INPUT 74, /* Coluna */
                                     INPUT NO). /* skip */

                 IF AVAIL rat-lote THEN DO:
                     RUN pi-imprime-pdf (INPUT rat-lote.cod-depos, /* Informa‡Æo */
                                         INPUT 78, /* Coluna */
                                         INPUT NO). /* skip */

                     RUN pi-imprime-pdf (INPUT rat-lote.cod-localiz, /* Informa‡Æo */
                                         INPUT 83, /* Coluna */
                                         INPUT NO). /* skip */
                 END.

                 IF AVAIL ordem-compra THEN DO:

                     RUN pi-imprime-pdf (INPUT ordem-compra.ct-codigo, /* Informa‡Æo */
                                         INPUT 97, /* Coluna */
                                         INPUT NO). /* skip */

                     RUN pi-imprime-pdf (INPUT SUBSTR(usuar_mestre.nom_usuario,1,17), /* Informa‡Æo */
                                         INPUT 114, /* Coluna */
                                         INPUT YES). /* skip */
                 END.
             END.
       
             PUT SKIP.

             IF LENGTH(c-desc-item) > 46 THEN DO:
                 
                 REPEAT ct = 47 TO LENGTH(c-desc-item) BY 46:
                     PUT substring(c-desc-item,ct,46)  FORMAT 'x(46)' AT 16.

                     IF tt-param.pdf THEN
                          RUN pi-imprime-pdf (INPUT SUBSTRING(c-desc-item,ct,46), /* Informa‡Æo */
                                              INPUT 26, /* Coluna */
                                              INPUT YES). /* skip */
                     
                 END.
             END.

             PUT "__________ _____________" AT 124.

             IF tt-param.pdf THEN 
                  RUN pi-imprime-pdf (INPUT "__________ _____________", /* Informa‡Æo */
                                      INPUT 134, /* Coluna */
                                      INPUT YES). /* skip */
        END.

        PUT  SKIP(2)
             FILL("+",150) FORMAT 'X(150)'.
        PAGE.

        IF tt-param.pdf THEN DO:

              RUN pdf_skip IN h_PDFinc("Spdf").
             
              RUN pi-imprime-pdf (INPUT FILL("+",150), /* Informa‡Æo */
                                  INPUT 10, /* Coluna */
                                  INPUT YES). /* skip */
        END.
END.


IF tt-param.pdf THEN DO:

    RUN pdf_close IN h_PDFinc("Spdf").
    RUN prgfin\apl\apya599.p (INPUT SESSION:TEMP-DIRECTORY + c-pdf).

    IF tt-param.destino = 1 /* Impressora */  THEN
    OS-COMMAND NO-WAIT COPY SESSION:TEMP-DIRECTORY + c-pdf PRINTER.

END.

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.
/* Final DO Programa */

PROCEDURE pi-imprime-pdf:
    DEFINE INPUT  PARAMETER ipc-dado AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipi-col AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipl-skip AS LOGICAL     NO-UNDO.

   RUN pdf_text_at IN h_PDFinc("Spdf", ipc-dado,ipi-col).

   IF ipl-skip THEN
        RUN pdf_skip IN h_PDFinc("Spdf").

END.

PROCEDURE PageFooter:
/*------------------------------------------------------------------------------
  Purpose:  Procedure to Print Page Footer -- on all pages.
------------------------------------------------------------------------------*/
  RUN pdf_skip ("Spdf").
  RUN pdf_set_dash ("Spdf",1,0).
  RUN pdf_line  ("Spdf", 50, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") - 5, 1).
  RUN pdf_skip ("Spdf").
  RUN pdf_skip ("Spdf").

  /* RUN pdf_text_to  ("Spdf",  "Page: " 
                           + STRING(pdf_page("Spdf"))
                           + " of " + pdf_TotalPages("Spdf"), 97). */

  RUN pdf_text_to ("Spdf", "DATASUL - " + c-sistema + " - " + CAPS(c-programa) + " - V:" + c-versao + c-revisao, 160).

END. /* PageFooter */

PROCEDURE PageHeader:
/*------------------------------------------------------------------------------
  Purpose:  Procedure to Print Page Footer -- on all pages.
------------------------------------------------------------------------------*/
  RUN pdf_set_dash ("Spdf",1,0).
  RUN pdf_text_to  ("Spdf",  c-empresa, 48).
  RUN pdf_text_to  ("Spdf",  c-titulo-relat, 100).
  RUN pdf_text_to  ("Spdf","Pag.: " 
                           + STRING(pdf_page("Spdf"))
                           + " de " 
                           + pdf_TotalPages("Spdf"), 175).
  RUN pdf_skip ("Spdf").
  RUN pdf_line  ("Spdf", 50, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") - 5, 1).
  RUN pdf_skip ("Spdf").
  RUN pdf_skip ("Spdf").
  RUN pdf_skip ("Spdf").

END. /* PageFooter */

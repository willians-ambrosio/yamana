/*------------------------------------------------------------------------------------------------------------
  Objetivo: Trigger de Write da Tabela doc-pend-aprov
  Autor: Sergio Luiz Neto da Silveira / DSC PRAXIS
  Data: 20/02/2017
  ------------------------------------------------------------------------------------------------------------*/

def param buffer p-doc-pend-aprov     for doc-pend-aprov.
def param buffer p-old-doc-pend-aprov for doc-pend-aprov.

DEFINE BUFFER b_doc-pend-aprov FOR doc-pend-aprov.

DEFINE VARIABLE l-aprovado AS LOGICAL   NO-UNDO INITIAL NO.

DEFINE NEW GLOBAL SHARED VARIABLE l-emergencial-cc0311 AS LOGICAL   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-arquivo            AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-exibe              AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE i-cont               AS INTEGER   NO-UNDO.
DEFINE                   VARIABLE raw-param            AS RAW       NO-UNDO.


{utp/ut-glob.i}
{utp/utapi019.i}
{cdp/cd0666.i}

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

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita   AS raw. 

IF l-emergencial-cc0311 THEN DO:
   blk:
   DO i-cont = 1 TO 10:
      IF R-INDEX(STRING(PROGRAM-NAME(i-cont)),"cc0311") > 0 THEN DO:
         FIND FIRST b_doc-pend-aprov 
              WHERE b_doc-pend-aprov.num-pedido = p-doc-pend-aprov.num-pedido
              NO-LOCK NO-ERROR.
         IF AVAILABLE(b_doc-pend-aprov) THEN DO:
            FIND CURRENT b_doc-pend-aprov 
                 EXCLUSIVE-LOCK NO-ERROR.

            ASSIGN b_doc-pend-aprov.ind-tip-doc = 6. 

            FIND CURRENT b_doc-pend-aprov 
                 NO-LOCK NO-ERROR.
         END.
            
         LEAVE blk.
      END.
   END.
END.



/* Pedidos de compra */
IF (p-doc-pend-aprov.ind-tip-doc = 4) OR (p-doc-pend-aprov.ind-tip-doc = 6) THEN DO:

   ASSIGN l-aprovado = NO.

   IF p-doc-pend-aprov.ind-situacao = 2 /* aprovado */ THEN DO:
      IF NOT CAN-FIND(FIRST b_doc-pend-aprov
                      WHERE b_doc-pend-aprov.ind-tip-doc  = p-doc-pend-aprov.ind-tip-doc AND
                            b_doc-pend-aprov.num-pedido   = p-doc-pend-aprov.num-pedido  AND
                            b_doc-pend-aprov.ind-situacao = 1 /*pendente */              AND
                            ROWID(b_doc-pend-aprov)      <> ROWID(p-doc-pend-aprov)    
                      NO-LOCK) THEN DO:
         ASSIGN l-aprovado = YES.
      END.
   END.

   IF l-aprovado THEN DO:
      RUN pi-imprime-pedido.

      IF c-arquivo <> "" THEN
         RUN pi-envia-email.
   END.
END.

PROCEDURE pi-imprime-pedido:
   CREATE tt-param.
   ASSIGN tt-param.destino                    = 3
          tt-param.usuario                    = c-seg-usuario
          tt-param.data-exec                  = TODAY         
          tt-param.hora-exec                  = TIME          
          tt-param.classifica                 = 1
          tt-param.desc-classifica            = ""
          tt-param.i-ini-num-pedido           = p-doc-pend-aprov.num-pedido
          tt-param.i-fim-num-pedido           = p-doc-pend-aprov.num-pedido 
          tt-param.i-ini-numero-ordem         = 0
          tt-param.i-fim-numero-ordem         = 999999999
          tt-param.l-contrato                 = YES
          tt-param.l-narrativa-ord-compr      = YES
          tt-param.l-narrativa-item-ord-compr = YES
          tt-param.l-narrativa-pedido         = YES
          tt-param.l-descricao                = YES
          tt-param.l-envia-email              = NO
          tt-param.i-idioma                   = 1.

   CASE tt-param.destino:
        WHEN 1 THEN ASSIGN tt-param.arquivo = "".
        WHEN 2 THEN ASSIGN tt-param.arquivo = STRING(p-doc-pend-aprov.num-pedido).
        WHEN 3 THEN ASSIGN tt-param.arquivo = SESSION:TEMP-DIRECTORY + STRING(p-doc-pend-aprov.num-pedido) + ".xlsx".
   END CASE.

   RAW-TRANSFER tt-param TO raw-param.

   ASSIGN c-arquivo = ""
          l-exibe   = NO.

   RUN esp/escc0305rp.p (INPUT raw-param,
                         INPUT TABLE tt-raw-digita).
END PROCEDURE.

PROCEDURE pi-envia-email :
   DEFINE VARIABLE c-destino    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE c-assunto    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE c-remetente  AS CHARACTER NO-UNDO.

   FIND FIRST param_email 
        NO-LOCK NO-ERROR.

   EMPTY TEMP-TABLE tt-envio2.
   EMPTY TEMP-TABLE tt-mensagem.
   EMPTY TEMP-TABLE tt-erros.

   FIND FIRST usuar-mater
        WHERE usuar-mater.cod-usuario = c-seg-usuario 
        NO-LOCK NO-ERROR.
   IF AVAILABLE(usuar-mater) THEN 
      ASSIGN c-remetente = usuar-mater.e-mail.

   FIND FIRST pedido-compr
        WHERE pedido-compr.num-pedido = p-doc-pend-aprov.num-pedido
        NO-LOCK NO-ERROR.
   IF AVAILABLE(pedido-compr) THEN DO:
      FIND FIRST usuar-mater
           WHERE usuar-mater.cod-usuario = pedido-compr.responsavel 
           NO-LOCK NO-ERROR.
      IF AVAILABLE(usuar-mater) THEN 
         ASSIGN c-destino = usuar-mater.e-mail.
   END.

   CREATE tt-envio2.
   ASSIGN tt-envio2.versao-integracao = 1
          tt-envio2.Servidor          = param_email.cod_servid_e_mail  WHEN AVAILABLE param_email
          tt-envio2.Porta             = param_email.num_porta          WHEN AVAILABLE param_email
          tt-envio2.remetente         = c-remetente
          tt-envio2.importancia       = 2
          tt-envio2.destino           = c-destino
          tt-envio2.copia             = ""
          tt-envio2.arq-anexo         = c-arquivo
          tt-envio2.log-enviada       = NO
          tt-envio2.formato           = "TXT"
          tt-envio2.assunto           = "Pedido de Compra Nro." + STRING(p-doc-pend-aprov.num-pedido).


/*    ASSIGN tt-envio2.Servidor = "10.130.210.117" */
/*           tt-envio2.Porta    = 25.              */
/*                                                 */


   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 1.
          tt-mensagem.mensagem     = "PEDIDO DE COMPRAS".

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 2.
          tt-mensagem.mensagem     = CHR(10) + CHR(13).

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 3.
          tt-mensagem.mensagem     = "Prezado(s) Senhor(es),".

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 4.
          tt-mensagem.mensagem     = CHR(10) + CHR(13).

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 5.
          tt-mensagem.mensagem     = "A µrea de Compras e Contratos informa que sua proposta de preáos foi a ganhadora conforme pedido(s) anexo(s).".

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 6.
          tt-mensagem.mensagem     = CHR(10) + CHR(13).

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 7.
          tt-mensagem.mensagem     = "Solicitamos sua especial atená∆o quanto ao atendimento no prazo de entrega estipulado.".

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 8.
          tt-mensagem.mensagem     = CHR(10) + CHR(13).

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 9.
          tt-mensagem.mensagem     = "OBS: ê OBRIGATORIO O NUMERO DO PEDIDO NA NOTA FISCAL.".

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 10.
          tt-mensagem.mensagem     = CHR(10) + CHR(13).

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 11.
          tt-mensagem.mensagem     = "Favor acusar o recebimento deste pedido.".

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 12.
          tt-mensagem.mensagem     = CHR(10) + CHR(13).

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 13.
          tt-mensagem.mensagem     = "Atenciosamente,".

   RUN utp/utapi019.p PERSISTENT SET h-utapi019.
   RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                  INPUT  TABLE tt-mensagem,
                                  OUTPUT TABLE tt-erros).
   DELETE PROCEDURE h-utapi019.
   
   IF CAN-FIND(FIRST tt-erros) THEN DO:
      EMPTY TEMP-TABLE tt-erro.
      
      FOR EACH tt-erros
               NO-LOCK:
         CREATE tt-erro.
         ASSIGN i-cont = i-cont + 1.
         
         ASSIGN tt-erro.i-sequen = i-cont
                tt-erro.cd-erro  = tt-erros.cod-erro
                tt-erro.mensagem = tt-erros.desc-erro.
      END.   
   
      IF OPSYS = "WIN32" THEN 
         RUN cdp/cd0666.w (INPUT TABLE tt-erro).   
   END.      
END PROCEDURE.



/****************************************************************************************** 
** 	   Programa: re0402rp-upc.p
**   	      Autor: 
**   	 Fornecedor: 
**    	 Data: 09/2018
** Change/Chamado:
**    Objetivo: Cria Documentos do Recebimento na Ilha de Dados (Desatualizacao)
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
** 30/10/2018   Vando Ribeiro	DKP         REQ03          Eliminar pendˆncias de aprova-
**                                                         ‡Æo de documentos.

****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: p-ind-event
** PAR¶METROS DE SAÖDA: tt-epc
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{utp/ut-glob.i}
{include/i-epc200.i}

DEF INPUT        PARAM p-ind-event AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

def var i-id        as integer no-undo.
def var i-id-it     as integer no-undo.
def var c-tipo-serv as char    no-undo.
DEF VAR l-total-ent    LIKE recebimento.valor-total NO-UNDO.
DEF VAR l-total-rateio LIKE recebimento.valor-total NO-UNDO.

DEF BUFFER b-docum-est    FOR docum-est.
def buffer b-item-doc-est for item-doc-est.

DEF TEMP-TABLE tt-item-doc-est NO-UNDO
    FIELDS numero-ordem LIKE item-doc-est.numero-ordem
    FIELDS num-pedido   LIKE item-doc-est.num-pedido
    FIELDS it-codigo    LIKE item-doc-est.it-codigo
    FIELDS parcela      LIKE item-doc-est.parcela
    FIELDS preco-total  LIKE item-doc-est.preco-total
    FIELDS dt-trans     LIKE docum-est.dt-trans.

DEF VAR c-item-cm AS CHAR NO-UNDO.
DEF VAR c-proj    AS CHAR NO-UNDO.

function fnRetornarData     returns date (input c-data-1 as char) forward.
function fnRetornarDataChar returns char  (input da-data  as date) forward.
function fnRetornarDataHoraChar returns char  (input da-data  as date) forward.

if p-ind-event = "NAO-DESATUALIZA":U then do:
    find first tt-epc
         where tt-epc.cod-event     = p-ind-event
           and tt-epc.cod-parameter = "ROWID-DOCUM-EST" no-error.
    if avail tt-epc then do:
       for first docum-est no-lock
           where rowid(docum-est) = to-rowid(tt-epc.val-parameter):

           FIND FIRST natur-oper
                 WHERE natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.

            IF AVAIL natur-oper AND natur-oper.nota-rateio THEN DO:
                FOR EACH rat-docum
                   WHERE rat-docum.serie-docto  = docum-est.serie-docto
                     AND rat-docum.nro-docto    = docum-est.nro-docto
                     AND rat-docum.cod-emitente = docum-est.cod-emitente
                     AND rat-docum.nat-operacao = docum-est.nat-operacao NO-LOCK:
    
                    FIND FIRST b-docum-est
                         WHERE b-docum-est.serie-docto  = rat-docum.nf-serie
                           AND b-docum-est.nro-docto    = rat-docum.nf-nro
                           AND b-docum-est.cod-emitente = rat-docum.nf-emitente
                           AND b-docum-est.nat-operacao = rat-docum.nf-nat-oper NO-LOCK NO-ERROR.
    
                    FOR EACH b-item-doc-est NO-LOCK OF b-docum-est:
                        CREATE tt-item-doc-est.
                        ASSIGN tt-item-doc-est.numero-ordem   = b-item-doc-est.numero-ordem
                               tt-item-doc-est.num-pedido     = b-item-doc-est.num-pedido
                               tt-item-doc-est.it-codigo      = b-item-doc-est.it-codigo
                               tt-item-doc-est.parcela        = b-item-doc-est.parcela
                               tt-item-doc-est.preco-total[1] = b-item-doc-est.preco-total[1]
                               tt-item-doc-est.dt-trans       = b-docum-est.dt-trans.
    
                        ASSIGN l-total-ent = l-total-ent + b-item-doc-est.preco-total[1].
                    END.
                END.
            
                FOR EACH item-doc-est no-lock of docum-est:
                    ASSIGN l-total-rateio = l-total-rateio + item-doc-est.preco-total[1].
                END.

                FOR EACH tt-item-doc-est NO-LOCK:
                    FIND FIRST ordem-compra  NO-LOCK
                         WHERE ordem-compra.numero-ordem = tt-item-doc-est.numero-ordem NO-ERROR.

                    IF NOT AVAIL ordem-compra OR (AVAIL ordem-compra AND ordem-compra.check-sum <> "CM") THEN
                        NEXT.
            
                    if ordem-compra.nr-contrato <> 0 THEN DO:
                       ASSIGN c-tipo-serv = "C":U
                              c-item-cm   = string(ordem-compra.nr-contrato)
                              c-proj      = STRING(ordem-compra.numero-ordem).
                    END.
                    ELSE DO: 
                       ASSIGN c-tipo-serv = "O":U
                              c-item-cm   = string(tt-item-doc-est.num-pedido)
                              c-proj      = STRING(ordem-compra.numero-ordem).
                    END.
                
                    FIND LAST proc_receb_ilha NO-LOCK NO-ERROR.
                    IF AVAIL proc_receb_ilha THEN 
                       ASSIGN i-id = proc_receb_ilha.id_Rec + 1.
                    ELSE
                       ASSIGN i-id = 1.

                    CREATE proc_receb_ilha.
                    ASSIGN proc_receb_ilha.id_rec           = i-id
                           proc_receb_ilha.nf               = docum-est.nro-docto
                           proc_receb_ilha.nr_pedido_cm     = c-item-cm
                           proc_receb_ilha.proj             = c-proj
                           proc_receb_ilha.tipo_nf          = "D":U
                           proc_receb_ilha.tipo_serv        = c-tipo-serv
                           proc_receb_ilha.mot_devol        = "":U
                           proc_receb_ilha.serieNF          = docum-est.serie
                           proc_receb_ilha.NatOp            = docum-est.nat-operacao
                           proc_receb_ilha.Vlr_NF           = ((tt-item-doc-est.preco-total[1] / l-total-ent)  * l-total-rateio).

                    FIND FIRST recebimento
                         WHERE recebimento.num-pedido   = tt-item-doc-est.num-pedido
                           AND recebimento.data-movto   = tt-item-doc-est.dt-trans
                           AND recebimento.numero-ordem = tt-item-doc-est.numero-ordem
                           AND recebimento.parcela      = tt-item-doc-est.parcela NO-LOCK NO-ERROR.
        
                    IF c-tipo-serv = "C":U THEN
                       ASSIGN proc_receb_ilha.seq_num = INT(SUBSTRING(recebimento.char-2,18,4)).
        
                    ASSIGN proc_receb_ilha.dt_pagto         = fnRetornarDataChar(docum-est.dt-atualiza)
                           proc_receb_ilha.dt_rec           = fnRetornarDataChar(docum-est.dt-emissao)
                           proc_receb_ilha.log-atualizado   = NO.
                   
                    IF c-tipo-serv = "O":U THEN DO:
                        FIND LAST proc_item_receb_ilha NO-LOCK NO-ERROR.
                        IF AVAIL proc_item_receb_ilha THEN 
                           ASSIGN i-id-it = proc_item_receb_ilha.id_ItRec + 1.
                        ELSE 
                           ASSIGN i-id-it = 1.
    
                        CREATE proc_item_receb_ilha.
                        ASSIGN proc_item_receb_ilha.id_ItRec = i-id-it
                               proc_item_receb_ilha.id_rec   = i-id 
                               proc_item_receb_ilha.cod_It   = tt-item-doc-est.it-codigo
                               proc_item_receb_ilha.qtde     = 0.
                    END.
                END.
            END.
            ELSE DO:
                FOR EACH item-doc-est no-lock of docum-est:
                    FIND FIRST ordem-compra  NO-LOCK
                         WHERE ordem-compra.numero-ordem = item-doc-est.numero-ordem NO-ERROR.
    
                    IF NOT AVAIL ordem-compra OR (AVAIL ordem-compra AND ordem-compra.check-sum <> "CM") THEN
                        RETURN "OK":U.                        
            
                    if ordem-compra.nr-contrato <> 0 THEN DO:
                       ASSIGN c-tipo-serv = "C":U
                              c-item-cm   = string(ordem-compra.nr-contrato)
                              c-proj      = STRING(ordem-compra.numero-ordem).
                    END.
                    ELSE DO: 
                       ASSIGN c-tipo-serv = "O":U
                              c-item-cm   = string(item-doc-est.num-pedido)
                              c-proj      = STRING(ordem-compra.numero-ordem).
                    END.
            
                    FOR EACH recebimento
                       WHERE recebimento.num-pedido   = item-doc-est.num-pedido
                         AND recebimento.numero-ordem = item-doc-est.numero-ordem
                         AND recebimento.parcela      = item-doc-est.parcela
                         AND recebimento.data-movto   = docum-est.dt-trans
                         AND recebimento.data-nota    = docum-est.dt-emissao
                         AND recebimento.cod-emitente = docum-est.cod-emitente
                         AND recebimento.serie-nota   = docum-est.serie-docto
                         AND recebimento.numero-nota  = docum-est.nro-docto
                         AND recebimento.it-codigo    = item-doc-est.it-codigo 
                         AND recebimento.int-1        = item-doc-est.sequencia NO-LOCK:
            
                        find last proc_receb_ilha no-lock no-error.
                        if avail proc_receb_ilha then
                           assign i-id = proc_receb_ilha.id_Rec + 1.
                        else
                           assign i-id = 1.
                       
                        create proc_receb_ilha.                                                
                        assign proc_receb_ilha.id_rec           = i-id                         
                               proc_receb_ilha.nf               = docum-est.nro-docto          
                               proc_receb_ilha.nr_pedido_cm     = c-item-cm   
                               proc_receb_ilha.proj             = c-proj          
                               proc_receb_ilha.tipo_nf          = "D":U                        
                               proc_receb_ilha.tipo_serv        = c-tipo-serv                  
                               proc_receb_ilha.mot_devol        = "":U                         
                               proc_receb_ilha.serieNF          = docum-est.serie              
                               proc_receb_ilha.NatOp            = docum-est.nat-operacao
                               proc_receb_ilha.Vlr_NF           = recebimento.valor-total.
            
                        IF c-tipo-serv = "C":U THEN
                           ASSIGN proc_receb_ilha.seq_num = INT(SUBSTRING(recebimento.char-2,18,4)).
            
                        assign proc_receb_ilha.dt_pagto         = fnRetornarDataChar(docum-est.dt-atualiza)
                               proc_receb_ilha.dt_rec           = fnRetornarDataChar(docum-est.dt-emissao)
                               proc_receb_ilha.log-atualizado   = NO.
                       
                        IF c-tipo-serv = "O":U THEN DO:
                            
                            find last proc_item_receb_ilha no-lock no-error.
                            if avail proc_item_receb_ilha then
                               assign i-id-it = proc_item_receb_ilha.id_ItRec + 1.
                            else
                               assign i-id-it = 1.
        
                            create proc_item_receb_ilha.
                            assign proc_item_receb_ilha.id_ItRec = i-id-it
                                   proc_item_receb_ilha.id_rec   = i-id 
                                   proc_item_receb_ilha.cod_It   = item-doc-est.it-codigo
                                   proc_item_receb_ilha.qtde     = recebimento.quant-receb * -1. 
                        END.
                    END.
                END.
            END.
        END.
    END.
    return "OK":U.
end.

IF p-ind-event = "Apos-Atualizacao" THEN DO: 
    FIND FIRST tt-epc
         WHERE tt-epc.cod-event     = "NAO-DESATUALIZA"
           AND tt-epc.cod-parameter = "ROWID-DOCUM-EST" NO-ERROR.
    IF AVAIL tt-epc THEN DO:

        FIND docum-est WHERE ROWID(docum-est) = TO-ROWID(tt-epc.val-parameter) NO-LOCK NO-ERROR.
        FIND FIRST esp_pend_aprov 
             WHERE esp_pend_aprov.serie-docto  = docum-est.serie-docto
               AND esp_pend_aprov.nro-docto    = docum-est.nro-docto     
               AND esp_pend_aprov.cod-emitente = docum-est.cod-emitente  
               AND esp_pend_aprov.nat-operacao = docum-est.nat-operacao 
               AND esp_pend_aprov.cod_usuario_aprovador1      <> ""
               AND esp_pend_aprov.data-aprovacao1             <> ? EXCLUSIVE-LOCK NO-ERROR.
        /*Elimina pendencia de aprova‡Æo do docto*/
        IF AVAIL esp_pend_aprov THEN
            DELETE esp_pend_aprov.

    END.
    RETURN "OK":U.
END.

function fnRetornarData returns date (input c-data-1 as char):

    define variable i-ano    as integer     no-undo.
    define variable i-mes    as integer     no-undo.
    define variable i-dia    as integer     no-undo.
    define variable c-data   as date        no-undo.
    
    assign i-ano = int(substring(c-data-1,1,4))
           i-mes = int(substring(c-data-1,5,2))
           i-dia = int(substring(c-data-1,7,2)). 

    assign c-data-1 = string(i-dia) + "/" + string(i-mes) + "/" + string(i-ano)
           c-data   = date(c-data-1).

    return c-data.
  
end function.

function fnRetornarDataChar returns char (input da-data as date):

   return string(year(da-data) ,"9999") + 
          string(month(da-data),"99")   +
          string(day(da-data)  ,"99").
  
end function.

function fnRetornarDataHoraChar returns char (input da-data as date):

   return string(year(da-data) ,"9999") + 
          string(month(da-data),"99")   +
          string(day(da-data)  ,"99")   + 
          replace(string(time, "hh:mm:ss"), ":", "").
  
end function.


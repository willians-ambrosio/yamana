/*******************************************************************************
** UPC - Cria DOcumentos do Recebimento na Ilha de Dados
** Speto / 2009
*******************************************************************************/
{utp/ut-glob.i}
{include/i-epc200.i}

DEF INPUT        PARAM p-ind-event AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

def var i-id        as integer no-undo.
def var i-id-it     as integer no-undo.
def var c-tipo-serv as char    no-undo.

DEF VAR c-item-cm AS CHAR NO-UNDO.
DEF VAR c-proj    AS CHAR NO-UNDO.

def buffer b-item-doc-est for item-doc-est.

function fnRetornarData     returns date (input c-data-1 as char) forward.
function fnRetornarDataChar returns char  (input da-data  as date) forward.
function fnRetornarDataHoraChar returns char  (input da-data  as date) forward.

if p-ind-event = "fim-atualizacao":U then do:

    find first tt-epc
         where tt-epc.cod-event     = p-ind-event
           and tt-epc.cod-parameter = "docum-est rowid" no-error.
    if avail tt-epc then do:

       for first docum-est no-lock
           where rowid(docum-est) = to-rowid(tt-epc.val-parameter):

           FOR EACH item-doc-est no-lock of docum-est:

                FIND FIRST ordem-compra  NO-LOCK
                     WHERE ordem-compra.numero-ordem = item-doc-est.numero-ordem NO-ERROR.

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
                           proc_receb_ilha.tipo_nf          = "R":U                        
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
                        
                        /*
                        FIND FIRST item 
                             WHERE item.it-codigo    = item-doc-est.it-codigo
                               AND item.ind-serv-mat = 2 NO-LOCK NO-ERROR.
                        IF AVAIL ITEM  THEN DO:
                        */
                            find last proc_item_receb_ilha no-lock no-error.
                            if avail proc_item_receb_ilha then
                               assign i-id-it = proc_item_receb_ilha.id_ItRec + 1.
                            else
                               assign i-id-it = 1.

                            create proc_item_receb_ilha.
                            assign proc_item_receb_ilha.id_ItRec = i-id-it
                                   proc_item_receb_ilha.id_rec   = i-id 
                                   proc_item_receb_ilha.cod_It   = item-doc-est.it-codigo
                                   proc_item_receb_ilha.qtde     = recebimento.quant-receb.

                        /*
                        END.
                        */

                    END.

                END.
                 
           END.

       end.
    end.
    return "OK":U.
end.

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


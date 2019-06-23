/*****************************************************************************************
**
** Programa: upc-re2005rp-u01
**
** Objetivo: Imprimir Log de Advertencia
**
** Autor   : Renato Oliveira
**
** Data    : Dezembro/2018
** 
** Versao  : 2.12.00.000 - Desenvolvimento Inicial
**
****************************************************************************************/
{include/i-prgvrs.i upc-re2005rp-u01 2.12.00.000}

{include/i-epc200.i re2005rp}

DEF INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/*     FOR EACH tt-epc.                            */
/*       MESSAGE tt-epc.cod-event SKIP             */
/*               tt-epc.cod-parameter SKIP         */
/*               string(tt-epc.val-parameter) SKIP */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
/*     END.                                        */

DEF VAR l-ok AS LOGICAL NO-UNDO.

{cdp/cd0666.i}

IF p-ind-event = "Inicio-Atualizacao" THEN DO:

    FOR FIRST tt-epc
        WHERE tt-epc.cod-event = p-ind-event
          AND tt-epc.cod-parameter = "doc-fisico rowid":

         FOR FIRST doc-fisico NO-LOCK
             WHERE ROWID(doc-fisico) = TO-ROWID(tt-epc.val-parameter).

             ASSIGN l-ok = YES.
             FOR EACH it-doc-fisico NO-LOCK OF doc-fisico,
                FIRST rat-lote NO-LOCK OF it-doc-fisico,
                FIRST item NO-LOCK
                WHERE item.it-codigo = rat-lote.it-codigo:

                 RUN pi-erro-nota (INPUT 27979,
                                   INPUT "Documento " ).
                 
             END.

             FIND FIRST es-doc-fisico-just NO-LOCK OF doc-fisico NO-ERROR.
             IF NOT AVAIL es-doc-fisico-just THEN DO:

                 IF l-ok = NO THEN DO:
                     PUT SKIP(1)
                         "*** Documento nÆo foi Atualizado ***" AT 01
                         "*** Solicite Aprova‡Æo para o Depto Fiscal *** " AT 01
                         SKIP(1).
                     RETURN "NOK".
                 END.

             END.
             ELSE DO:
                 PUT SKIP(1)
                     "*** Aprova‡Æo Efetuada pelo Depto Fiscal *** " AT 01 es-doc-fisico-just.char-1 FORMAT "x(30)"
                     SKIP(1).
                 RETURN "OK".
             END.

        END.

        PUT SKIP(1).

    END.

END.

IF p-ind-event = "Fim-Atualizacao" THEN DO:

    FOR FIRST tt-epc
        WHERE tt-epc.cod-event = p-ind-event
          AND tt-epc.cod-parameter = "doc-fisico rowid":

         FOR FIRST doc-fisico NO-LOCK
             WHERE ROWID(doc-fisico) = TO-ROWID(tt-epc.val-parameter).

             FOR FIRST es-doc-fisico EXCLUSIVE-LOCK OF doc-fisico:
                 ASSIGN SUBSTRING(es-doc-fisico.char-1,1,10) = "Atualizado".
                 RELEASE es-doc-fisico.
             END.

         END.

    END.

END.

/* Procedures Internas */
procedure pi-erro-nota:

    def input parameter cod-mensag  as integer                      no-undo.
    def input parameter c-complem   as char                         no-undo.

    def var c-cabec1 as char format "X(132)".
    def var c-cabec2 as char format "X(132)".          
    def var c-textolabel as char no-undo.

/*     run utp/ut-msgs.p ( input "msg",        */
/*                         input cod-mensag,   */
/*                         input c-complem  ). */
/*                                             */
/*     create tt-erro.                         */
/*     assign tt-erro.cd-erro  = cod-mensag    */
/*            tt-erro.mensagem = return-value  */
/*            l-erro           = yes.          */

    if line-counter > 62 then
        page.

    if line-counter <= 5 then do:

        assign c-cabec1 = "Seq Item             Descri‡Æo                                Dep    Localiz      Qtde Digitada Lote/S‚rie Validade Lote Informa‡Æo"
               c-cabec2 = "--- ---------------- ---------------------------------------- ------ ---------- --------------- ---------- ------------- -----------------------------------------------------".

        put c-cabec1 at 0 skip
            c-cabec2 at 0 skip.

    end.

    put it-doc-fisico.sequencia   at  1   FORMAT ">>9"
        it-doc-fisico.it-codigo   at  5   FORMAT "x(16)"
        item.desc-item            at 22   FORMAT "x(40)"
        rat-lote.cod-depos        at 63   FORMAT "x(03)"
        rat-lote.cod-localiz      at 70   FORMAT "x(10)"
        rat-lote.quantidade       at 83   FORMAT ">,>>>,>>9.999"
        rat-lote.lote             at 97   FORMAT "x(10)"
        rat-lote.dt-vali-lote     at 108  FORMAT "99/99/9999".

    IF it-doc-fisico.quantidade = it-doc-fisico.quant-conf THEN DO:
        PUT "Ok" AT 122.
    END.
    ELSE DO:

        ASSIGN l-ok = NO.

        PUT "Quantidade Divergente" AT 122.

        CREATE tt-erro.
        ASSIGN tt-erro.cd-erro  = cod-mensag
               tt-erro.mensagem = "Quantidade Divergente".

    END.

end.    


RETURN "OK":u.

/* Fim do Programa */

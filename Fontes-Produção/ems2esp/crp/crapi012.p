/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer portador             for ems2cadme.portador.
def buffer empresa              for ems2cadme.empresa.
def buffer cheque               for ems2cadme.cheque.
def buffer histor_exec_especial for ems2cadme.histor_exec_especial.

{include/i-prgvrs.i CRAPI012 2.00.00.033}  /*** 010033 ***/
/******************************************************************************
**
**    Programa: CRAPI012.P
**
**    Objetivo: API consulta contabiliza‡Æo.
**
*******************************************************************************/
{utp/ut-glob.i}
{crp/crapi012.i}

{include/i_dbvers.i} /* miniflexibiliza‡Æo */

def input  parameter table for tt-mov-tit.
def output parameter table for tt-conta-contabil.
def output parameter table for tt-erro.

def buffer b-esp-doc                for esp-doc.
def buffer b-relacto-titulo-cheque  for relacto-titulo-cheque.
def buffer b-conta-contab           for conta-contab.
def buffer b-portador               for ems2cadme.portador.
def buffer b-mov-tit                for mov-tit.
def buffer b-titulo                 for titulo.

def var c-mensagem            as character format "x(60)"      no-undo.
def var c-conta-var-mon       like conta-contab.conta-contabil no-undo.
def var c-conta-1             like conta-contab.conta-contabil no-undo.
def var c-conta-2             like conta-contab.conta-contabil no-undo.
def var c-conta-3             like conta-contab.conta-contabil no-undo.
def var c-modalidade          as character format "X(13)"      no-undo.
def var i-cont-tit            as integer                       no-undo.
def var l-antec-contabil      as logical   initial yes         no-undo.
def var de-total-cheque       as decimal                       no-undo.
def var de-total-cheque-me    as decimal                       no-undo.
def var de-cheque             as decimal                       no-undo.
def var de-baixa              as decimal                       no-undo.
def var de-total-baixados     as decimal                       no-undo.
def var i-versao-integ        as integer                       no-undo.
def var de-vl-cart-receb      as decimal                       no-undo.

def var i-periodo             as integer                       no-undo.
def var l-contab-exerc-ant    as logical                       no-undo.
def var l-ok                  as logical                       no-undo.

def var l-usa-cobranca as log no-undo.
def var l-planta-centr as log no-undo.
def var l-usa-trans    as log no-undo.
def var c-conta-db like conta-cr.conta-saldo no-undo.
def var c-conta-cr like conta-cr.conta-saldo no-undo.

/* IN 381 */
def var de-vl-pis          like titulo.vl-saldo             NO-UNDO.
def var de-vl-cof          like titulo.vl-saldo             NO-UNDO.
def var de-vl-csl          like titulo.vl-saldo             NO-UNDO.
def var de-vl-est-pis      like titulo.vl-saldo             NO-UNDO.
def var de-vl-est-cof      like titulo.vl-saldo             NO-UNDO.
def var de-vl-est-csl      like titulo.vl-saldo             NO-UNDO.
DEF VAR c-conta-pis-cr     LIKE conta-contab.conta-contabil NO-UNDO.
DEF VAR c-conta-cof-cr     LIKE conta-contab.conta-contabil NO-UNDO.
DEF VAR c-conta-csl-cr     LIKE conta-contab.conta-contabil NO-UNDO.
DEF VAR c-conta-pis-db LIKE conta-contab.conta-contabil NO-UNDO.
DEF VAR c-conta-cof-db LIKE conta-contab.conta-contabil NO-UNDO.
DEF VAR c-conta-csl-db LIKE conta-contab.conta-contabil NO-UNDO.

def var c-conta-despes like cta-corrente.conta-despes-banc.
DEF VAR l-cta-despesa    AS LOGICAL NO-UNDO.
DEF VAR l-retem-imposto  AS LOGICAL NO-UNDO.
DEF VAR l-estorn-impto   AS LOGICAL NO-UNDO.

IF NO THEN
    FIND FIRST titulo NO-LOCK NO-ERROR.

/* Definicao das funcoes disponiveis a partir da versao 2.03 */
{cdp/cdcfgfin.i} 

assign i-versao-integ = 001.

find first tt-mov-tit no-lock no-error.

/*----- Testa VersÆo Integra‡Æo ------*/
if  tt-mov-tit.cod-versao-integ <> i-versao-integ then do:
    if  tt-mov-tit.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 3941,
                           input "").
        return error.
    end.
    else do: 
        run utp/ut-msgs.p (input "msg",
                           input 3941,
                           input "").
        assign c-mensagem = return-value.

        create tt-erro.
        assign tt-erro.i-cod-erro     = 3941
               tt-erro.c-desc-erro    = c-mensagem.
    end.    
end.

find first mov-tit 
    where rowid(mov-tit) = tt-mov-tit.gr-mov-tit no-lock no-error.

if  not avail mov-tit then do:
    if  tt-mov-tit.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 949,
                           input "").
        return error.
    end.
    else do: 
        run utp/ut-msgs.p (input "msg",
                           input 949,
                           input "").
        assign c-mensagem = return-value.

        create tt-erro.
        assign tt-erro.i-cod-erro     = 949
               tt-erro.c-desc-erro    = c-mensagem.
    end.
end.

find first portador
     where portador.ep-codigo    = mov-tit.ep-codigo
     and   portador.cod-portador = mov-tit.cod-portador
     and   portador.modalidade   = mov-tit.modalidade
     no-lock  no-error.

find first cta-corrente 
     where cta-corrente.cod-banco    = portador.cod-banco
     and   cta-corrente.agencia      = portador.agencia
     and   cta-corrente.conta-corren = portador.conta-corren
     no-lock no-error.

find first empresa 
    where empresa.ep-codigo = mov-tit.ep-codigo no-lock no-error.
if  not avail empresa then do:
    if  tt-mov-tit.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 12,
                           input "").
        return error.
    end.
    else do: 
        run utp/ut-msgs.p (input "msg",
                           input 12,
                           input "").
        assign c-mensagem = return-value.

        create tt-erro.
        assign tt-erro.i-cod-erro     = 12
               tt-erro.c-desc-erro    = c-mensagem.
    end.
end.

find first param-cr 
     where param-cr.ep-codigo = empresa.ep-codigo no-lock no-error.
if  not avail param-cr then
    return.

/* Verifica se usa cobran‡a centralizada, conta transit¢ria e se a planta ‚ centralizadora */
&if defined(BF_FIN_COBRANCA_CENTR) &then
    assign l-usa-cobranca = param-cr.log-cob-centr
           l-planta-centr = param-cr.log-planta-centr
           l-usa-trans    = param-cr.log-usa-trans.
&else
    find first funcao 
        where  funcao.cd-funcao = "spp-cobranca-centr" no-lock no-error.
    if avail funcao then 
        assign l-usa-cobranca = param-cr.log-2
               l-planta-centr = param-cr.contr-ant
               l-usa-trans    = param-cr.desconto.
&endif.

find first esp-doc 
     where esp-doc.cod-esp = mov-tit.cod-esp no-lock no-error.
if  not avail esp-doc then do:
    if  tt-mov-tit.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 950,
                           input "").
        return error.
    end.
    else do: 
        run utp/ut-msgs.p (input "msg",
                           input 950,
                           input "").
        assign c-mensagem = return-value.

        create tt-erro.
        assign tt-erro.i-cod-erro     = 950
               tt-erro.c-desc-erro    = c-mensagem.
    end.
end.

find first emitente
     where emitente.cod-emitente = mov-tit.cod-emitente
     and   emitente.identific   <> 2 no-lock no-error.
if  not avail emitente then do:
    if  tt-mov-tit.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 47,
                           input "").
        return error.
    end.
    else do: 
        run utp/ut-msgs.p (input "msg",
                           input 47,
                           input "").
        assign c-mensagem = return-value.

        create tt-erro.
        assign tt-erro.i-cod-erro     = 47
               tt-erro.c-desc-erro    = c-mensagem.
    end.
end.

/* Rotina para verificar se a funcaoo Conta de Despesa no Estorno de Desconto Bancario esta ativa */
ASSIGN l-cta-despesa = no.
&IF DEFINED(BF_FIN_CTA_DESPESA) &THEN
    ASSIGN l-cta-despesa = YES.
&ELSE
    IF CAN-FIND (funcao
        WHERE  funcao.cd-funcao = 'spp-cta-despesa'
        AND  funcao.ativo = YES) THEN DO:
        ASSIGN l-cta-despesa = YES.
    END.
&ENDIF

do  on error undo, return:

    if  esp-doc.tipo <> 2 and mov-tit.esp-ant <> "" then do:

        find first b-esp-doc
             where b-esp-doc.cod-esp = mov-tit.esp-ant no-lock no-error.
        if  avail b-esp-doc then do:
            if  b-esp-doc.tipo <> 4 and b-esp-doc.tipo <> 5 then do:
                find first conta-cr
                     where conta-cr.ep-codigo   = mov-tit.ep-codigo
                     and   conta-cr.cod-estabel = mov-tit.cod-estabel
                     and   conta-cr.cod-esp     = mov-tit.esp-ant
                     and   conta-cr.cod-gr-cli  = emitente.cod-gr-cli
                     no-lock no-error.

                if  not avail conta-cr then do:
                    if  tt-mov-tit.l-vid-rel = yes then do:
                        run utp/ut-msgs.p (input "show",
                                           input 926,
                                           input "").
                        return error.
                    end.
                    else do: 
                        run utp/ut-msgs.p (input "msg",
                                           input 926,
                                           input "").
                        assign c-mensagem = return-value.
                        create tt-erro.
                        assign tt-erro.i-cod-erro     = 926
                               tt-erro.c-desc-erro    = c-mensagem.
                    end.
                end.
                else        
                    assign c-conta-1 = conta-cr.conta-saldo
                           c-conta-3 = conta-cr.conta-saldo.

            end.
        end.       
    end.

    find first conta-cr use-index codigo
         where conta-cr.ep-codigo   = mov-tit.ep-codigo
         and   conta-cr.cod-estabel = mov-tit.cod-estabel
         and   conta-cr.cod-esp     = mov-tit.cod-esp
         and   conta-cr.cod-gr-cli  = emitente.cod-gr-cli
         no-lock no-error.

    if  not avail conta-cr then do:
        if  tt-mov-tit.l-vid-rel = yes then do:
            run utp/ut-msgs.p (input "show",
                               input 926,
                               input "").
            return error.
        end.
        else do: 
            run utp/ut-msgs.p (input "msg",
                               input 926,
                               input "").
            assign c-mensagem = return-value.
            create tt-erro.
            assign tt-erro.i-cod-erro     = 926
                   tt-erro.c-desc-erro    = c-mensagem.
        end.
    end.

    ASSIGN l-retem-imposto = NO.
    IF  i-pais-impto-usuario = 1 THEN DO:
        FIND FIRST b-titulo USE-INDEX codigo NO-LOCK
             WHERE b-titulo.ep-codigo   = mov-tit.ep-codigo
               AND b-titulo.cod-estabel = mov-tit.cod-estabel
               AND b-titulo.cod-esp     = mov-tit.cod-esp
               AND b-titulo.serie       = mov-tit.serie
               AND b-titulo.nr-docto    = mov-tit.nr-docto
               AND b-titulo.parcela     = mov-tit.parcela NO-ERROR.
        IF  AVAIL b-titulo AND 
            &IF "{&mgadm_version}" < "2.06" &THEN
            SUBSTR(b-titulo.char-2,46,1) = 'S'
            &ELSE
            b-titulo.l-ret-impto
            &ENDIF THEN
            ASSIGN l-retem-imposto = YES.
    END.

    /* IN 381 */
    IF i-pais-impto-usuario   = 1   AND   /* Brasil */
       l-retem-imposto              AND   /* Retem imposto na implantacao */
       ((mov-tit.transacao    = 13  AND   /* Acerto de Valores */
        mov-tit.tipo          = 1   AND   /* Normal */
        mov-tit.lancamento    = 1)  OR    /* D‚bito */
       (mov-tit.transacao     = 3   AND   /* Devolu‡Æo */
        mov-tit.tipo          = 1))       /* Normal */
    THEN DO:
        ASSIGN de-vl-pis      = 0
               de-vl-cof      = 0
               de-vl-csl      = 0
               c-conta-pis-cr = ""  c-conta-pis-db = ""
               c-conta-cof-cr = ""  c-conta-cof-db = ""
               c-conta-csl-cr = ""  c-conta-csl-db = "".
        IF  NOT AVAIL titulo 
            OR titulo.ep-codigo   <> mov-tit.ep-codigo
            OR titulo.cod-estabel <> mov-tit.cod-estabel
            OR titulo.cod-esp     <> mov-tit.cod-esp
            OR titulo.serie       <> mov-tit.serie
            OR titulo.nr-docto    <> mov-tit.nr-docto
            OR titulo.parcela     <> mov-tit.parcela THEN
            FIND FIRST titulo USE-INDEX codigo NO-LOCK
                 WHERE titulo.ep-codigo   = mov-tit.ep-codigo
                 AND   titulo.cod-estabel = mov-tit.cod-estabel
                 AND   titulo.cod-esp     = mov-tit.cod-esp
                 AND   titulo.serie       = mov-tit.serie
                 AND   titulo.nr-docto    = mov-tit.nr-docto
                 AND   titulo.parcela     = mov-tit.parcela NO-ERROR.

        &if "{&mgadm_version}":U < "2.06":U &then
            assign de-vl-pis      = dec(substr(titulo.char-2,1,15))  
                   de-vl-cof      = dec(substr(titulo.char-2,16,15)) 
                   de-vl-csl      = dec(substr(titulo.char-2,31,15))
                   c-conta-pis-cr = trim(substr(conta-cr.char-2,34,8)) + TRIM(SUBSTR(conta-cr.char-2,42,8))  
                   c-conta-cof-cr = trim(substr(conta-cr.char-2,66,8)) + TRIM(SUBSTR(conta-cr.char-2,74,8))  
                   c-conta-csl-cr = trim(substr(conta-cr.char-2,98,8)) + TRIM(SUBSTR(conta-cr.char-2,106,8))
                   c-conta-pis-db = TRIM(substr(conta-cr.char-2,18,8)) + TRIM(substr(conta-cr.char-2,26,8))
                   c-conta-cof-db = TRIM(substr(conta-cr.char-2,50,8)) + TRIM(substr(conta-cr.char-2,58,8))
                   c-conta-csl-db = TRIM(substr(conta-cr.char-2,82,8)) + TRIM(substr(conta-cr.char-2,90,8)).
        &else
            assign de-vl-pis      = titulo.vl-pis    
                   de-vl-cof      = titulo.vl-cofins 
                   de-vl-csl      = titulo.vl-csll
                   c-conta-pis-cr = conta-cr.conta-pis-cr
                   c-conta-cof-cr = conta-cr.conta-cofins-cr
                   c-conta-csl-cr = conta-cr.conta-csll-cr
                   c-conta-pis-db = conta-cr.conta-pis-db
                   c-conta-cof-db = conta-cr.conta-cofins-db
                   c-conta-csl-db = conta-cr.conta-csll-db.
        &endif
        
        &if "{&mgadm_version}":U < "2.06B":U &then
            ASSIGN l-estorn-impto = IF (SUBSTRING(mov-tit.char-2,122,1) = "" 
                                    OR SUBSTRING(mov-tit.char-2,122,1) = "N") THEN NO ELSE YES.
        &else
            ASSIGN l-estorn-impto = mov-tit.l-eston-impto.
        &endif
        
        /*--------- VERIFICA€ÇO PIS --------*/
        if  de-vl-pis > 0
        AND (c-conta-pis-cr = "" OR c-conta-pis-db = "") then do: 
            if  tt-mov-tit.l-vid-rel = yes then do:
                run utp/ut-msgs.p (input "show", input 29586,
                                   input string("PIS") + "~~" +
                                         string(conta-cr.ep-codigo) + "~~" +
                                         conta-cr.cod-estabel + "~~" +
                                         conta-cr.cod-esp + "~~" +
                                         string(conta-cr.cod-gr-cli)).
                RETURN ERROR.
            END.
            ELSE DO:
                run utp/ut-msgs.p (input "help", input 29586,
                                   input string("PIS") + "~~" +
                                         string(conta-cr.ep-codigo) + "~~" +
                                         conta-cr.cod-estabel + "~~" +
                                         conta-cr.cod-esp + "~~" +
                                         string(conta-cr.cod-gr-cli)).
                assign c-mensagem = trim(return-value).
                create tt-erro.
                assign tt-erro.i-cod-erro     = 29586
                       tt-erro.c-desc-erro    = c-mensagem.
            END.
        end.

        /*--------- VERIFICA€ÇO COFINS --------*/

        if  de-vl-cof > 0
        AND (c-conta-cof-cr = "" OR c-conta-cof-db = "") then do: 
            if  tt-mov-tit.l-vid-rel = yes then do:
                run utp/ut-msgs.p (input "show", input 29586,
                                   input string("COFINS") + "~~" +
                                         string(conta-cr.ep-codigo) + "~~" +
                                         conta-cr.cod-estabel + "~~" +
                                         conta-cr.cod-esp + "~~" +
                                         string(conta-cr.cod-gr-cli)).
                RETURN ERROR.
            END.
            ELSE DO:
                run utp/ut-msgs.p (input "help", input 29586,
                                   input string("COFINS") + "~~" +
                                         string(conta-cr.ep-codigo) + "~~" +
                                         conta-cr.cod-estabel + "~~" +
                                         conta-cr.cod-esp + "~~" +
                                         string(conta-cr.cod-gr-cli)).
                assign c-mensagem = trim(return-value).
                create tt-erro.
                assign tt-erro.i-cod-erro     = 29586
                       tt-erro.c-desc-erro    = c-mensagem.
            END.
        end.

        /*--------- VERIFICA€ÇO CSLL --------*/
        if  de-vl-csl > 0
        AND (c-conta-csl-cr = "" OR c-conta-csl-db = "") then do: 
            if  tt-mov-tit.l-vid-rel = yes then do:
                run utp/ut-msgs.p (input "show", input 29586,
                                   input string("CSLL") + "~~" +
                                         string(conta-cr.ep-codigo) + "~~" +
                                         conta-cr.cod-estabel + "~~" +
                                         conta-cr.cod-esp + "~~" +
                                         string(conta-cr.cod-gr-cli)).
                RETURN ERROR.
            END.
            ELSE DO:
                run utp/ut-msgs.p (input "help", input 29586,
                                   input string("CSLL") + "~~" +
                                         string(conta-cr.ep-codigo) + "~~" +
                                         conta-cr.cod-estabel + "~~" +
                                         conta-cr.cod-esp + "~~" +
                                         string(conta-cr.cod-gr-cli)).
                assign c-mensagem = trim(return-value).
                create tt-erro.
                assign tt-erro.i-cod-erro     = 29586
                       tt-erro.c-desc-erro    = c-mensagem.
            END.
        end.

    END.

    if  mov-tit.transacao <> 4  /* Implanta‡Æo Antecipa‡Æo */
    and mov-tit.transacao <> 14 /* Implanta‡Æo */
    and mov-tit.transacao <> 16 /* Corr Monet Integral */
    and mov-tit.transacao <> 15 /* Ganhos Perdas FASB */ 
    and mov-tit.transacao <> 13 /* Acerto Valor */
    and mov-tit.transacao <> 19 /* Varia‡Æo Cambial Maior */
    and mov-tit.transacao <> 20 /* Varia‡Æo Cambial Menor */
    and mov-tit.baixa-subs = no then do:

        find first portador
             where portador.ep-codigo    = mov-tit.ep-codigo
             and   portador.cod-portador = mov-tit.cod-portador
             and   portador.modalidade   = mov-tit.modalidade
             no-lock  no-error.
        if  not avail portador then do:
            &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
                DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
                ASSIGN cAuxTraducao001 = {adinc/i03ad209.i 04 mov-tit.modalidade}.
                run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                                    INPUT "",
                                    INPUT "").
                ASSIGN  c-modalidade = RETURN-VALUE.
            &else
                ASSIGN c-modalidade = {adinc/i03ad209.i 04 mov-tit.modalidade}.
            &endif
            if  tt-mov-tit.l-vid-rel = yes then do:
                run utp/ut-msgs.p (input "show",
                                   input 927,
                                   input "").
                return error.
            end.
            else do: 
                run utp/ut-msgs.p (input "msg",
                                   input 927,
                                   input "").
                assign c-mensagem = return-value.
                create tt-erro.
                assign tt-erro.i-cod-erro     = 927
                       tt-erro.c-desc-erro    = c-mensagem.
            end.
        end.
        else do:
            if  portador.bancario = yes then do:
                find first cta-corrente 
                     where cta-corrente.cod-banco    = portador.cod-banco
                     and   cta-corrente.agencia      = portador.agencia
                     and   cta-corrente.conta-corren = portador.conta-corren
                     no-lock no-error.
                if  not avail cta-corrente then do:
                    if  tt-mov-tit.l-vid-rel = yes then do:
                        run utp/ut-msgs.p (input "show",
                                           input 928,
                                           input "").
                        return error.
                    end.
                    else do: 
                        run utp/ut-msgs.p (input "msg",
                                           input 928,
                                           input "").
                        assign c-mensagem = return-value.
                        create tt-erro.
                        assign tt-erro.i-cod-erro     = 928
                               tt-erro.c-desc-erro    = c-mensagem.
                    end.
                end.
            end.
        end.    
    end.

    if  mov-tit.transacao = 16 then do: /* Corr Monet Integral */
        if  mov-tit.esp-ant = " " then
            assign c-conta-1 = conta-cr.conta-saldo.
        find first ext-cmi
             where ext-cmi.ep-codigo      = conta-cr.ep-codigo
             and   ext-cmi.conta-contabil = c-conta-1
             no-lock no-error.
    end.

    if  mov-tit.transacao = 15    /* Ganhos/Perdas FASB */
    or (mov-tit.transacao = 2 and /* Baixa */
        esp-doc.tipo      = 6 and 
        mov-tit.vl-ant-fasb <> 0) then do:
        find first ext-fasb
             where ext-fasb.ep-codigo      = conta-cr.ep-codigo
             and   ext-fasb.conta-contabil = conta-cr.conta-saldo
             no-lock no-error.
        if avail ext-fasb then do:
           /* empresa.tp-varia‡Æo
              -------------------
              1 - Varia‡Æo Cambial
              2 - PrevisÆo */

            assign c-conta-1 = ext-fasb.conta-traducao
                   c-conta-2 = (if empresa.tp-variacao = 1 
                                then ext-fasb.conta-traducao
                                else ext-fasb.conta-efeito-maxi).
        end.
    end.

    /*--- Transa‡Æo 14 - Implanta‡Æo ---*/
    if  mov-tit.transacao = 14 then do:
        if  mov-tit.baixa-subs = no then do:

            assign c-conta-db = conta-cr.conta-saldo
                   c-conta-cr = mov-tit.conta-credito.

            /* Inicio: VALIDA€åES PARA COBRAN€A CENTRALIZADA*/
            run pi-valida-cobranca-centralizada.      
            if  avail conta-cr then do:
                run pi-criar-tt-conta-contabil (Input c-conta-db,
                                                Input 1,
                                                input mov-tit.vl-original).
                if  mov-tit.frete > 0 then
                    run pi-criar-tt-conta-contabil (Input conta-cr.conta-frete,
                                                    Input 2,
                                                    input mov-tit.frete).
                if  mov-tit.diversos > 0 then
                   run pi-criar-tt-conta-contabil (Input conta-cr.conta-diversos,
                                                   Input 2,
                                                   input mov-tit.diversos).
            end.
            run pi-criar-tt-conta-contabil (Input c-conta-cr,
                                            Input 2,
                                            input (mov-tit.vl-original - 
                                                   mov-tit.frete - 
                                                   mov-tit.diversos)).
        end.
        else do:

            run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                            Input 1,
                                            input mov-tit.vl-original).

            /*---- Localiza Conta Transit¢ria de Substitui‡Æo ----*/
            find first b-conta-contab
                 where b-conta-contab.ep-codigo      = mov-tit.ep-codigo
                 and   b-conta-contab.conta-contabil = param-cr.conta-trans-retencao no-lock no-error.

            if  avail b-conta-contab then do:
                run pi-criar-tt-conta-contabil (Input b-conta-contab.conta-contabil,
                                                Input 2,
                                                input mov-tit.vl-original).
            end.
        end.
    end.

    /*--- Transa‡Æo 1 - Altera‡Æo ---*/
    if  mov-tit.transacao  = 1
    and esp-doc.tipo = 2 then do: /* Antecipa‡Æo */
        if  avail conta-cr then do:
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                            Input 1,
                                            input mov-tit.vl-antecip).
            run pi-criar-tt-conta-contabil (Input mov-tit.conta-credito,
                                            Input 2,
                                            input mov-tit.vl-antecip).
        end.
    end.

    /*--- Transa‡Æo 4 - Implanta‡Æo Antecipa‡Æo ---*/
    if  mov-tit.transacao = 4 then do:

        RUN crp/cr9500.p(INPUT  mov-tit.num-id-mov-tit,
                         OUTPUT de-total-cheque,
                         OUTPUT de-total-cheque-me).

        /* Busca o portador/modalidade cheque do primeiro cheque */
        for each  relacto-titulo-cheque
            where relacto-titulo-cheque.num-id-mov-tit = mov-tit.num-id-mov-tit 
            no-lock:

            find first cheque no-lock
                where  cheque.cod-banco    = relacto-titulo-cheque.cod-banco
                and    cheque.agencia      = relacto-titulo-cheque.agencia
                and    cheque.conta-corren = relacto-titulo-cheque.conta-corren
                and    cheque.nr-cheque    = relacto-titulo-cheque.nr-cheque
                no-error.

            if  avail cheque then do:
                find first b-portador
                    where b-portador.ep-codigo  = mov-tit.ep-codigo
                    and   b-portador.cod-port   = cheque.cod-port
                    and   b-portador.modalidade = 8
                    no-lock no-error.
            end.
            LEAVE.
        end.

        IF  de-total-cheque > 0 AND AVAIL b-portador THEN DO:

            run pi-criar-tt-conta-contabil (Input if avail b-portador 
                                                  then b-portador.conta-contabil
                                                  else "",
                                            Input 1,
                                            input de-total-cheque).
        END.

        if  avail conta-cr then do:
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                            Input 2,
                                            input mov-tit.vl-original).
            if (mov-tit.vl-original - de-total-cheque) > 0 then
                run pi-criar-tt-conta-contabil (Input mov-tit.conta-credito,
                                                Input 1,
                                                input (mov-tit.vl-original -
                                                       de-total-cheque)).
        end.
    end.

    /*--- Transa‡Æo 13 - Acerto Valor ---*/
    if  mov-tit.transacao = 13 then do:
        if  avail conta-cr then do:
            if  mov-tit.tipo = 1 
            or  mov-tit.tipo = 7 then do:  /* Normal */
                if  mov-tit.lancamento = 2 then do: /* Cr‚dito */
                    run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                    Input 1,
                                                    input mov-tit.vl-baixa).
                    run pi-criar-tt-conta-contabil (Input mov-tit.conta-credito,
                                                    Input 2,
                                                    input mov-tit.vl-baixa).
                end.
                if  mov-tit.lancamento = 1 then do: /* D‚bito */
                    ASSIGN de-vl-est-pis = 0
                           de-vl-est-cof = 0
                           de-vl-est-csl = 0.

                    /* Cr‚dito */
                    run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,Input 2,input mov-tit.vl-baixa).
                    
                    /* IN 381 */
                    IF  mov-tit.vl-baixa-me  <> 0 AND
                        l-retem-imposto           AND
                        i-pais-impto-usuario  = 1 THEN DO:
                        if  de-vl-pis <> 0 then do:
                            ASSIGN de-vl-est-pis      = ((mov-tit.vl-baixa * de-vl-pis) / titulo.vl-original).
                            IF  l-estorn-impto THEN
                                run pi-criar-tt-conta-contabil (Input c-conta-pis-db,Input 1, input de-vl-est-pis).
                            run pi-criar-tt-conta-contabil (Input c-conta-pis-cr,Input 2, input de-vl-est-pis).
                        end.

                        if  de-vl-cof <> 0 then do:
                            ASSIGN de-vl-est-cof      = ((mov-tit.vl-baixa * de-vl-cof) / titulo.vl-original).
                            IF  l-estorn-impto THEN
                                run pi-criar-tt-conta-contabil (Input c-conta-cof-db,Input 1,input de-vl-est-cof).
                            run pi-criar-tt-conta-contabil (Input c-conta-cof-cr,Input 2,input de-vl-est-cof).
                        end.

                        if  de-vl-csl <> 0 then do:
                            ASSIGN de-vl-est-csl      = ((mov-tit.vl-baixa * de-vl-csl) / titulo.vl-original).
                            IF  l-estorn-impto THEN
                                run pi-criar-tt-conta-contabil (Input c-conta-csl-db,Input 1,input de-vl-est-csl).
                            run pi-criar-tt-conta-contabil (Input c-conta-csl-cr,Input 2,input de-vl-est-csl).
                        end.                                             
                    END.

                    /* d‚bito */
                    IF  l-estorn-impto THEN
                        run pi-criar-tt-conta-contabil (Input mov-tit.conta-credito,Input 1,input mov-tit.vl-baixa).
                    ELSE
                        run pi-criar-tt-conta-contabil (Input mov-tit.conta-credito,Input 1,input (mov-tit.vl-baixa + de-vl-est-pis + 
                                                                                                   de-vl-est-cof    + de-vl-est-csl)).

                    if  mov-tit.vl-var-monet <> 0 then
                        run pi-criar-tt-conta-contabil (Input conta-cr.conta-var-monetaria,
                                                        Input (if mov-tit.vl-var-monet > 0 
                                                               then 2
                                                               else 1),
                                                        input mov-tit.vl-var-monet).
                end.
            end.
            else do:
                if  mov-tit.tipo = 2 then do: /* Antecipa‡Æo */
                    run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                    Input (if mov-tit.lancamento = 1 
                                                           then 2
                                                           else 1),
                                                    input mov-tit.vl-baixa).
                    run pi-criar-tt-conta-contabil (Input mov-tit.conta-credito,
                                                    Input mov-tit.lancamento,
                                                    input mov-tit.vl-baixa).
                end.
            end.
        end.

        /*--- Estorno valores referente AVA devolu‡Æo cheques ---*/
        &if  defined(BF_FIN_CHEQUES_CRP) &then 
            if  entry(01, mov-tit.char-1, ";") = "DEV_CH":U then do:
                run pi-estorno-ava-devolucao-cheque.
            end.
        &endif        

    end.

    if  mov-tit.transacao = 2 
    or mov-tit.transacao = 3 
    or mov-tit.transacao = 5 then do:
        if  mov-tit.vl-var-monet <> 0 then 
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-var-monetaria,
                                            Input (if mov-tit.vl-var-monet > 0
                                                   then 2
                                                   else 1),
                                            input if mov-tit.vl-var-monet < 0 then
                                                     mov-tit.vl-var-monet * -1
                                                  else mov-tit.vl-var-monet).
    end.

    /*--- Transa‡Æo 2 - Baixa ---*/
    if  mov-tit.transacao = 2 then do:
        if  esp-doc.tipo = 6 and mov-tit.vl-ant-fasb <> 0 then do:
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                            Input if mov-tit.vl-ant-fasb > 0 
                                                  then 2 
                                                  else 1,
                                            input mov-tit.vl-ant-fasb).
            run pi-criar-tt-conta-contabil (Input c-conta-1,
                                            Input if mov-tit.vl-ant-fasb > 0 
                                                  then 1 
                                                  else 1,
                                            input mov-tit.vl-ant-fasb).
        end.

        if  mov-tit.vl-antecip > 0 then do:
            find first esp-doc
                 where esp-doc.cod-esp = mov-tit.esp-ant
                 no-lock no-error.
            if  avail esp-doc then do:
                if  esp-doc.contabiliza = yes then do:
                    if  mov-tit.vl-var-monet-antecip <> 0 then
                        run pi-criar-tt-conta-contabil (Input conta-cr.conta-var-monetaria,
                                                        Input if mov-tit.vl-var-monet-antecip > 0 
                                                              then 1 
                                                              else 2,
                                                        input mov-tit.vl-var-monet-antecip).
                    if  esp-doc.tipo = 6 then do:
                        run pi-criar-tt-conta-contabil (Input c-conta-3,
                                                        Input 1,
                                                        input mov-tit.vl-baixa).
                        run pi-criar-tt-conta-contabil (Input conta-cr.conta-juros-recebidos,
                                                        Input 2,
                                                        input mov-tit.vl-baixa).
                    end.
                    else do:
                        run pi-criar-tt-conta-contabil (Input c-conta-1,
                                                        Input 1,
                                                        input mov-tit.vl-baixa).
                        run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                        Input 2,
                                                        input mov-tit.vl-baixa).
                    end.
                    if  mov-tit.vl-baixa > mov-tit.vl-antecip then
                        run pi-criar-tt-conta-contabil (Input if portador.bancario
                                                              then cta-corrente.conta-saldo
                                                              else portador.conta-contabil,
                                                        Input 1,
                                                        input mov-tit.vl-baixa).
                    else do:
                        if  mov-tit.vl-baixa = mov-tit.vl-antecip
                        and mov-tit.vl-desconto > 0 then do:
                            run pi-criar-tt-conta-contabil (Input if portador.bancario 
                                                                  then cta-corrente.conta-descontos
                                                                  else portador.conta-contabil,
                                                            Input 2,
                                                            input mov-tit.vl-baixa).
                        end.
                    end.
                    assign l-antec-contabil = yes.
                end.
                else 
                    assign l-antec-contabil = no.
            end.
            else do:
                /* Esp‚cie de antecipa‡Æo nÆo cadastrada */
                run utp/ut-msgs.p (input "show",
                                   input 931,
                                   input " ").
            end.
        end.

        assign de-vl-cart-receb = 0.

        &if "{&mgadm_version}" >= "2.02" &then
                vers_block:
                do:
                    find first histor_exec_especial no-lock
                         where histor_exec_especial.cod_modul_dtsul = 'CRP'
                         and   histor_exec_especial.cod_prog_dtsul  = 'EMS_202_DESPESAS_CARTORIO' no-error.

                    if  not avail histor_exec_especial then
                        leave vers_block.

                    assign de-vl-cart-receb = mov-tit.vl-desp-cart-recebidas.

                    if  de-vl-cart-receb > 0 then do:  /* Despesas Cartorio Recebidas */
                        find estabelec where 
                             estabelec.cod-estabel = mov-tit.cod-estabel
                             no-lock no-error.
                        if  avail estabelec then 
                            run pi-criar-tt-conta-contabil (Input estabelec.conta-desp-cart-receb,
                                                            Input 2,
                                                            input de-vl-cart-receb).
                    end.                
                end.
        &endif.

        if  mov-tit.vl-antecip <= 0 or l-antec-contabil = no then do:
            if  mov-tit.modalidade = 2 then do:  /* Desconto */
                run pi-criar-tt-conta-contabil (Input cta-corrente.conta-tit-desc,
                                                Input 1,
                                                input mov-tit.vl-baixa).
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                Input 2,
                                                input mov-tit.vl-baixa).

                if  mov-tit.vl-juros-rec > 0 then do:
                    run pi-criar-tt-conta-contabil (Input cta-corrente.conta-juros-recebidos,
                                                    Input 1,
                                                    input mov-tit.vl-juros-rec).
                end.
                if  mov-tit.vl-desconto > 0 then do:
                    run pi-criar-tt-conta-contabil (Input cta-corrente.conta-saldo,
                                                    Input 2,
                                                   input mov-tit.vl-desconto).
                end.                     
            end.
            else do:

                if  mov-tit.baixa-subs = no then do:

                    if  mov-tit.vl-baixa > mov-tit.vl-antecip then do:

                        RUN crp/cr9500.p(INPUT  mov-tit.num-id-mov-tit,
                                         OUTPUT de-total-cheque,
                                         OUTPUT de-total-cheque-me).

                        /* Busca o portador/modalidade cheque do primeiro cheque */
                        for each  relacto-titulo-cheque
                            where relacto-titulo-cheque.num-id-mov-tit = mov-tit.num-id-mov-tit 
                            no-lock:

                            find first cheque no-lock
                                where  cheque.cod-banco    = relacto-titulo-cheque.cod-banco
                                and    cheque.agencia      = relacto-titulo-cheque.agencia
                                and    cheque.conta-corren = relacto-titulo-cheque.conta-corren
                                and    cheque.nr-cheque    = relacto-titulo-cheque.nr-cheque
                                no-error.

                            if  avail cheque then do:
                                find first b-portador
                                    where b-portador.ep-codigo  = mov-tit.ep-codigo
                                    and   b-portador.cod-port   = cheque.cod-port
                                    and   b-portador.modalidade = 8
                                    no-lock no-error.
                            end.
                            LEAVE.
                        end.

                        if  de-total-cheque > 0 then 
                            run pi-criar-tt-conta-contabil (input b-portador.conta-contabil,
                                                            input 1,
                                                            input de-total-cheque).

                        if (mov-tit.vl-baixa -
                            de-total-cheque -
                            mov-tit.vl-abatimen - 
                            mov-tit.vl-desconto  +
                            mov-tit.vl-var-monet +
                            mov-tit.vl-juros-rec +
                            mov-tit.vl-multa     +
                            de-vl-cart-receb) > 0 then 
                           run pi-criar-tt-conta-contabil (Input if portador.bancario 
                                                                 then cta-corrente.conta-saldo
                                                                 else portador.conta-contabil,
                                                           Input 1,
                                                           input (mov-tit.vl-baixa - 
                                                                  de-total-cheque -
                                                                  mov-tit.vl-abatimen - 
                                                                  mov-tit.vl-desconto  +
                                                                  mov-tit.vl-var-monet +
                                                                  mov-tit.vl-juros-rec +
                                                                  mov-tit.vl-multa     + 
                                                                  de-vl-cart-receb)).
                    end.

                    &if "{&mgadm_version}" >= "2.02" &then
                        
                        vers_block:
                        do:
                            &if "{&mgadm_version}" = "2.02" &then
                                find first histor_exec_especial no-lock
                                   where histor_exec_especial.cod_modul_dtsul = 'CRP'
                                   and   histor_exec_especial.cod_prog_dtsul  = 'ems_202_perdas_dedutiveis' no-error.
                                if  not avail histor_exec_especial then 
                                    leave vers_block.  
                            &endif

                            assign l-contab-exerc-ant = no.                                

                            if  esp-doc.tipo    = 6 and /* Juros */
                                esp-doc.cod-esp = param-cr.cod-esp-perdas-dedutiv then do:                                    

                                find titulo use-index codigo where
                                     titulo.ep-codigo   = mov-tit.ep-codigo and
                                     titulo.cod-estabel = mov-tit.cod-estabel and
                                     titulo.cod-esp     = mov-tit.cod-esp and
                                     titulo.serie       = mov-tit.serie and
                                     titulo.nr-docto    = mov-tit.nr-docto and
                                     titulo.parcela     = mov-tit.parcela 
                                     no-lock no-error.
                                if  avail titulo then do:

                                    /* Verifica se emissao documento Perdas Dedut¡veis est  
                                       no exerc¡cio Anterior */
                                    assign l-ok = no.           
                                    for each ano-fiscal where
                                             ano-fiscal.ep-codigo   = mov-tit.ep-codigo and
                                             ano-fiscal.ano-fiscal >= ( year(mov-tit.dt-trans) - 1)
                                             no-lock:                                                                                             
                                        do  i-periodo = 1 to ano-fiscal.num-periodos:
                                            if  mov-tit.dt-trans >= ano-fiscal.ini-periodo[i-periodo] and
                                                mov-tit.dt-trans <= ano-fiscal.fim-periodo[i-periodo] then do:

                                                if  titulo.dt-emissao < ano-fiscal.ini-periodo[1] then
                                                    assign l-contab-exerc-ant = yes
                                                           l-ok               = yes.
                                                leave.
                                            end.                                                
                                        end.
                                        if  l-ok = yes then
                                            leave.
                                    end. /* for each ano-fiscal */                                                                                                                                                                                        

                                end. /* if  avial titulo */
                            end. /* if  esp-doc.tipo = 6 */

                            if  l-contab-exerc-ant = yes then  
                                run pi-criar-tt-conta-contabil (Input conta-cr.conta-perd-dedut-exerc-an,
                                                                Input 2,
                                                                input mov-tit.vl-baixa).                                                                            
                            else                                                       
                                run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                                Input 2,
                                                                input mov-tit.vl-baixa).
                        end.                                
                    &endif 

                    &if "{&mgadm_version}" <= "2.02" &then
                        vers_block:
                        do:
                            &if "{&mgadm_version}" = "2.02" &then
                                find first histor_exec_especial no-lock
                                   where histor_exec_especial.cod_modul_dtsul = 'CRP'
                                   and   histor_exec_especial.cod_prog_dtsul  = 'ems_202_perdas_dedutiveis' no-error.
                                if  avail histor_exec_especial then 
                                    leave vers_block.  
                            &endif

                            run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                            Input 2,
                                                            input mov-tit.vl-baixa).

                        end.    
                    &endif                             

                end. /* mov-tit.baixa-subs = no */

                if  mov-tit.baixa-subs = yes then do:
                    run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                    Input 2,
                                                    input mov-tit.vl-baixa).

                    /*---- Localiza Conta Transit¢ria de Substitui‡Æo ----*/
                    find first b-conta-contab
                         where b-conta-contab.ep-codigo      = mov-tit.ep-codigo
                         and   b-conta-contab.conta-contabil = param-cr.conta-trans-retencao no-lock no-error.

                    if  avail b-conta-contab then do:
                        run pi-criar-tt-conta-contabil (Input b-conta-contab.conta-contabil,
                                                        Input 1,
                                                        input mov-tit.vl-baixa).
                    end.
                end.
            end.
        end.

        if  mov-tit.vl-desp-banc > 0 then do:
            find first estabelec
                 where estabelec.cod-estabel = mov-tit.cod-estabel no-lock no-error.

            run pi-criar-tt-conta-contabil (Input estabelec.conta-despesas,
                                            Input 1,
                                            input mov-tit.vl-desp-banc).

            run pi-criar-tt-conta-contabil (Input if portador.bancario 
                                                  then cta-corrente.conta-despes-banc 
                                                  else portador.conta-contabil,
                                            Input 2,
                                            input mov-tit.vl-desp-banc).

        end.

        if  mov-tit.vl-juros-rec > 0 then do:
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-juros-recebidos,
                                            Input 2,
                                            input mov-tit.vl-juros-rec).
        end.

        if  mov-tit.vl-multa > 0 then do:
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-multa,
                                            Input 2,
                                            input mov-tit.vl-multa).
        end.

        if  mov-tit.vl-desconto > 0 then
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-descontos,
                                            Input 1,
                                            input mov-tit.vl-desconto).
        if  mov-tit.vl-abatimen > 0 then
            run pi-criar-tt-conta-contabil (Input conta-cr.conta-abatimento,
                                            Input 1,
                                            input mov-tit.vl-abatimen).

        IF l-retem-imposto THEN DO:
           &if "{&mgadm_version}":U < "2.06":U &then
            IF NUM-ENTRIES(mov-tit.char-1,";") > 2 THEN DO:
                if dec(entry(3,mov-tit.char-1,";")) <> 0 then do: /*PIS*/
                    run pi-criar-tt-conta-contabil (Input TRIM(substr(conta-cr.char-2,18,8)) + TRIM(substr(conta-cr.char-2,26,8)),
                                                    Input 1,
                                                    input dec(entry(3,mov-tit.char-1,";"))).
                    run pi-criar-tt-conta-contabil (Input TRIM(substr(conta-cr.char-2,34,8)) + TRIM(substr(conta-cr.char-2,42,8)),
                                                    Input 2,
                                                    input dec(entry(3,mov-tit.char-1,";"))).
                end.
            END.
            IF NUM-ENTRIES(mov-tit.char-1,";") > 3 THEN DO:
                if dec(entry(4,mov-tit.char-1,";")) <> 0 then do: /*COFINS*/
                    run pi-criar-tt-conta-contabil (Input TRIM(substr(conta-cr.char-2,50,8)) + TRIM(substr(conta-cr.char-2,58,8)),
                                                    Input 1,
                                                    input dec(entry(4,mov-tit.char-1,";"))).
                    run pi-criar-tt-conta-contabil (Input TRIM(substr(conta-cr.char-2,66,8)) + TRIM(substr(conta-cr.char-2,74,8)),
                                                    Input 2,
                                                    input dec(entry(4,mov-tit.char-1,";"))).
                end.
            END.
            IF NUM-ENTRIES(mov-tit.char-1,";") > 4 THEN DO:
                if dec(entry(5,mov-tit.char-1,";")) <> 0 then do: /*CSLL*/                                                  
                    run pi-criar-tt-conta-contabil (Input TRIM(substr(conta-cr.char-2,82,8)) + TRIM(substr(conta-cr.char-2,90,8)), 
                                                    Input 1,
                                                    input dec(entry(5,mov-tit.char-1,";"))).
                    run pi-criar-tt-conta-contabil (Input TRIM(substr(conta-cr.char-2,98,8)) + TRIM(substr(conta-cr.char-2,106,8)),
                                                    Input 2,
                                                    input dec(entry(5,mov-tit.char-1,";"))).
                end.
            END.
           &else
            if mov-tit.vl-pis <> 0 then do: /*PIS*/
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-pis-db,
                                                Input 1,
                                                input mov-tit.vl-pis).
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-pis-cr,
                                                Input 2,
                                                input mov-tit.vl-pis).
            end.                        
            if mov-tit.vl-cofins <> 0 then do: /*COFINS*/
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-cofins-db,
                                                Input 1,
                                                input mov-tit.vl-cofins).
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-cofins-cr,
                                                Input 2,
                                                input mov-tit.vl-cofins).
            end.                        
            if mov-tit.vl-csll <> 0 then do: /*CSLL*/
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-csll-db,
                                                Input 1,
                                                input mov-tit.vl-csll).
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-csll-cr,
                                                Input 2,
                                                input mov-tit.vl-csll).
            end.                        
           &endif
        END.
    end.

    /*--- Transa‡Æo 3 - Devolu‡Æo ---*/
    if  mov-tit.transacao = 3 then do:
        if  avail mov-tit then do:

            ASSIGN de-vl-est-pis = 0
                   de-vl-est-cof = 0
                   de-vl-est-csl = 0.

            /* O teste de conta-credito <> " " esta sendo feito para diferenciar os movimentos que vieram do
               recebimento daquekes que foram gerados no contas a receber.
               Quando o movimento vem do recebimento a conta-credito ‚ informada normalmente, buscando, neste
               caso, a conta-credito do proprio movimento.
               Quando o movimento ‚ do contas a receber a conta-credito est  sendo gravada em branco, por isto,
               neste caso, busca a conta-credito na tabela de contas de saldo. */

            /* IN 381 */
            IF  mov-tit.tipo         = 1 AND /* Normal */
                l-retem-imposto          AND
                i-pais-impto-usuario = 1 THEN DO:  
                if  de-vl-pis <> 0 THEN
                    ASSIGN de-vl-est-pis      = ((mov-tit.vl-baixa * de-vl-pis) / titulo.vl-original).

                if  de-vl-cof <> 0 THEN
                    ASSIGN de-vl-est-cof      = ((mov-tit.vl-baixa * de-vl-cof) / titulo.vl-original).

                if  de-vl-csl <> 0 THEN 
                    ASSIGN de-vl-est-csl      = ((mov-tit.vl-baixa * de-vl-csl) / titulo.vl-original).
            END.

            run pi-criar-tt-conta-contabil (Input if  mov-tit.conta-credito <> " " then
                                                      mov-tit.conta-credito
                                                  else
                                                      conta-cr.conta-devolucao,
                                            Input 1,
                                            input (mov-tit.vl-baixa + de-vl-est-pis +
                                                   de-vl-est-cof    + de-vl-est-csl)).

            /* IN 381 */
            IF  mov-tit.tipo         = 1 AND  /* Normal */
                l-retem-imposto          AND
                i-pais-impto-usuario = 1 THEN DO: 
                if  de-vl-est-pis <> 0 then do:
                    run pi-criar-tt-conta-contabil (Input c-conta-pis-cr,
                                                    Input 2,
                                                    input de-vl-est-pis).
                end.

                if  de-vl-est-cof <> 0 then do:
                    run pi-criar-tt-conta-contabil (Input c-conta-cof-cr,
                                                    Input 2,
                                                    input de-vl-est-cof).
                end.

                if  de-vl-est-csl <> 0 then do:
                    run pi-criar-tt-conta-contabil (Input c-conta-csl-cr,
                                                    Input 2,
                                                    input de-vl-est-csl).
                end.                                             
            END.

            run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                            Input 2,
                                            input mov-tit.vl-baixa).
            if  mov-tit.vl-juros-rec > 0 
            and avail cta-corrente then do:
                run pi-criar-tt-conta-contabil (Input if portador.bancario then
                                                      cta-corrente.conta-juros-recebidos
                                                      else portador.conta-contabil,
                                                Input 1,
                                                input mov-tit.vl-juros-rec).
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-juros-recebidos,
                                                Input 2,
                                                input mov-tit.vl-juros-rec).
            end.
            if  mov-tit.vl-desconto > 0 then do:
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-descontos,
                                                Input 1,
                                                input mov-tit.vl-desconto).
            end.
            if  mov-tit.vl-abatimen > 0 then do:
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-abatimento,
                                                Input 1,
                                                input mov-tit.vl-abatimen).
            end.
        end.
    end.

    /*--- Transa‡Æo 5 - Desconto ---*/ 
    if  mov-tit.transacao = 5 then do:
        find first estabelec
             where estabelec.cod-estabel = mov-tit.cod-estabel 
             no-lock no-error.

        if  avail cta-corrente then do:
            run pi-criar-tt-conta-contabil (Input cta-corrente.conta-saldo,
                                            Input 1,
                                            input mov-tit.vl-baixa).
            run pi-criar-tt-conta-contabil (Input cta-corrente.conta-tit-desc,
                                            Input 2,
                                            input mov-tit.vl-baixa).
            if  (mov-tit.vl-desp-financ + mov-tit.vl-desp-banc) > 0 then do:
                run pi-criar-tt-conta-contabil (Input if portador.bancario 
                                                      then cta-corrente.conta-despes-financ 
                                                      else portador.conta-contabil,
                                                Input 2,
                                                input (mov-tit.vl-desp-financ + mov-tit.vl-desp-banc)).

                run pi-criar-tt-conta-contabil (Input estabelec.conta-despesas,
                                                Input 1,
                                                input (mov-tit.vl-desp-banc + mov-tit.vl-desp-financ)).
            end.

            if  mov-tit.vl-iof > 0 then do:
                run pi-criar-tt-conta-contabil (Input cta-corrente.conta-impto-oper-financ,
                                                Input 1,
                                                input mov-tit.vl-iof).
                run pi-criar-tt-conta-contabil (Input cta-corrente.conta-saldo,
                                                Input 2,
                                                input mov-tit.vl-iof).
            end.
        end.
    end.

    /*--- Transa‡Æo 6 - Estorno Desc Dupl ---*/ 
    if  mov-tit.transacao = 6 then do:

        if  l-cta-despesa then do:
            &if "{&mgadm_version}" < "2.06" &then
                if  SUBSTRING(mov-tit.char-2,50,66) <> "" then
                    c-conta-despes = SUBSTRING(mov-tit.char-2,50,66).
            &else
                if string(mov-tit.conta-despes) <> "" then
                    assign c-conta-despes = string(mov-tit.conta-despes).
            &endif
                else
                    assign c-conta-despes = if portador.bancario then cta-corrente.conta-despes-financ else portador.conta-contabil.
        end.
        else
            assign c-conta-despes = cta-corrente.conta-despes-financ.

        find first estabelec
             where estabelec.cod-estabel = mov-tit.cod-estabel 
             no-lock no-error.

        if  avail cta-corrente then do:
            run pi-criar-tt-conta-contabil (Input cta-corrente.conta-tit-desc,
                                            Input 1,
                                            input mov-tit.vl-baixa).

            run pi-criar-tt-conta-contabil (Input cta-corrente.conta-saldo,
                                            Input 2,
                                            input mov-tit.vl-baixa).

            if  (mov-tit.vl-desp-banc + mov-tit.vl-desp-financ) > 0 then do:

                run pi-criar-tt-conta-contabil (Input estabelec.conta-despesas,
                                                Input 1,
                                                input (mov-tit.vl-desp-banc + mov-tit.vl-desp-financ)).

                run pi-criar-tt-conta-contabil (Input c-conta-despes,
                                                Input 2,
                                                input (mov-tit.vl-desp-banc + mov-tit.vl-desp-financ)).
            end.

        end.
    end.

    /*--- Transa‡Æo 16 - Corr Monet Integral ---*/ 
    if  mov-tit.transacao = 16 then do:
        if  mov-tit.acerto-cmi <> 0 then do:
            run pi-criar-tt-conta-contabil (Input c-conta-1,
                                            Input if mov-tit.acerto-cmi > 0 then 2 else 1,
                                            input mov-tit.acerto-cmi).
            if  avail ext-cmi then
                run pi-criar-tt-conta-contabil (Input ext-cmi.conta-gp-cmcac,
                                                Input if mov-tit.acerto-cmi > 0 then 1 else 2,
                                                input mov-tit.acerto-cmi).
        end.
        if  mov-tit.ganho-perda <> 0 then do:
            run pi-criar-tt-conta-contabil (Input c-conta-1,
                                            Input if mov-tit.ganho-perda > 0 then 1 else 2,
                                            input mov-tit.ganho-perda).
            if  avail ext-cmi then
                run pi-criar-tt-conta-contabil (Input ext-cmi.conta-gp-presente,
                                                Input if mov-tit.ganho-perda > 0 then 2 else 1,
                                                input mov-tit.ganho-perda).
        end.
    end.

    /*--- Transa‡Æo 15 - Ganhos/Perdas FASB ---*/ 
    if  mov-tit.transacao = 15 then do:
        if  avail mov-tit then do:
            if  (mov-tit.vl-original / 100) > 1 then do:
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                Input 2,
                                                input mov-tit.vl-original).
                run pi-criar-tt-conta-contabil (Input c-conta-2,
                                                Input 1,
                                                input mov-tit.vl-original).
            end.

            if  (mov-tit.vl-original / 100) < 1 then do:
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                Input 1,
                                                input mov-tit.vl-original).
                run pi-criar-tt-conta-contabil (Input c-conta-2,
                                                Input 2,
                                                input mov-tit.vl-original).
            end.

            if  (mov-tit.vl-juros-rec / 100) - 
                (mov-tit.vl-original / 100) > 0 then do:
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                Input 2,
                                                input mov-tit.vl-juros-rec).
                run pi-criar-tt-conta-contabil (Input c-conta-1,
                                                Input 1,
                                                input mov-tit.vl-juros-rec).
            end.
            if  (mov-tit.vl-juros-rec / 100) -
                (mov-tit.vl-original / 100) < 0 then do:
                run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                                Input 1,
                                                input mov-tit.vl-juros-rec).
                run pi-criar-tt-conta-contabil (Input c-conta-1,
                                                Input 2,
                                                input mov-tit.vl-juros-rec).
            end.
        end.
    end.

    /*--- Transa‡äes 19 e 20 - Varia‡Æo Cambial ---*/ 
    if  mov-tit.transacao = 19 or mov-tit.transacao = 20 then do:
        run pi-criar-tt-conta-contabil (Input conta-cr.conta-saldo,
                                        Input if mov-tit.transacao = 19 then 1 else 2,
                                        input mov-tit.vl-baixa).
        run pi-criar-tt-conta-contabil (Input conta-cr.conta-var-monetaria,
                                        Input if mov-tit.transacao = 19 then 2 else 1,
                                        input mov-tit.vl-baixa).
    end.

end.

PROCEDURE pi-criar-tt-conta-contabil :

    define input parameter p-conta-contabil like conta-contab.conta-contabil no-undo.
    define input parameter p-natureza       as integer                       no-undo.
    define input parameter p-valor          like titulo.vl-original          no-undo.

    define var c-titulo-conta like conta-contab.titulo no-undo.

    find first tt-conta-contabil
         where tt-conta-contabil.tt-cod-conta-contabil = p-conta-contabil
            &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
                DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
                &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
                    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
                    ASSIGN cAuxTraducao001 = {adinc/i01ad042.i 04 p-natureza}.
                    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                                        INPUT "",
                                        INPUT "").
                    ASSIGN  cAuxTraducao002 = RETURN-VALUE.
                &else
                    ASSIGN cAuxTraducao002 = {adinc/i01ad042.i 04 p-natureza}.
                &endif
                run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao002)," ","_"),
                                    INPUT "",
                                    INPUT "").
                and   tt-conta-contabil.tt-natureza           = RETURN-VALUE
            &else
                &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
                    DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
                    ASSIGN cAuxTraducao002 = {adinc/i01ad042.i 04 p-natureza}.
                    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao002)," ","_"),
                                        INPUT "",
                                        INPUT "").
                    and   tt-conta-contabil.tt-natureza           = RETURN-VALUE
                &else
                    and   tt-conta-contabil.tt-natureza           = {adinc/i01ad042.i 04 p-natureza}
                &endif
            &endif
         no-error.

    if  not avail tt-conta-contabil then do:

        find first conta-contab
             where conta-contab.ep-codigo    = mov-tit.ep-codigo
             and conta-contab.conta-contabil = p-conta-contabil
             no-lock no-error.

        if avail conta-contab then do:
           assign c-titulo-conta = conta-contab.titulo.
        end.
        else do:
           {utp/ut-liter.i Conta_Cont bil_nÆo_cadastrada MCR L}
           assign c-titulo-conta = trim(return-value).
        end.

        create tt-conta-contabil.
        assign tt-conta-contabil.tt-cod-conta-contabil = p-conta-contabil
               tt-conta-contabil.tt-titulo-conta       = c-titulo-conta
        .
        &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
            DEFINE VARIABLE cAuxTraducao003 AS CHARACTER NO-UNDO.
            ASSIGN cAuxTraducao003 = {adinc/i01ad042.i 04 p-natureza}.
            run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao003)," ","_"),
                                INPUT "",
                                INPUT "").
            ASSIGN tt-conta-contabil.tt-natureza           = RETURN-VALUE.
        &else
            ASSIGN tt-conta-contabil.tt-natureza           = {adinc/i01ad042.i 04 p-natureza}.
        &endif
ASSIGN                tt-conta-contabil.tt-valor              = p-valor.

    end.
    else do:
        assign tt-conta-contabil.tt-valor = tt-conta-contabil.tt-valor + p-valor.
    end.

END PROCEDURE.

Procedure pi-valida-cobranca-centralizada:

    if  l-usa-cobranca and mov-tit.transacao = 14 and
      &if defined(BF_FIN_COBRANCA_CENTR) &then
        mov-tit.log-port-centr
      &else
        mov-tit.baixa-conf
      &endif
        then do:
        if l-planta-centr then
            if l-usa-trans then 
           &if defined(BF_FIN_COBRANCA_CENTR) &then
               assign c-conta-db = conta-cr.conta-trans-cobr
                      c-conta-cr = mov-tit.conta-credito.
           &else
               assign c-conta-db = substring(conta-cr.char-2,1,17)
                      c-conta-cr = mov-tit.conta-credito.
           &endif
            else 
               assign c-conta-db = conta-cr.conta-saldo
                      c-conta-cr = mov-tit.conta-credito.
        else
            if l-usa-trans then 
           &if defined(BF_FIN_COBRANCA_CENTR) &then
               assign c-conta-db = conta-cr.conta-saldo
                      c-conta-cr = conta-cr.conta-trans-cobr.
           &else
               assign c-conta-db = conta-cr.conta-saldo
                      c-conta-cr = substring(conta-cr.char-2,1,17).
           &endif
            else 
               assign c-conta-db = conta-cr.conta-saldo
                      c-conta-cr = conta-cr.conta-saldo.
    end.

END PROCEDURE.


PROCEDURE pi-estorno-ava-devolucao-cheque:

    find first estabelec
         where estabelec.cod-estabel = mov-tit.cod-estabel no-lock no-error.

    if  not avail estabelec then
        return 'NOK'.

    find first portador
         where portador.ep-codigo    = mov-tit.ep-codigo
         and   portador.cod-portador = mov-tit.cod-portador
         and   portador.modalidade   = mov-tit.modalidade
         no-lock  no-error.

    if  not avail portador then
        return 'NOK'.

    if  portador.bancario = yes then do:
        find first cta-corrente
             where cta-corrente.cod-banco    = portador.cod-banco
             and   cta-corrente.agencia      = portador.agencia
             and   cta-corrente.conta-corren = portador.conta-corren
             no-lock no-error.

        if  not avail cta-corrente then
            return 'NOK'.
    end.

    find first emitente
         where emitente.cod-emitente = mov-tit.cod-emitente
         and   emitente.identific   <> 2 no-lock no-error.

    if  not avail emitente then
        return 'NOK'.

    find first conta-cr use-index codigo
         where conta-cr.ep-codigo   = mov-tit.ep-codigo
         and   conta-cr.cod-estabel = mov-tit.cod-estabel
         and   conta-cr.cod-esp     = mov-tit.cod-esp
         and   conta-cr.cod-gr-cli  = emitente.cod-gr-cli
         no-lock no-error.

    if  not avail conta-cr then
        return 'NOK'.

    if  mov-tit.vl-desp-banc > 0 then do:
        run pi-criar-tt-conta-contabil (Input estabelec.conta-despesas,
                                        Input 2,
                                        input mov-tit.vl-desp-banc).
        run pi-criar-tt-conta-contabil (Input if portador.bancario 
                                              then cta-corrente.conta-despes-banc 
                                              else portador.conta-contabil,
                                        Input 1,
                                        input mov-tit.vl-desp-banc).
    end.

    if  mov-tit.vl-juros-rec > 0 then do:
        run pi-criar-tt-conta-contabil (Input if portador.bancario = yes 
                                              then cta-corrente.conta-juros-recebidos
                                              else portador.conta-contabil,
                                        Input 2,
                                        input mov-tit.vl-juros-rec).
        run pi-criar-tt-conta-contabil (Input conta-cr.conta-juros,
                                        Input 1,
                                        input mov-tit.vl-juros-rec).
    end.

    if  mov-tit.vl-multa > 0 then do:
        run pi-criar-tt-conta-contabil (Input if portador.bancario = yes
                                              then cta-corrente.conta-saldo
                                              else portador.conta-contabil,
                                        Input 2,
                                        input mov-tit.vl-multa).
        run pi-criar-tt-conta-contabil (Input conta-cr.conta-multa,
                                        Input 1,
                                        input mov-tit.vl-multa).
    end.

    if  mov-tit.vl-desconto > 0 then do:
        run pi-criar-tt-conta-contabil (Input if portador.bancario = yes 
                                              then cta-corrente.conta-saldo
                                              else portador.conta-contabil,
                                        Input 1,
                                        input mov-tit.vl-desconto).
        run pi-criar-tt-conta-contabil (Input conta-cr.conta-desconto,
                                        Input 2,
                                        input mov-tit.vl-desconto).
    end.

    if  mov-tit.vl-abatimen > 0 then do:
        run pi-criar-tt-conta-contabil (Input if portador.bancario = yes 
                                              then cta-corrente.conta-saldo
                                              else portador.conta-contabil,
                                        Input 1,
                                        input mov-tit.vl-abatimen).
        run pi-criar-tt-conta-contabil (Input conta-cr.conta-abatimento,
                                        Input 2,
                                        input mov-tit.vl-abatimen).
    end.

END PROCEDURE.


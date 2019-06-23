/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda   for ems2cadme.moeda.
def buffer cotacao for ems2cadme.cotacao.

{include/i-prgvrs.i APAPI007 2.00.00.016}  /*** 010016 ***/
/******************************************************************************
**
**    Programa: APAPI007.P
**
**    Objetivo: C lculo dos saldos anterior e em aberto.
**
*******************************************************************************/

/* Fator Mult/Div moeda - Portugal */
{cdp/cdcfgfin.i}
{cdp/cd1234.i2}

/* Definicao TTs e Variaveis - Validacao Decimais - Chile */
{cdp/cd1234.i}

{utp/ut-glob.i}    
{app/apapi007.i}
{app/apapi007.i1}
{app/ap9999.i}
    
def input-output parameter table for tt-param-filtro.
def input-output parameter table for tt-tit-ap.
def output       parameter table for tt-erro.
    
def buffer b-tt-tit-ap for tt-tit-ap.
    
def var h-acomp          as handle                           no-undo.
def var de-tot-aberto    as decimal format "->>>,>>>,>>9.99" no-undo.
def var de-tot-anterior  as decimal format "->>>,>>>,>>9.99" no-undo.
def var i-versao-integ   as integer                          no-undo.
def var c-mensagem       as char    format "x(60)"           no-undo.
    
find first tt-param-filtro exclusive-lock no-error.
        
assign tt-param-filtro.saldo-aberto   = 0
       tt-param-filtro.saldo-anterior = 0.
        
assign i-versao-integ = 001. 

/*----- Testa VersÆo Integra‡Æo ------*/
if  tt-param-filtro.cod-versao-integ <> i-versao-integ then do:

    if  tt-param-filtro.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 3941,
                           input "").
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
    return error.
end.                    


find first emitente
     where emitente.cod-emit  = tt-param-filtro.i-cod-fornec
     and   emitente.identific <> 1 no-lock no-error.
     
if  not avail emitente then do:
    if  tt-param-filtro.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 627,
                           input "").
    end.
    else do:
        run utp/ut-msgs.p (input "msg",
                           input 627,
                           input "").
       
        assign c-mensagem = return-value.
        
        create tt-erro.
        assign tt-erro.i-cod-erro  = 627
               tt-erro.c-desc-erro = c-mensagem.
    end.
    return error.
end.

if  tt-param-filtro.c-esp-ini >
    tt-param-filtro.c-esp-fim then do:
    if  tt-param-filtro.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 3627,
                           input "").
    end.
    else do:
        run utp/ut-msgs.p (input "msg",
                           input 3627,
                           input "").
       
        assign c-mensagem = return-value.
        
        create tt-erro.
        assign tt-erro.i-cod-erro  = 3627
               tt-erro.c-desc-erro = c-mensagem.
    end.
    return error.
end.

if  tt-param-filtro.dt-trans-ini >
    tt-param-filtro.dt-trans-fim then do:
    if  tt-param-filtro.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 184,
                           input "").
    end.
    else do:
        run utp/ut-msgs.p (input "msg",
                           input 184,
                           input "").
       
        assign c-mensagem = return-value.
        
        create tt-erro.
        assign tt-erro.i-cod-erro  = 184
               tt-erro.c-desc-erro = c-mensagem.
    end.
    return error.
end.

find param-ap 
     where param-ap.ep-codigo = tt-param-filtro.i-empresa no-lock no-error.
if  not avail param-ap then do:
    if  tt-param-filtro.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 8759,
                           input "").
    end.
    else do:
        run utp/ut-msgs.p (input "msg",
                           input 8759,
                           input "").
   
        assign c-mensagem = return-value.
      
        create tt-erro.
        assign tt-erro.i-cod-erro  = 8759
               tt-erro.c-desc-erro = c-mensagem.
    end.         
    return error.
end.

if  tt-param-filtro.l-ant-aber = no then do: /* Calcula saldo aberto */
            
    if  tt-param-filtro.l-acompanha = yes then do:
        run utp/ut-acomp.p persistent set h-acomp.
        {utp/ut-liter.i Calculando_Saldo_em_Aberto,_Aguarde...}
        run pi-inicializar in h-acomp (input trim(return-value)).
    end.
 
    if  avail emitente then do on stop undo, leave:
        if  tt-param-filtro.l-matriz = no then do:
            for each tit-ap use-index ch-liq
                where tit-ap.ep-codigo    = tt-param-filtro.i-empresa
                and   tit-ap.cod-fornec   = emitente.cod-emit 
                and ((tit-ap.cod-estabel  = tt-param-filtro.c-cod-est and tt-param-filtro.l-faixa = no) 
                or   (tit-ap.cod-estabel >= tt-param-filtro.c-cod-est-ini
                and   tit-ap.cod-estabel <= tt-param-filtro.c-cod-est-fim and tt-param-filtro.l-faixa = yes))
                and   tit-ap.cod-esp     >= tt-param-filtro.c-esp-ini
                and   tit-ap.cod-esp     <= tt-param-filtro.c-esp-fim
                and   tit-ap.portador    >= tt-param-filtro.i-port-ini
                and   tit-ap.portador    <= tt-param-filtro.i-port-fim
                no-lock:
    
                {cdp/cd9600.i "tt-param-filtro.i-moeda"
                              "tt-param-filtro.da-conversao"
                              "de-calc-cotacao"
                              "return error."}
                            
                {app/ap9999.i1 "tit-ap"
                               "tt-param-filtro.i-moeda"}
    
                if  tt-param-filtro.l-acompanha = yes then do:
                    {utp/ut-liter.i Docto/P:}
                    run pi-acompanhar in h-acomp (input trim(return-value)
                                                  + "  "
                                                  + tit-ap.nr-docto 
                                                  + "/" 
                                                  + tit-ap.parcela
                                                  + " - "
                                                  + string(tit-ap.dt-transacao, "99/99/9999")).
                end.
                
                if  tit-ap.tipo <> 1 
                and tit-ap.tipo <> 2 
                and tit-ap.tipo <> 3 
                and tit-ap.tipo <> 8 then
                    next.
                            
                if  tit-ap.dt-transacao > tt-param-filtro.dt-trans-fim then
                    next.
                        
                run pi-calculo-aberto.
                
            end.
        end.
        else do:
            for each tit-ap use-index matriz
                where tit-ap.ep-codigo    = tt-param-filtro.i-empresa
                and   tit-ap.matriz       = emitente.cod-emit 
                and ((tit-ap.cod-estabel  = tt-param-filtro.c-cod-est and tt-param-filtro.l-faixa = no) 
                or   (tit-ap.cod-estabel >= tt-param-filtro.c-cod-est-ini
                and   tit-ap.cod-estabel <= tt-param-filtro.c-cod-est-fim and tt-param-filtro.l-faixa = yes))
                and   tit-ap.cod-esp     >= tt-param-filtro.c-esp-ini
                and   tit-ap.cod-esp     <= tt-param-filtro.c-esp-fim
                and   tit-ap.portador    >= tt-param-filtro.i-port-ini
                and   tit-ap.portador    <= tt-param-filtro.i-port-fim
                no-lock:
    
                {cdp/cd9600.i "tt-param-filtro.i-moeda"
                              "tt-param-filtro.da-conversao"
                              "de-calc-cotacao"
                              "return error."}
                            
                {app/ap9999.i1 "tit-ap"
                               "tt-param-filtro.i-moeda"}
    
                if  tt-param-filtro.l-acompanha = yes then do:
                    {utp/ut-liter.i Docto/P:}
                    run pi-acompanhar in h-acomp (input trim(return-value)
                                                  + "  "
                                                  + tit-ap.nr-docto 
                                                  + "/" 
                                                  + tit-ap.parcela
                                                  + " - "
                                                  + string(tit-ap.dt-transacao, "99/99/9999")).
                end.
                
                if  tit-ap.tipo <> 1 
                and tit-ap.tipo <> 2 
                and tit-ap.tipo <> 3 
                and tit-ap.tipo <> 8 then
                    next.
                            
                if  tit-ap.dt-transacao > tt-param-filtro.dt-trans-fim then
                    next.
                        
                run pi-calculo-aberto.
                
            end.        
        end.
    end.
    
    if  tt-param-filtro.l-acompanha = yes then            
        run pi-finalizar in h-acomp.
    
end.
else do: /* Calcula saldo anterior */
    
    assign de-tot-anterior = 0.
    
    if  tt-param-filtro.l-acompanha = yes then do:
        run utp/ut-acomp.p persistent set h-acomp.
        {utp/ut-liter.i Calculando_Saldo_Anterior,_Aguarde...}
        run pi-inicializar in h-acomp (input trim(return-value)).
    end.
                      
    do  on stop undo, leave:
        if  avail emitente then do:
            if tt-param-filtro.l-matriz = no then do:
                for each  tit-ap use-index ch-liq 
                    where tit-ap.ep-codigo    = tt-param-filtro.i-empresa
                    and   tit-ap.cod-fornec   = emitente.cod-emit 
                    and ((tit-ap.cod-estabel  = tt-param-filtro.c-cod-est     and tt-param-filtro.l-faixa = no) 
                    or   (tit-ap.cod-estabel >= tt-param-filtro.c-cod-est-ini
                    and   tit-ap.cod-estabel <= tt-param-filtro.c-cod-est-fim and tt-param-filtro.l-faixa = yes))
                    and   tit-ap.cod-esp     >= tt-param-filtro.c-esp-ini
                    and   tit-ap.cod-esp     <= tt-param-filtro.c-esp-fim
                    and   tit-ap.portador    >= tt-param-filtro.i-port-ini
                    and   tit-ap.portador    <= tt-param-filtro.i-port-fim
                    no-lock:
                    {app/apapi007.i2}
                end.             
            end.
            else do:                    
                for each  tit-ap use-index matriz
                    where tit-ap.ep-codigo    = tt-param-filtro.i-empresa
                    and   tit-ap.matriz       = emitente.cod-emit 
                    and ((tit-ap.cod-estabel  = tt-param-filtro.c-cod-est     and tt-param-filtro.l-faixa = no) 
                    or   (tit-ap.cod-estabel >= tt-param-filtro.c-cod-est-ini
                    and   tit-ap.cod-estabel <= tt-param-filtro.c-cod-est-fim and tt-param-filtro.l-faixa = yes))
                    and   tit-ap.cod-esp     >= tt-param-filtro.c-esp-ini
                    and   tit-ap.cod-esp     <= tt-param-filtro.c-esp-fim
                    and   tit-ap.portador    >= tt-param-filtro.i-port-ini
                    and   tit-ap.portador    <= tt-param-filtro.i-port-fim
                    no-lock:
                
                    {app/apapi007.i2}
                end. 
            end.    
        end.
    end.
        
    assign tt-param-filtro.saldo-anterior = tt-param-filtro.saldo-anterior
                                          + de-tot-anterior.
                    
    if  tt-param-filtro.l-acompanha = yes then   
        run pi-finalizar in h-acomp.
        
end.    
    
PROCEDURE pi-calculo-aberto:
                    
    assign de-tot-aberto = 0.
            
    if   tit-ap.dt-transacao <= tt-param-filtro.dt-trans-fim then do:
                
        if  tit-ap.tipo = 1 
        or  tit-ap.tipo = 8 then do:
                        
            assign de-tot-aberto = de-tot-aberto + de-calc-valor-saldo.        
                
            for each  mov-ap use-index codigo
                where mov-ap.ep-codigo     = tit-ap.ep-codigo
                and   mov-ap.cod-est       = tit-ap.cod-est
                and   mov-ap.cod-esp       = tit-ap.cod-esp
                and   mov-ap.nr-docto      = tit-ap.nr-docto
                and   mov-ap.serie         = tit-ap.serie
                and   mov-ap.parcela       = tit-ap.parcela
                and   mov-ap.cod-fornec    = tit-ap.cod-fornec
                and   mov-ap.dt-transacao  > tt-param-filtro.dt-trans-fim
                and  (mov-ap.transacao     = 2 
                or    mov-ap.transacao     = 5
                or    mov-ap.transacao     = 3
                or   (mov-ap.transacao     = 6 and mov-ap.lancamento = 1))
                no-lock:
                        
                {app/ap9999.i2 "mov-ap"
                               "tt-param-filtro.i-moeda"
                               "tit-ap"}
                    
                if  mov-ap.transacao  = 5 
                and mov-ap.lancamento = 1 then 
                    assign de-tot-aberto = de-tot-aberto - de-calc-valor-mov.
                else
                    assign de-tot-aberto = de-tot-aberto + de-calc-valor-mov.
                    
            end.
        end.
        
        if  tit-ap.tipo = 2 then do:
                
            assign de-tot-aberto = de-tot-aberto - de-calc-valor-saldo.
                        
            for each  mov-ap use-index ch-antecip
                where mov-ap.ep-codigo     = tit-ap.ep-codigo
                and   mov-ap.cod-est       = tit-ap.cod-est
                and   mov-ap.esp-ant       = tit-ap.cod-esp
                and   mov-ap.serie-ant     = tit-ap.serie
                and   mov-ap.docto-ant     = tit-ap.nr-docto
                and   mov-ap.parc-ant      = tit-ap.parcela
                and   mov-ap.fornec-ant    = tit-ap.cod-fornec
                and   mov-ap.dt-transacao  > tt-param-filtro.dt-trans-fim
                and  (mov-ap.transacao     = 2
                or    mov-ap.transacao     = 3)
                no-lock:
                        
                {app/ap9999.i2 "mov-ap"
                               "tt-param-filtro.i-moeda"
                               "tit-ap"}
                            
                assign de-tot-aberto = de-tot-aberto - de-calc-vl-antecip.
                    
            end.
            
            for each  mov-ap use-index codigo
                where mov-ap.ep-codigo     = tit-ap.ep-codigo
                and   mov-ap.cod-est       = tit-ap.cod-est
                and   mov-ap.cod-esp       = tit-ap.cod-esp
                and   mov-ap.serie         = tit-ap.serie
                and   mov-ap.nr-docto      = tit-ap.nr-docto
                and   mov-ap.parcela       = tit-ap.parcela
                and   mov-ap.cod-fornec    = tit-ap.cod-fornec
                and   mov-ap.dt-transacao  > tt-param-filtro.dt-trans-fim
                and   mov-ap.transacao     = 5
                no-lock:
                
                {app/ap9999.i2 "mov-ap"
                               "tt-param-filtro.i-moeda"
                               "tit-ap"}
                               
                if  mov-ap.transacao = 5 then
                    if  mov-ap.lancamento = 1 then 
                        assign de-tot-aberto = de-tot-aberto - de-calc-valor-mov.
                    else
                        assign de-tot-aberto = de-tot-aberto + de-calc-valor-mov.
    
            end.
        end.
    end.
        
    if  tit-ap.dt-transacao >= tt-param-filtro.dt-trans-ini
    and tit-ap.dt-transacao <= tt-param-filtro.dt-trans-fim
    and tit-ap.tipo          = 3 then do:
        assign de-tot-aberto = de-tot-aberto + de-calc-valor-saldo.
    end.
            
    assign tt-param-filtro.saldo-aberto = tt-param-filtro.saldo-aberto
                                        + de-tot-aberto.
            
END PROCEDURE.

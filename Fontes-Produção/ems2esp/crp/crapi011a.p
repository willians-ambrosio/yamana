/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CRAPI011A 2.00.00.003}  /*** 010003 ***/
/******************************************************************************
**
**    Programa: CRAPI011.P
**    Objetivo: API c lculo estat¡stica dos clientes.
**
*******************************************************************************/

{crp/crapi011a.i}

def buffer b-emitente for emitente.

def input  parameter table for tt-dados.
def output parameter table for tt-estatistica-a.
def output parameter table for tt-erro.

def var da-ini-est     like emit-estat.data                               no-undo.
def var da-fim-est     like emit-estat.data                               no-undo.
def var da-data-sel    as date  init today                                no-undo.
def var i-versao-integ as int                                             no-undo.
def var i-cont         as integer                                         no-undo.
def var i-atraso       as integer   format "->>9"               extent 13 no-undo.
def var i-recebimento  as integer   format "->>9"               extent 13 no-undo.
def var i-tipo         as integer   no-undo.

DEF VAR de-valor-rec   AS DEC EXTENT 12 NO-UNDO.
DEF VAR de-vl-tot-rec  AS DEC EXTENT 12 NO-UNDO.
DEF VAR de-vl-pr-rec   AS DEC EXTENT 12 NO-UNDO.
DEF VAR de-dec-1       AS DEC EXTENT 12 NO-UNDO.

def var c-per-atraso      as character format "99/9999"         extent 12 no-undo.
def var c-mensagem        as char      format "x(60)"                     no-undo.
def var c-matriz          like emitente.nome-matriz                       no-undo.
def var i-grupo-centraliz like emitente.cod-emitente                      no-undo.

def var de-vendas      as decimal   format "->>,>>>,>>>,>>9.99" extent 13 no-undo.
def var de-atraso-acum as decimal   format "->>,>>>,>>>,>>9.99"           no-undo.
def var de-atraso-a    as decimal   format "->>,>>>,>>>,>>9.99"           no-undo.
def var de-prazo-acum  as decimal   format "->>,>>>,>>>,>>9.99"           no-undo.
def var de-atraso-pmr  like emit-estat.valor-rec.
def var h-acomp        as handle no-undo.
def var i-tit          as int    no-undo.

{cdp/cdcfgfin.i}
{include/i_dbvers.i}

assign i-versao-integ = 001.

find first tt-dados exclusive-lock.

/*----- Testa VersÆo Integra‡Æo ------*/
if  tt-dados.cod-versao-integ <> i-versao-integ then do:
    if  tt-dados.l-vid-rel = yes then do:
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
     where emitente.cod-emit = tt-dados.i-cod-emit no-lock no-error.

if  not avail emitente then do:
    if  tt-dados.l-vid-rel = yes then do:
        run utp/ut-msgs.p (input "show",
                           input 8144,
                           input "").
    end.
    else do: 
        run utp/ut-msgs.p (input "msg",
                           input 8144,
                           input "").
        assign c-mensagem = return-value.

        create tt-erro.
        assign tt-erro.i-cod-erro     = 8144
               tt-erro.c-desc-erro    = c-mensagem.
    end.
    return error.
end.
else
    assign c-matriz = emitente.nome-matriz.
    
for each tt-estatistica-a exclusive-lock:
    delete tt-estatistica-a.
end.

run utp/ut-acomp.p persistent set h-acomp.     
run pi-inicializar in h-acomp (input "Processando, Aguarde...").
       
create tt-estatistica-a.
assign tt-estatistica-a.fi-vendas-acumuladas = 0 
       tt-estatistica-a.fi-saldo-aberto      = 0
       tt-estatistica-a.fi-vendas            = 0
       tt-estatistica-a.fi-atm-media         = 0
       tt-estatistica-a.fi-pmr-media         = 0
       tt-estatistica-a.fi-vendas-media      = 0.
     
assign tt-estatistica-a.fi-atm-1  = 0  
       tt-estatistica-a.fi-atm-2  = 0  
       tt-estatistica-a.fi-atm-3  = 0  
       tt-estatistica-a.fi-atm-4  = 0
       tt-estatistica-a.fi-atm-5  = 0  
       tt-estatistica-a.fi-atm-6  = 0  
       tt-estatistica-a.fi-atm-7  = 0  
       tt-estatistica-a.fi-atm-8  = 0
       tt-estatistica-a.fi-atm-9  = 0  
       tt-estatistica-a.fi-atm-10 = 0  
       tt-estatistica-a.fi-atm-11 = 0  
       tt-estatistica-a.fi-atm-12 = 0.
     
assign tt-estatistica-a.fi-pmr-1  = 0  
       tt-estatistica-a.fi-pmr-2  = 0  
       tt-estatistica-a.fi-pmr-3  = 0  
       tt-estatistica-a.fi-pmr-4  = 0
       tt-estatistica-a.fi-pmr-5  = 0  
       tt-estatistica-a.fi-pmr-6  = 0  
       tt-estatistica-a.fi-pmr-7  = 0  
       tt-estatistica-a.fi-pmr-8  = 0
       tt-estatistica-a.fi-pmr-9  = 0  
       tt-estatistica-a.fi-pmr-10 = 0  
       tt-estatistica-a.fi-pmr-11 = 0  
       tt-estatistica-a.fi-pmr-12 = 0.
   
assign tt-estatistica-a.fi-vendas-1  = 0  
       tt-estatistica-a.fi-vendas-2  = 0  
       tt-estatistica-a.fi-vendas-3  = 0  
       tt-estatistica-a.fi-vendas-4  = 0
       tt-estatistica-a.fi-vendas-5  = 0  
       tt-estatistica-a.fi-vendas-6  = 0  
       tt-estatistica-a.fi-vendas-7  = 0  
       tt-estatistica-a.fi-vendas-8  = 0
       tt-estatistica-a.fi-vendas-9  = 0  
       tt-estatistica-a.fi-vendas-10 = 0  
       tt-estatistica-a.fi-vendas-11 = 0  
       tt-estatistica-a.fi-vendas-12 = 0.

assign c-per-atraso[1]  = ""  
       c-per-atraso[2]  = ""  
       c-per-atraso[3]  = ""  
       c-per-atraso[4]  = ""
       c-per-atraso[5]  = ""  
       c-per-atraso[6]  = ""  
       c-per-atraso[7]  = ""  
       c-per-atraso[8]  = ""
       c-per-atraso[9]  = ""  
       c-per-atraso[10] = ""  
       c-per-atraso[11] = ""  
       c-per-atraso[12] = "".
  
assign i-atraso[1]  = 0   
       i-atraso[2]  = 0   
       i-atraso[3]  = 0  
       i-atraso[4]  = 0
       i-atraso[5]  = 0   
       i-atraso[6]  = 0   
       i-atraso[7]  = 0  
       i-atraso[8]  = 0
       i-atraso[9]  = 0   
       i-atraso[10] = 0   
       i-atraso[11] = 0  
       i-atraso[12] = 0
       i-atraso[13] = 0.
     
assign i-recebimento[1]  = 0   
       i-recebimento[2]  = 0  
       i-recebimento[3]  = 0
       i-recebimento[4]  = 0   
       i-recebimento[5]  = 0  
       i-recebimento[6]  = 0
       i-recebimento[7]  = 0   
       i-recebimento[8]  = 0  
       i-recebimento[9]  = 0
       i-recebimento[10] = 0   
       i-recebimento[11] = 0  
       i-recebimento[12] = 0
       i-recebimento[13] = 0.
     
assign de-vendas[1]  = 0   
       de-vendas[2]  = 0   
       de-vendas[3]  = 0  
       de-vendas[4]  = 0
       de-vendas[5]  = 0   
       de-vendas[6]  = 0   
       de-vendas[7]  = 0  
       de-vendas[8]  = 0
       de-vendas[9]  = 0   
       de-vendas[10] = 0   
       de-vendas[11] = 0  
       de-vendas[12] = 0
       de-vendas[13] = 0.

assign da-fim-est = date(int(substr(tt-dados.fi-periodo,1,2)),1,int(substr(tt-dados.fi-periodo,3,4)))
       da-ini-est = date(month(da-fim-est),1,int(year(da-fim-est)) - 1).  
&if  "{&mgadm_version}" < "2.02" &then 
    if tt-dados.tipo-consulta = 1 then do:
        {crp/crapi011.i1}
    end.
    else do:
        {crp/crapi011.i2} 
    end.
&else
    case tt-dados.tipo-consulta:
        when 1 then do:
            {crp/crapi011.i1}
        end.
        when 2 then do:
            DO i-cont = 1 to 12:
               ASSIGN de-valor-rec[i-cont]  = 0
                      de-vl-tot-rec[i-cont] = 0
                      de-vl-pr-rec[i-cont]  = 0
                      de-dec-1[i-cont]      = 0.
            END.
            {crp/crapi011.i2}
        end.
        when 3 then do:
            assign i-grupo-centraliz =  tt-dados.i-cod-emit.
            {crp/crapi011.i3}        
        end.
    end case.
&endif

assign tt-estatistica-a.fi-periodo-1  = c-per-atraso[1]
       tt-estatistica-a.fi-periodo-2  = c-per-atraso[2]    
       tt-estatistica-a.fi-periodo-3  = c-per-atraso[3]
       tt-estatistica-a.fi-periodo-4  = c-per-atraso[4]
       tt-estatistica-a.fi-periodo-5  = c-per-atraso[5]
       tt-estatistica-a.fi-periodo-6  = c-per-atraso[6]
       tt-estatistica-a.fi-periodo-7  = c-per-atraso[7]
       tt-estatistica-a.fi-periodo-8  = c-per-atraso[8]
       tt-estatistica-a.fi-periodo-9  = c-per-atraso[9]
       tt-estatistica-a.fi-periodo-10 = c-per-atraso[10]
       tt-estatistica-a.fi-periodo-11 = c-per-atraso[11]
       tt-estatistica-a.fi-periodo-12 = c-per-atraso[12].

assign tt-estatistica-a.fi-atm-1  = i-atraso[1]
       tt-estatistica-a.fi-atm-2  = i-atraso[2]
       tt-estatistica-a.fi-atm-3  = i-atraso[3]
       tt-estatistica-a.fi-atm-4  = i-atraso[4]
       tt-estatistica-a.fi-atm-5  = i-atraso[5]
       tt-estatistica-a.fi-atm-6  = i-atraso[6]
       tt-estatistica-a.fi-atm-7  = i-atraso[7]
       tt-estatistica-a.fi-atm-8  = i-atraso[8]
       tt-estatistica-a.fi-atm-9  = i-atraso[9]
       tt-estatistica-a.fi-atm-10 = i-atraso[10]
       tt-estatistica-a.fi-atm-11 = i-atraso[11]
       tt-estatistica-a.fi-atm-12 = i-atraso[12].

assign tt-estatistica-a.fi-pmr-1  = i-recebimento[1]
       tt-estatistica-a.fi-pmr-2  = i-recebimento[2]
       tt-estatistica-a.fi-pmr-3  = i-recebimento[3]
       tt-estatistica-a.fi-pmr-4  = i-recebimento[4]
       tt-estatistica-a.fi-pmr-5  = i-recebimento[5]
       tt-estatistica-a.fi-pmr-6  = i-recebimento[6]
       tt-estatistica-a.fi-pmr-7  = i-recebimento[7]
       tt-estatistica-a.fi-pmr-8  = i-recebimento[8]
       tt-estatistica-a.fi-pmr-9  = i-recebimento[9]
       tt-estatistica-a.fi-pmr-10 = i-recebimento[10]
       tt-estatistica-a.fi-pmr-11 = i-recebimento[11]
       tt-estatistica-a.fi-pmr-12 = i-recebimento[12].

assign tt-estatistica-a.fi-vendas-1  = de-vendas[1]
       tt-estatistica-a.fi-vendas-2  = de-vendas[2]
       tt-estatistica-a.fi-vendas-3  = de-vendas[3]
       tt-estatistica-a.fi-vendas-4  = de-vendas[4]
       tt-estatistica-a.fi-vendas-5  = de-vendas[5]
       tt-estatistica-a.fi-vendas-6  = de-vendas[6]
       tt-estatistica-a.fi-vendas-7  = de-vendas[7]
       tt-estatistica-a.fi-vendas-8  = de-vendas[8]
       tt-estatistica-a.fi-vendas-9  = de-vendas[9]
       tt-estatistica-a.fi-vendas-10 = de-vendas[10]
       tt-estatistica-a.fi-vendas-11 = de-vendas[11]
       tt-estatistica-a.fi-vendas-12 = de-vendas[12].
       
assign tt-estatistica-a.fi-atm-media    = if de-atraso-a <> 0 
                                        then de-atraso-acum / de-atraso-a
                                        else 0.

assign tt-estatistica-a.fi-pmr-media    = if de-atraso-pmr <> 0 
                                        then de-prazo-acum  / de-atraso-pmr
                                        else 0.
                                        
assign tt-estatistica-a.fi-vendas-media = de-vendas[13] / 12.

run pi-finalizar in h-acomp.

return 'OK'.
  

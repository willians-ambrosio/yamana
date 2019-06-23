/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCC0406RP 2.00.00.023 } /*** 010023 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i ESCC0406rp MCC}
&ENDIF

{include/i_fnctrad.i}
/*******************************************************************************
**
**       Programa: ESCC0406.P
**
**       Data....: Mar‡o de 1997
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Pedidos Emitidos
**
**       Versao..: 1.00.000 - Sandra Stadelhofer
**
*******************************************************************************/
{cdp/cdcfgmat.i}
/* Variaveis e temp-tables comuns */
{ccp/ESCC0406.i5 new}

def temp-table tt-raw-digita
    field raw-digita      as raw.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

{include/i-rpvar.i} 

create tt-param.
raw-transfer raw-param to tt-param.

find first param-compra no-lock no-error.
if avail param-compra then
    assign l-preco-bruto = (if param-compra.ipi-sobre-preco = 1 then yes else no).
else
    assign l-preco-bruto = no.

find first param-global no-lock no-error.
if avail param-global then
    assign c-empresa = grupo.

assign c-programa = "CC/0406"
       c-versao   = "1.00"
       c-revisao  = "000"
       da-iniper  = tt-param.da-iniper /*date(01,01,0001)*/
       da-fimper  = tt-param.da-fimper /*date(12,31,9999)*/ .

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_Emitidos_de * r}
assign c-titulo-relat = trim(return-value) + " "
                      + string(tt-param.da-iniper, "99/99/9999") + " ".
{utp/ut-liter.i a * r}
assign c-titulo-relat = c-titulo-relat
                      + trim(return-value) + " "
                      + string(tt-param.da-fimper,"99/99/9999").


{include/i-rpcab.i}                      

{include/i-rpout.i}
                                                                         
view frame f-cabec.
view frame f-rodape.


/* if  i-classifica = 1 then run ccp/ESCC0406a.p (input raw-param). /* Por Pedido          */ */
/* if  i-classifica = 2 then run ccp/ESCC0406b.p (input raw-param). /* Por Estabelecimento */ */
/* if  i-classifica = 3 then run ccp/ESCC0406c.p (input raw-param). /* Por Fornecedor      */ */
/* if  i-classifica = 4 then run ccp/ESCC0406d.p (input raw-param). /* Por Comprador       */ */


RUN ccp/escc0406excel.p (input raw-param).

/*
if tt-param.l-imprime-param then do:
    page.
    put unformatted
        c-lb-param   skip(1)
        c-lb-moeda   at 5 ": " tt-param.i-moeda " - " tt-param.c-moeda skip(1).
    
    assign l-param = tt-param.l-receb.
    put c-lb-ped-rec at 5 format "x(26)" ": " l-param.
    
    assign l-param = tt-param.l-alter.
    put c-lb-ord-alt at 5 format "x(35)" ": " l-param.
    
    assign l-param = tt-param.l-emergencial.
    put c-lb-ped-eme at 5 format "x(28)" ": " l-param.
    
    assign l-param = tt-param.l-moeda.
    put c-lb-outr-mo at 5 format "x(20)" ": " l-param.
    
    assign l-param = tt-param.l-extra-fornecedor.
    put c-lb-ext-fornec at 5 format "x(32)" ": " l-param skip(1).
    
    assign l-param = tt-param.l-narrativa.
    put c-lb-nar-it  at 5 format "x(17)" ": " l-param
        c-lb-nar-ord at 5 format "x(18)" ": " l-param skip(1).    
    
    assign l-param = tt-param.l-end-entrega.
    put c-lb-end-entrega at 5 format "x(11)" ": " l-param skip(1).
    
    if avail param-global and param-global.modulo-07 then do:    
        assign l-param = tt-param.l-despesas.
        put c-lb-despesas at 5 format "x(19)" ": " l-param.
        if tt-param.l-despesas then do:
            if tt-param.i-despesas-pag = 1 then
                put c-lb-despesas-pag1 at 5 format "x(34)".
            else
                put c-lb-despesas-pag2 at 5 format "x(20)".
            assign l-param = tt-param.l-despesas-inc.
            put c-lb-despesas-inc at 5 format "x(32)" ": " l-param.
        end.    
        put skip(1).
    end.    


    put unformatted
        c-lb-selec            skip(1)
        c-lb-pedido           at 5  ": "
        tt-param.i-pedido-i   at 22 "|< >| " at 35 tt-param.i-pedido-f
        c-lb-data             at 5  ": "
        tt-param.da-iniper    at 22 "|< >| " at 35 tt-param.da-fimper
        c-lb-estab            at 5  ": "
        tt-param.c-estabel-i  at 22 "|< >| " at 35 tt-param.c-estabel-f
        c-lb-fornec           at 5  ": "
        tt-param.i-fornec-i   at 22 "|< >| " at 35 tt-param.i-fornec-f
        c-lb-compr            at 5  ": "
        tt-param.c-comp-i     at 22 "|< >| " at 35 tt-param.c-comp-f
        skip(1)
        c-lb-classe           skip(1)
        tt-param.c-classe     at 5  skip(1)    
        c-lb-impr             skip(1)
        c-lb-dest             at 5  ": " tt-param.c-destino " - " tt-param.arquivo
        c-lb-usuar            at 5  ": " tt-param.usuario.
end.
*/
{include/i-rpclo.i}

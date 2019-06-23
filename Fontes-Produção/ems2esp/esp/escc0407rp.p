/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i escc0407RP 2.00.00.012}  /*** 010012 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i escc0407RP MCC}
&ENDIF


{include/i_fnctrad.i}
/*******************************************************************************
**
**       Programa: escc0407.P
**
**       Data....: Mar‡o de 1997
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem das Entregas Previstas
**
**       Versao..: 1.00.000 - Sandra Stadelhofer
**
*******************************************************************************/

/* Variaveis e Frames Comuns */
{esp/escc0407.i1 "new"}.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpvar.i}

find first param-global no-lock no-error.

find first ems2cadme.empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error.
assign c-empresa  = (if avail empresa then empresa.razao-social else "")
       c-programa = "ESCC0407"
       c-versao   = "1.00"
       c-revisao  = "000".

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Entregas_Previstas_de * r}
assign c-titulo-relat = trim(return-value) + " "
                      + string(tt-param.da-iniper, "99/99/9999").
{utp/ut-liter.i a * r}
assign c-titulo-relat = c-titulo-relat     + " "
                      + trim(return-value) + " "
                      + string(tt-param.da-fimper, "99/99/9999").

find first param-compra no-lock no-error.
if  avail param-compra then
    assign l-preco-bruto = (if param-compra.ipi-sobre-preco = 1 then yes else no).
else
    assign l-preco-bruto = no.

{include/i-rpcab.i}

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

run esp/escc0407a.p (input raw-param).

/* case tt-param.i-classifica:                                                         */
/*     when 1 then run esp/escc0407b.p (input raw-param). /* Por Comprador / Data   */ */
/*     when 2 then run esp/escc0407a.p (input raw-param). /* Por Data de Entrega    */ */
/*     when 3 then run esp/escc0407c.p (input raw-param). /* Por Estabelecimento    */ */
/*     when 4 then do:                                                                 */
/*         if tt-param.i-nome-abrev = 1                                                */
/*            then run esp/escc0407d.p (input raw-param). /* Por Fornecedor         */ */
/*            else run esp/escc0407e.p (input raw-param). /* Por Nome Abrev Fornec  */ */
/*     end.                                                                            */
/*     when 5 then run esp/escc0407f.p (input raw-param). /* Por Item               */ */
/*     when 6 then run esp/escc0407g.p (input raw-param). /* Por Pedido             */ */
/*     when 7 then run esp/escc0407h.p (input raw-param). /* Por Processo de Compra */ */
/* end case.                                                                           */

page.
put unformatted
    c-lb-param     skip(1)
    c-lb-moeda     at 5 ": " tt-param.i-tipo-moeda " - "
                             tt-param.c-descri-moeda skip(1).
{utp/ut-liter.i Sim/NÆo}
ASSIGN c-format = TRIM(RETURN-VALUE).

assign l-param = tt-param.l-lista-narra-item.
put c-lb-narra-it  at 5 format "x(17)" "..: " l-param FORMAT c-format.
assign l-param = tt-param.l-lista-narra-pedido.
put c-lb-narra-ped at 5 format "x(19)" ": "   l-param FORMAT c-format.
assign l-param = tt-param.l-lista-narra-ordem.
put c-lb-narra-ord at 5 format "x(18)" ".: "  l-param FORMAT c-format.
assign l-param = tt-param.l-lista-texto-livre.
put c-lb-texto     at 5 format "x(19)" ": "   l-param FORMAT c-format " - "
                                              tt-param.c-tipo-texto skip.

assign l-param = tt-param.l-ped-emitido.
put c-lb-ped-emit at 05 format "x(25)" ": " l-param format c-format skip.
assign l-param = tt-param.l-ped-aprovado.
put c-lb-ped-aprov at 05 format "x(25)" ": " l-param format c-format skip(1).

if avail param-global and param-global.modulo-07 then do:    
    assign l-param = tt-param.l-despesas-imp.
    put c-lb-despesas at 5 format "x(19)" ": " l-param FORMAT c-format.
    if tt-param.l-despesas-imp then do:
        if tt-param.i-despesas-pag = 1 then
            put c-lb-despesas-pag1 at 5 format "x(34)".
        else
            put c-lb-despesas-pag2 at 5 format "x(20)".
        assign l-param = tt-param.l-despesas-inc.
        put c-lb-despesas-inc at 5 format "x(32)" ": " l-param FORMAT c-format.
    end.    
    put skip(1).
end.

assign l-param = tt-param.l-salto-pagina.
put unformatted
    c-lb-selec               skip(1)
    c-lb-estab               at 5  ":"
    tt-param.c-estabel-i     at 23 "|<  >| " at 40 tt-param.c-estabel-f
    c-lb-item                at 5  ":"
    tt-param.c-item-i        at 23 "|<  >| " at 40 tt-param.c-item-f
    c-lb-grupo               at 5  ":"
    tt-param.i-gr-fornec-i   at 23 "|<  >| " at 40 tt-param.i-gr-fornec-f
    c-lb-fornec              at 5  ":"
    tt-param.i-fornec-i      at 23 "|<  >| " at 40 tt-param.i-fornec-f
    c-lb-compr               at 5  ":"
    tt-param.c-comprado-i    at 23 "|<  >| " at 40 tt-param.c-comprado-f
    c-lb-proc                at 5  ":"
    tt-param.i-nr-processo-i at 23 "|<  >| " at 40 tt-param.i-nr-processo-f
    c-lb-pedi                at 5  ":"
    tt-param.i-pedido-i      at 23 "|<  >| " at 40 tt-param.i-pedido-f
    c-lb-data                at 5  ":".
put tt-param.da-iniper       at 23 "|<  >| " at 40 tt-param.da-fimper skip(1).
put unformatted
    c-lb-classe                    skip(1)
    tt-param.c-classe        at 5  skip(1)
    c-lb-fornec              at 5  " "  tt-param.c-param  skip(1).
put c-lb-salta               at 5  format "x(27)" ": " l-param FORMAT c-format skip(1).
put unformatted
    c-lb-impr                      skip(1)
    c-lb-dest                at 5  ": " tt-param.c-destino " - " tt-param.arquivo
    c-lb-usuar               at 5  ": " tt-param.usuario.

{include/i-rpclo.i}

return "OK":U.

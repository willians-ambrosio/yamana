/******************************************************************************
**
**  Programa: cdp/escdr024arp.p
**  Data....: Junho 2013
**  Autor...: Fernando Campos - DSC
**  Objetivo: Relatorio Condi‡Æo de Pagamento Ativa
**
******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i ESCDR024ARP 2.06.00.001}
{include/i-rpvar.i}
{utp/ut-glob.i}

def var h-acomp                     as handle       no-undo.
def var cond-pagto-ext-cod-vencto   as character    no-undo.
def var cond-pagto-ext-dia-sem-base as character    no-undo.
def var cond-pagto-ext-dia-sem-venc as character    no-undo.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field cod-cond-pag-ini as integer format ">>>9"
    field cod-cond-pag-fim as integer format ">>>9"
    field cd-ativa         as logical.

def temp-table tt-raw-digita
    field raw-digita       as raw.

form cond-pagto.cod-cond-pag        column-label "Cond"                     format ">>>9"       at 001
     cond-pagto.descricao           column-label "Descricao"                format "x(28)"      at 006
     cond-pagto-ext-cod-vencto      column-label "C¢digo Vcto"              format "x(014)"     at 035
     cond-pagto.dia-mes-base        column-label "DMB"                      format ">9"         at 050
     cond-pagto-ext-dia-sem-base    column-label "Dia Semana Base"          format "x(013)"     at 054
     cond-pagto.dia-mes-venc        column-label "DMV"                      format ">9"         at 070
     cond-pagto-ext-dia-sem-venc    column-label "Dia Semana Vencimento"    format "x(013)"     at 074
     cond-pagto.num-parcelas        column-label "Parc"                     format ">9"         at 096
     cond-pagto.nr-dias-ante        column-label "Antec"                    format ">9"         at 101
     cond-pagto.nr-dupdes           column-label "Num Dup Desc"             format ">9"         at 107
     cond-pagto.per-des-pgan        column-label "% Desc"                   format ">9.99"      at 120
     cond-pagto.nr-tab-finan        column-label "Fin"                      format ">>9"        at 127
     cond-pagto.log-atual-idx       column-label "At Ind"                   format "Sim/NÆo"    at 131
     cond-pagto.nr-ind-finan        column-label "IF"                       format ">9"         at 138
     with down width 139 no-box stream-io frame f-relat-09-132.

/* recebimento de parametros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

/* --- Main Block --- */

find first empresa no-lock no-error.

create tt-param.
raw-transfer raw-param to tt-param no-error.

assign c-programa 	  = "ESCDR024A"
       c-versao	      = "2.06"
       c-revisao	  = ".00.001"
       c-empresa	  = empresa.nome
       c-sistema	  = "Especificos"
       c-titulo-relat = "Listagem Condi‡äes Pagamento - Especifico".

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Aguarde...").

{include/i-rpout.i}
{include/i-rpcab.i}

do on error undo, return no-apply:
    run pi-executar.
end.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK":U.

/* --- Procedure --- */

procedure pi-executar:

    view frame f-cabec.
    view frame f-rodape.

    for each cond-pagto no-lock
        where cond-pagto.cod-cond-pag >= tt-param.cod-cond-pag-ini
        and   cond-pagto.cod-cond-pag <= tt-param.cod-cond-pag-fim:

        find es-cond-pagto no-lock
            where es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag no-error.
        if not avail es-cond-pagto then do:
            if tt-param.cd-ativa = yes then next.
        end.
        else do:
            if es-cond-pagto.cd-ativa <> tt-param.cd-ativa then next.
        end.

        run pi-acompanhar in h-acomp(input "Cond.Pagto: "+ string(cond-pagto.cod-cond-pag)).

        assign cond-pagto-ext-cod-vencto   = entry(cond-pagto.cod-vencto  ,"Dias da Data,· Vista,Antecipado,Contra Entrega,Fora Dezena,Fora Quinzena,Fora Mˆs,Fora Semana,Apresenta‡Æo,D¡as de Fch,Contado,Anticipo,Contra Entrega,Fuer Decena,Fuer Quincena,Fuer Mes,Fuer Semana,Presentaci¢n,Date Days,Cash,Advanced,Cash on Dlvy,Ten Days,Bimonthly,Month,Week,Layout")
               cond-pagto-ext-dia-sem-base = entry(cond-pagto.dia-sem-base,"Domingo,Segunda,Ter‡a,Quarta,Quinta,Sexta,S bado,NÆo Considera,Domingo,Lunes,Martes,Mi‚rcols,Jueves,Viernes,S bado,No considera,Sunday,Mon,Tues,Wed,Thurs,Fri,Saturday,Exclude")
               cond-pagto-ext-dia-sem-venc = entry(cond-pagto.dia-sem-venc,"Domingo,Segunda,Ter‡a,Quarta,Quinta,Sexta,S bado,NÆo Considera,Domingo,Lunes,Martes,Mi‚rcols,Jueves,Viernes,S bado,No considera,Sunday,Mon,Tues,Wed,Thurs,Fri,Saturday,Exclude").

        run utp/ut-liter.p (input cond-pagto-ext-cod-vencto, "*", "").
        assign cond-pagto-ext-cod-vencto = return-value.

        run utp/ut-liter.p (input cond-pagto-ext-dia-sem-base, "*", "").
        assign cond-pagto-ext-dia-sem-base = return-value.

        run utp/ut-liter.p (input cond-pagto-ext-dia-sem-venc, "*", "").
        assign cond-pagto-ext-dia-sem-venc = return-value.

        display cond-pagto.cod-cond-pag
                cond-pagto.descricao
                cond-pagto-ext-cod-vencto
                cond-pagto.dia-mes-base
                cond-pagto-ext-dia-sem-base
                cond-pagto.dia-mes-venc
                cond-pagto-ext-dia-sem-venc
                cond-pagto.num-parcelas
                cond-pagto.nr-dias-ante
                cond-pagto.nr-dupdes
                cond-pagto.per-des-pgan
                cond-pagto.nr-tab-finan
                cond-pagto.log-atual-idx
                cond-pagto.nr-ind-finan
                with frame f-relat-09-132.
        down with frame f-relat-09-132.

    end.

end procedure.

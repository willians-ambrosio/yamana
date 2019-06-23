/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCN0001RP 2.00.00.001 } /*** 010001 ***/
/*******************************************************************************
**
**       Programa: ESCN0001RP
**
**       Data....: Agosto de 2016
**
**       Objetivo: Exporta‡Æo CSV Altera‡Æo Pre‡o Itens
**
**       Versao..: 1.00.000
**
*******************************************************************************/
{cdp/cdcfgmat.i}

&GLOBAL-DEFINE RTF NO

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           AS char format "x(35)":U
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG
    /*Fim alteracao 15/02/2005*/
    field nr-contrato      as int
    field arquivo-destino  as char.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

{cnp/esp/escn0001tt.i}

DEFINE TEMP-TABLE tt-linha-exp NO-UNDO
    FIELD num-linha    AS INTEGER 
    FIELD linha        AS CHARACTER FORMAT "X(5000)".

define input parameter raw-param as raw no-undo.
define input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

{include/i-rpvar.i}
def var h-acomp     as  handle  no-undo.
def var i-linha     as  int     no-undo.
def var c-destino   as  char    no-undo.

DEF STREAM str-csv.

find first param-global no-lock no-error.

assign c-programa 	  = "ESCN0001RP"
       c-versao	      = "1.00"
       c-revisao	  = ".00.000"
       c-empresa      = param-global.grupo
       c-sistema	  = "Contratos"
       c-titulo-relat = "Exporta‡Æo CSV Altera‡Æo Pre‡o Itens".

{include/i-rpcab.i}
{include/i-rpout.i}

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input return-value).

FOR FIRST tt-param:

    EMPTY TEMP-TABLE tt-contrato-cc00.
    EMPTY TEMP-TABLE tt-contrato-cc01.
    EMPTY TEMP-TABLE tt-item-contrato-ic00.
    EMPTY TEMP-TABLE tt-item-contrato-ic01.
    
    /* Gera dados. */
    FOR EACH contrato-for NO-LOCK
        WHERE contrato-for.nr-contrato = tt-param.nr-contrato:

        CREATE tt-contrato-cc00.
        ASSIGN tt-contrato-cc00.im-nr-contrat       = contrato-for.nr-contrato
               tt-contrato-cc00.im-cod-emitente     = contrato-for.cod-emitente
               tt-contrato-cc00.im-impr-contrat     = contrato-for.impr-contrat
               tt-contrato-cc00.im-cod-tipo-contrat = contrato-for.cod-tipo-contrat
               tt-contrato-cc00.im-natureza         = contrato-for.natureza
               tt-contrato-cc00.im-gestor-tecnico   = contrato-for.gestor-tecnico
               tt-contrato-cc00.im-frete            = contrato-for.frete
               tt-contrato-cc00.im-variacao-qtd     = contrato-for.variacao-qtd
               tt-contrato-cc00.im-variacao-preco   = contrato-for.variacao-preco
               tt-contrato-cc00.im-cod-mensagem     = contrato-for.cod-mensagem
               tt-contrato-cc00.im-cod-cond-pag     = contrato-for.cod-cond-pag
               tt-contrato-cc00.im-cod-transp       = contrato-for.cod-transp
               tt-contrato-cc00.im-via-transp       = contrato-for.via-transp
               tt-contrato-cc00.im-val-total        = contrato-for.val-total
               tt-contrato-cc00.im-cod-comprado     = contrato-for.cod-comprado
               tt-contrato-cc00.im-ind-sit-contrat  = contrato-for.ind-sit-contrat
               tt-contrato-cc00.im-qtd-total        = contrato-for.qtd-total
               tt-contrato-cc00.im-sld-qtd          = contrato-for.sld-qtd
               tt-contrato-cc00.im-sld-val          = contrato-for.sld-val
               tt-contrato-cc00.im-acum-rec-qtd     = contrato-for.acum-rec-qtd
               tt-contrato-cc00.im-acum-rec-val     = contrato-for.acum-rec-val
               tt-contrato-cc00.im-sld-qtd-liber    = contrato-for.sld-qtd-liber
               tt-contrato-cc00.im-sld-val-liber    = contrato-for.sld-val-liber
               tt-contrato-cc00.im-val-fatur-minimo = contrato-for.val-fatur-minimo
               tt-contrato-cc00.im-dat-ini-validade = contrato-for.dt-ini-validade
               tt-contrato-cc00.im-dat-fim-validade = contrato-for.dt-ter-validade
               tt-contrato-cc00.im-dat-contrat      = contrato-for.dt-contrato
               tt-contrato-cc00.im-dec-2            = contrato-for.dec-2
               tt-contrato-cc00.im-cod-estabel      = contrato-for.cod-estabel
               tt-contrato-cc00.im-cod-estab-cobr   = contrato-for.cod-estab-cobr
               tt-contrato-cc00.im-cod-estab-orig   = contrato-for.cod-estab-orig
               tt-contrato-cc00.im-cod-estab-entr   = contrato-for.cod-estab-entr.
        
        CREATE tt-contrato-cc01.
        ASSIGN tt-contrato-cc01.im-nr-contrat        = contrato-for.nr-contrato
               tt-contrato-cc01.im-cod-emitente      = contrato-for.cod-emitente
               tt-contrato-cc01.im-des-contrat       = contrato-for.des-contrat
               tt-contrato-cc01.im-acum-val-pago     = contrato-for.acum-val-pago
               tt-contrato-cc01.im-dat-revisao       = contrato-for.dat-revisao
               tt-contrato-cc01.im-mo-codigo         = contrato-for.mo-codigo
               tt-contrato-cc01.im-log-libera        = contrato-for.log-libera
               tt-contrato-cc01.im-tp-fornecim       = contrato-for.tp-fornecim
               tt-contrato-cc01.im-contato           = contrato-for.contato
               tt-contrato-cc01.im-ind-control-rec   = contrato-for.ind-control-rec
               tt-contrato-cc01.im-sld-qtd-med       = contrato-for.sld-qtd-med
               tt-contrato-cc01.im-sld-val-med       = contrato-for.sld-val-med
               tt-contrato-cc01.im-sld-qtd-liber-med = contrato-for.sal-qtd-liber-med
               tt-contrato-cc01.im-sld-val-liber-med = contrato-for.sld-val-liber-med
               tt-contrato-cc01.im-cod-projeto       = contrato-for.cod-projeto
               tt-contrato-cc01.im-cod-cond-fatur    = contrato-for.cod-cond-fatur
               tt-contrato-cc01.im-sld-val-receb     = contrato-for.sld-val-receb
               tt-contrato-cc01.im-narrat-contrat    = contrato-for.narrat-contrat
               tt-contrato-cc01.im-num-ord-inv       = contrato-for.num-ord-inv.

        FOR EACH item-contrat  NO-LOCK
            WHERE item-contrat.nr-contrato = contrato-for.nr-contrato:

            CREATE tt-item-contrato-ic00.
            ASSIGN tt-item-contrato-ic00.im-nr-contrat           = item-contrat.nr-contrato
                   tt-item-contrato-ic00.im-num-seq-item         = item-contrat.num-seq-item
                   tt-item-contrato-ic00.im-cod-emitente         = item-contrat.cod-emitente
                   tt-item-contrato-ic00.im-it-codigo            = item-contrat.it-codigo
                   tt-item-contrato-ic00.im-preco-unit           = item-contrat.preco-unit
                   tt-item-contrato-ic00.im-qtd-minima           = item-contrat.qtd-minima
                   tt-item-contrato-ic00.im-sld-val              = item-contrat.sld-val
                   tt-item-contrato-ic00.im-val-fatur-minimo     = item-contrat.val-fatur-minimo
                   tt-item-contrato-ic00.im-mo-codigo            = item-contrat.mo-codigo
                   tt-item-contrato-ic00.im-log-libera           = item-contrat.log-libera
                   tt-item-contrato-ic00.im-val-total            = item-contrat.val-total
                   tt-item-contrato-ic00.im-cod-refer            = item-contrat.cod-refer
                   tt-item-contrato-ic00.im-codigo-ipi           = item-contrat.codigo-ipi
                   tt-item-contrato-ic00.im-codigo-icm           = item-contrat.codigo-icm
                   tt-item-contrato-ic00.im-un                   = item-contrat.un
                   tt-item-contrato-ic00.im-contato              = item-contrat.contato
                   tt-item-contrato-ic00.im-frequencia           = item-contrat.frequencia
                   tt-item-contrato-ic00.im-ind-sit-item         = item-contrat.ind-sit-item
                   tt-item-contrato-ic00.im-qtd-total            = item-contrat.qtd-total
                   tt-item-contrato-ic00.im-ind-un-contrato      = item-contrat.ind-un-contrato
                   tt-item-contrato-ic00.im-sld-qtd              = item-contrat.sld-qtd
                   tt-item-contrato-ic00.im-acum-rec-val         = item-contrat.acum-rec-val
                   tt-item-contrato-ic00.im-acum-rec-qtd         = item-contrat.acum-rec-qtd
                   tt-item-contrato-ic00.im-ind-tipo-control-val = item-contrat.ind-tipo-control-val
                   tt-item-contrato-ic00.im-sld-qtd-liber        = item-contrat.sld-qtd-liber
                   tt-item-contrato-ic00.im-sld-val-liber        = item-contrat.sld-val-liber
                   tt-item-contrato-ic00.im-log-control-event    = item-contrat.log-control-event
                   tt-item-contrato-ic00.im-ind-caract-item      = item-contrat.ind-caract-item
                   tt-item-contrato-ic00.im-log-caract-item      = 0
                   tt-item-contrato-ic00.im-log-obrig-item       = item-contrat.log-obrig-item
                   tt-item-contrato-ic00.im-log-ind-multa        = item-contrat.log-ind-multa
                   tt-item-contrato-ic00.im-perc-multa-dia       = item-contrat.perc-multa-dia
                   tt-item-contrato-ic00.im-perc-multa-limite    = item-contrat.perc-multa-limite
                   tt-item-contrato-ic00.im-cod-depos            = item-contrat.cod-depos
                   tt-item-contrato-ic00.im-aliquota-icm         = item-contrat.aliquota-icm
                   tt-item-contrato-ic00.im-aliquota-ipi         = item-contrat.aliquota-ipi
                   tt-item-contrato-ic00.im-aliquota-iss         = item-contrat.aliquota-iss
                   tt-item-contrato-ic00.im-tp-despesa           = item-contrat.tp-despesa
                   tt-item-contrato-ic00.im-cod-cond-pag         = item-contrat.cod-cond-pag
                   tt-item-contrato-ic00.im-frete-ped            = item-contrat.frete.

            CREATE tt-item-contrato-ic01.
            ASSIGN tt-item-contrato-ic01.im-nr-contrat       = item-contrat.nr-contrato
                   tt-item-contrato-ic01.im-num-seq-item     = item-contrat.num-seq-item
                   tt-item-contrato-ic01.im-cod-emitente     = item-contrat.cod-emitente
                   tt-item-contrato-ic01.im-it-codigo        = item-contrat.it-codigo
                   tt-item-contrato-ic01.im-preco-fornec     = item-contrat.preco-fornec
                   tt-item-contrato-ic01.im-taxa-financ      = item-contrat.taxa-financ
                   tt-item-contrato-ic01.im-val-frete        = item-contrat.val-frete
                   tt-item-contrato-ic01.im-val-taxa         = item-contrat.val-taxa
                   tt-item-contrato-ic01.im-prazo-ent        = item-contrat.prazo-ent
                   tt-item-contrato-ic01.im-dat-cotac        = item-contrat.dat-cotac
                   tt-item-contrato-ic01.im-preco-base       = item-contrat.preco-base
                   tt-item-contrato-ic01.im-cod-comprado     = item-contrat.cod-comprado
                   tt-item-contrato-ic01.im-perc-desconto    = item-contrat.perc-desconto
                   tt-item-contrato-ic01.im-narrat-compra    = item-contrat.narrat-compra
                   tt-item-contrato-ic01.im-ind-tipo-control = item-contrat.ind-tipo-control
                   tt-item-contrato-ic01.im-pre-unit-for     = item-contrat.pre-unit-for
                   tt-item-contrato-ic01.im-dat-base         = item-contrat.dat-base
                   tt-item-contrato-ic01.im-sld-qtd-receb    = item-contrat.sld-qtd-receb
                   tt-item-contrato-ic01.im-sld-val-receb    = item-contrat.sld-val-receb
                   tt-item-contrato-ic01.im-narrat-item      = item-contrat.narrat-item
                   tt-item-contrato-ic01.im-num-ord-invest   = item-contrat.num-ord-inv
                   tt-item-contrato-ic01.im-ordem-base       = 0.

            FOR FIRST item-fornec NO-LOCK 
                WHERE item-fornec.it-codigo     = item-contrat.it-codigo
                AND   item-fornec.cod-emitente  = item-contrat.cod-emitente:

                ASSIGN tt-item-contrato-ic01.im-perc-compra  = item-fornec.perc-compra.
            END.
        END.
    END.

    /* Gera arquivo. */
    OUTPUT STREAM str-csv TO VALUE (tt-param.arquivo-destino).
        
        /* Linha CCIN */
        PUT STREAM str-csv UNFORMATTED 
            "CCIN" ";"
            "3" SKIP.

        FOR EACH tt-contrato-cc00:
            
            /* Linha CC00 */
            PUT STREAM str-csv UNFORMATTED 
                "CC00"                                          ";"
                tt-contrato-cc00.im-nr-contrat                  ";"
                STRING(tt-contrato-cc00.im-impr-contrat, "S/N") ";"
                tt-contrato-cc00.im-cod-tipo-contrat            ";"
                tt-contrato-cc00.im-natureza                    ";"
                tt-contrato-cc00.im-gestor-tecnico              ";"
                tt-contrato-cc00.im-frete                       ";"
                tt-contrato-cc00.im-variacao-qtd                ";"
                tt-contrato-cc00.im-variacao-preco              ";"
                tt-contrato-cc00.im-cod-mensagem                ";"
                tt-contrato-cc00.im-cod-cond-pag                ";"
                tt-contrato-cc00.im-cod-transp                  ";"
                tt-contrato-cc00.im-via-transp                  ";"
                tt-contrato-cc00.im-val-total                   ";"
                tt-contrato-cc00.im-cod-comprado                ";"
                tt-contrato-cc00.im-ind-sit-contrat             ";"
                tt-contrato-cc00.im-qtd-total                   ";"
                tt-contrato-cc00.im-sld-qtd                     ";"
                tt-contrato-cc00.im-sld-val                     ";"
                tt-contrato-cc00.im-acum-rec-qtd                ";"
                tt-contrato-cc00.im-acum-rec-val                ";"
                tt-contrato-cc00.im-sld-qtd-liber               ";"
                tt-contrato-cc00.im-sld-val-liber               ";"
                tt-contrato-cc00.im-val-fatur-minimo            ";"
                tt-contrato-cc00.im-dat-ini-validade            ";"
                tt-contrato-cc00.im-dat-fim-validade            ";"
                tt-contrato-cc00.im-dat-contrat                 ";"
                tt-contrato-cc00.im-cod-emitente                ";"
                tt-contrato-cc00.im-cod-estabel                 ";"
                tt-contrato-cc00.im-cod-estab-cobr              ";"
                tt-contrato-cc00.im-cod-estab-orig              ";"
                tt-contrato-cc00.im-cod-estab-entr              ";"
                tt-contrato-cc00.im-dec-2                       SKIP.
    
            FOR EACH tt-contrato-cc01 
                WHERE tt-contrato-cc01.im-nr-contrat = tt-contrato-cc00.im-nr-contrat:

                /* Linha CC01 */
                PUT STREAM str-csv UNFORMATTED 
                    "CC01"                                        ";"
                    tt-contrato-cc01.im-nr-contrat                ";"
                    tt-contrato-cc01.im-des-contrat               ";"
                    tt-contrato-cc01.im-acum-val-pago             ";"
                    tt-contrato-cc01.im-mo-codigo                 ";"
                    STRING(tt-contrato-cc01.im-log-libera, "S/N") ";"
                    tt-contrato-cc01.im-tp-fornecim               ";"
                    ""                                            ";" /* A linha 8 ‚ uma linha vazia */
                    tt-contrato-cc01.im-ind-control-rec           ";"
                    tt-contrato-cc01.im-sld-qtd-med               ";"
                    tt-contrato-cc01.im-sld-qtd-liber-med         ";"
                    tt-contrato-cc01.im-sld-val-med               ";"
                    tt-contrato-cc01.im-sld-val-liber-med         ";"
                    tt-contrato-cc01.im-cod-projeto               ";"
                    tt-contrato-cc01.im-cod-cond-fatur            ";"
                    tt-contrato-cc01.im-dat-revisao               ";"
                    tt-contrato-cc01.im-contato                   ";"
                    tt-contrato-cc01.im-narrat-contrat            ";"
                    tt-contrato-cc01.im-num-ord-inv               SKIP.

            END.
            
            FOR EACH tt-item-contrato-ic00
                WHERE tt-item-contrato-ic00.im-nr-contrat = tt-contrato-cc00.im-nr-contrat:
                
                PUT STREAM str-csv UNFORMATTED 
                    "IC00"                                                    ";"
                    tt-item-contrato-ic00.im-nr-contrat                       ";"
                    tt-item-contrato-ic00.im-preco-unit                       ";"
                    tt-item-contrato-ic00.im-qtd-minima                       ";"
                    tt-item-contrato-ic00.im-sld-val                          ";"
                    tt-item-contrato-ic00.im-val-fatur-minimo                 ";"
                    tt-item-contrato-ic00.im-mo-codigo                        ";"
                    STRING(tt-item-contrato-ic00.im-log-libera, "S/N")        ";"
                    tt-item-contrato-ic00.im-it-codigo                        ";"
                    tt-item-contrato-ic00.im-val-total                        ";"
                    tt-item-contrato-ic00.im-cod-refer                        ";"
                    STRING(tt-item-contrato-ic00.im-codigo-ipi, "S/N")        ";"
                    tt-item-contrato-ic00.im-codigo-icm                       ";"
                    tt-item-contrato-ic00.im-un                               ";"
                    tt-item-contrato-ic00.im-contato                          ";"
                    tt-item-contrato-ic00.im-num-seq-item                     ";"
                    tt-item-contrato-ic00.im-frequencia                       ";"
                    tt-item-contrato-ic00.im-ind-sit-item                     ";"
                    tt-item-contrato-ic00.im-qtd-total                        ";"
                    tt-item-contrato-ic00.im-ind-un-contrato                  ";"
                    tt-item-contrato-ic00.im-sld-qtd                          ";"
                    tt-item-contrato-ic00.im-acum-rec-val                     ";"
                    tt-item-contrato-ic00.im-acum-rec-qtd                     ";"
                    tt-item-contrato-ic00.im-ind-tipo-control-val             ";"
                    tt-item-contrato-ic00.im-sld-qtd-liber                    ";"
                    tt-item-contrato-ic00.im-sld-val-liber                    ";"
                    STRING(tt-item-contrato-ic00.im-log-control-event, "S/N") ";"
                    tt-item-contrato-ic00.im-ind-caract-item                  ";"
                    STRING(tt-item-contrato-ic00.im-log-obrig-item, "S/N")    ";"
                    STRING(tt-item-contrato-ic00.im-log-ind-multa, "S/N")     ";"
                    tt-item-contrato-ic00.im-perc-multa-dia                   ";"
                    tt-item-contrato-ic00.im-perc-multa-limite                ";"
                    tt-item-contrato-ic00.im-cod-depos                        ";"
                    tt-item-contrato-ic00.im-aliquota-icm                     ";"
                    tt-item-contrato-ic00.im-aliquota-ipi                     ";"
                    tt-item-contrato-ic00.im-aliquota-iss                     ";"
                    tt-item-contrato-ic00.im-tp-despesa                       ";"
                    tt-item-contrato-ic00.im-cod-cond-pag                     ";"
                    STRING(tt-item-contrato-ic00.im-frete-ped, "S/N")         ";"
                    tt-item-contrato-ic00.im-cod-emitente                     ";" SKIP.

                FOR EACH tt-item-contrato-ic01
                    WHERE tt-item-contrato-ic01.im-nr-contrat   = tt-item-contrato-ic00.im-nr-contrat
                    AND   tt-item-contrato-ic01.im-num-seq-item = tt-item-contrato-ic00.im-num-seq-item:

                    PUT STREAM str-csv UNFORMATTED 
                        "IC01"                                              ";"
                        tt-item-contrato-ic01.im-nr-contrat                 ";"
                        tt-item-contrato-ic01.im-num-seq-item               ";"
                        tt-item-contrato-ic01.im-preco-fornec               ";"
                        STRING(tt-item-contrato-ic01.im-taxa-financ, "S/N") ";"
                        tt-item-contrato-ic01.im-val-frete                  ";"
                        tt-item-contrato-ic01.im-val-taxa                   ";"
                        tt-item-contrato-ic01.im-prazo-ent                  ";"
                        tt-item-contrato-ic01.im-dat-cotac                  ";"
                        tt-item-contrato-ic01.im-preco-base                 ";"
                        tt-item-contrato-ic01.im-cod-comprado               ";"
                        tt-item-contrato-ic01.im-perc-desconto              ";"
                        tt-item-contrato-ic01.im-narrat-compra              ";"
                        tt-item-contrato-ic01.im-ind-tipo-control           ";"
                        tt-item-contrato-ic01.im-pre-unit-for               ";"
                        tt-item-contrato-ic01.im-dat-base                   ";"
                        tt-item-contrato-ic01.im-sld-qtd-receb              ";"
                        tt-item-contrato-ic01.im-sld-val-receb              ";"
                        tt-item-contrato-ic01.im-ordem-base                 ";"
                        tt-item-contrato-ic01.im-narrat-item                ";"
                        tt-item-contrato-ic01.im-perc-compra                ";"
                        tt-item-contrato-ic01.im-num-ord-invest             SKIP.
                    
                END.
            END.
        END.

    OUTPUT STREAM str-csv CLOSE.

END.

view frame f-cabec.
view frame f-rodape.
/************************************************/

/** IMPRESSÇO PARAMETROS **/
page.

case tt-param.destino:
   when 1 then
       assign c-destino = "Impressora".
   when 2 then
       assign c-destino = "Arquivo".
   when 3 then
       assign c-destino = "Terminal".
end case.

put unformatted
    "IMPRESSÇO"             at 01                                                       skip(1)
    "            Destino: " at 10  c-destino          " - "    tt-param.arquivo         skip
    "    Destino Arquivo: " at 10  "Arquivo "         " - "    tt-param.arquivo-destino skip
    "           Contrato: " at 10  tt-param.nr-contrato                                 skip
    "            Usu rio: " at 10  tt-param.usuario                                     skip
    "Imprimir Parƒmetros: " at 10  "Sim". 

/************************************************/

{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

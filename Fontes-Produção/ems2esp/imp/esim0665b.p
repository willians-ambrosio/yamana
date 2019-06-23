/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIM0665B 2.00.00.015}  /*** 010015 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esim0665b MIM}
&ENDIF

/******************************************************************************
**
**       PROGRAMA:  ESIM0665B.P
**
**       DATA....:  FEVEREIRO DE 2003
**
**       OBJETIVO:  Custo Real de Importa‡Æo / Por Embarque - Resumido
**
**       VERSAO..:  1.00.000 - Ivonei Vock
**
******************************************************************************/

{cdp/cdcfgmat.i}
{imp/esim0665.i} /* Defini‡Æo tt-param          **
               ** Defini‡Æo tt-detalha-item   **
               ** Defini‡Æo tt-embarque-cabec **
               ** Defini‡Æo tt-embarque-corpo */
{imp/esim0665.i1} /* Defini‡Æo das Frames */

def input param table for tt-param.
def input param table for tt-detalha-item.
def input param table for tt-embarque-cabec.
def input param table for tt-embarque-corpo.

find first tt-param.

FOR EACH tt-embarque-corpo-agr:
    DELETE tt-embarque-corpo-agr.
END.

for each tt-embarque-corpo:
    find first tt-embarque-corpo-agr
         where tt-embarque-corpo-agr.embarque    = tt-embarque-corpo.embarque
           and tt-embarque-corpo-agr.cod-estabel = tt-embarque-corpo.cod-estabel no-error.
    if  not avail tt-embarque-corpo-agr then do:
        create tt-embarque-corpo-agr.
        assign tt-embarque-corpo-agr.embarque     = tt-embarque-corpo.embarque
               tt-embarque-corpo-agr.cod-estabel  = tt-embarque-corpo.cod-estabel
               tt-embarque-corpo-agr.desc-estabel = tt-embarque-corpo.desc-estabel
               tt-embarque-corpo-agr.cod-emitente = tt-embarque-corpo.cod-emitente.
    end.

    assign tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea = tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea
                                                             + tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea
           tt-embarque-corpo-agr.vl-ger-mercado-rea          = tt-embarque-corpo-agr.vl-ger-mercado-rea
                                                             + tt-embarque-corpo.vl-ger-mercado-rea
           tt-embarque-corpo-agr.vl-ger-tot-des-rea          = tt-embarque-corpo-agr.vl-ger-tot-des-rea
                                                             + tt-embarque-corpo.vl-ger-tot-des-rea
           tt-embarque-corpo-agr.vl-ger-tot-imp-rea          = tt-embarque-corpo-agr.vl-ger-tot-imp-rea
                                                             + tt-embarque-corpo.vl-ger-tot-imp-rea
           tt-embarque-corpo-agr.vl-ger-ii-rea               = tt-embarque-corpo-agr.vl-ger-ii-rea
                                                             + tt-embarque-corpo.vl-ger-ii-rea
           tt-embarque-corpo-agr.vl-ger-ipi-rea              = tt-embarque-corpo-agr.vl-ger-ipi-rea
                                                             + tt-embarque-corpo.vl-ger-ipi-rea
           tt-embarque-corpo-agr.vl-ger-icms-rea             = tt-embarque-corpo-agr.vl-ger-icms-rea
                                                             + tt-embarque-corpo.vl-ger-icms-rea
           tt-embarque-corpo-agr.vl-ger-cofins-rea           = tt-embarque-corpo-agr.vl-ger-cofins-rea
                                                             + tt-embarque-corpo.vl-ger-cofins-rea
           tt-embarque-corpo-agr.vl-ger-pis-rea              = tt-embarque-corpo-agr.vl-ger-pis-rea
                                                             + tt-embarque-corpo.vl-ger-pis-rea
           tt-embarque-corpo-agr.total-vl-frete              = tt-embarque-corpo-agr.total-vl-frete
                                                             + tt-embarque-corpo.total-vl-frete
           tt-embarque-corpo-agr.total-vl-embalagem          = tt-embarque-corpo-agr.total-vl-embalagem
                                                             + tt-embarque-corpo.total-vl-embalagem
           tt-embarque-corpo-agr.total-vl-seguro             = tt-embarque-corpo-agr.total-vl-seguro
                                                             + tt-embarque-corpo.total-vl-seguro
           tt-embarque-corpo-agr.total-vl-outras             = tt-embarque-corpo-agr.total-vl-outras
                                                             + tt-embarque-corpo.total-vl-outras
           tt-embarque-corpo-agr.per-mercado                 = (tt-embarque-corpo-agr.vl-ger-mercado-rea
                                                             / tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea) * 100
           tt-embarque-corpo-agr.per-despesa                 = (tt-embarque-corpo-agr.vl-ger-tot-des-rea
                                                             / tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea) * 100
           tt-embarque-corpo-agr.per-imposto                 = (tt-embarque-corpo-agr.vl-ger-tot-imp-rea
                                                             / tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea) * 100
           tt-embarque-corpo-agr.per-ii                      = (tt-embarque-corpo-agr.vl-ger-ii-rea
                                                             / tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea) * 100.
end.

for each tt-embarque-corpo-agr
    break by tt-embarque-corpo-agr.cod-estabel
          BY tt-embarque-corpo-agr.cod-emitente
          by tt-embarque-corpo-agr.embarque :
    
    if  first-of(tt-embarque-corpo-agr.cod-estabel) then do:
        put skip(1).
        disp tt-embarque-corpo-agr.cod-estabel  @ tt-embarque-corpo.cod-estabel
             tt-embarque-corpo-agr.desc-estabel @ tt-embarque-corpo.desc-estabel
             with frame f-estabel.
    end.
    /*EMBARQUE*/
    
    if  tt-param.l-desp-imposto then do: /*Detalha Despesa x Imposto*/
        disp tt-embarque-corpo-agr.embarque @ tt-embarque-corpo.embarque
             tt-embarque-corpo-agr.cod-emitente @ tt-embarque-corpo.cod-emitente
             tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea @ tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea 
             tt-embarque-corpo-agr.vl-ger-mercado-rea @ tt-embarque-corpo.vl-ger-mercado-rea
             tt-embarque-corpo-agr.vl-ger-tot-des-rea @ tt-embarque-corpo.vl-ger-tot-des-rea
             tt-embarque-corpo-agr.vl-ger-tot-imp-rea @ tt-embarque-corpo.vl-ger-tot-imp-rea
             tt-embarque-corpo-agr.per-mercado @ tt-embarque-corpo.per-mercado
             tt-embarque-corpo-agr.per-despesa @ tt-embarque-corpo.per-despesa
             tt-embarque-corpo-agr.per-imposto @ tt-embarque-corpo.per-imposto
             tt-embarque-corpo-agr.per-ii @ tt-embarque-corpo.per-ii
             tt-embarque-corpo-agr.vl-ger-ii-rea @ tt-embarque-corpo.vl-ger-ii-rea
             tt-embarque-corpo-agr.vl-ger-ipi-rea @ tt-embarque-corpo.vl-ger-ipi-rea
             tt-embarque-corpo-agr.vl-ger-ipi-rea @ tt-embarque-corpo.vl-ger-ipi-rea
             tt-embarque-corpo-agr.vl-ger-icms-rea @ tt-embarque-corpo.vl-ger-icms-rea
             tt-embarque-corpo-agr.vl-ger-pis-rea @ tt-embarque-corpo.vl-ger-pis-rea
             tt-embarque-corpo-agr.vl-ger-cofins-rea @ tt-embarque-corpo.vl-ger-cofins-rea
             tt-embarque-corpo-agr.total-vl-frete @ tt-embarque-corpo.total-vl-frete
             tt-embarque-corpo-agr.total-vl-embalagem @ tt-embarque-corpo.total-vl-embalagem
             tt-embarque-corpo-agr.total-vl-seguro @ tt-embarque-corpo.total-vl-seguro
             tt-embarque-corpo-agr.total-vl-outras @ tt-embarque-corpo.total-vl-outras
             with frame f-embarq-resum-1.
        down with frame f-embarq-resum-1.
    end.
    else do:
        disp tt-embarque-corpo-agr.embarque     @ tt-embarque-corpo.embarque
             tt-embarque-corpo-agr.cod-emitente @ tt-embarque-corpo.cod-emitente
             tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea @ tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea
             tt-embarque-corpo-agr.vl-ger-mercado-rea @ tt-embarque-corpo.vl-ger-mercado-rea
             tt-embarque-corpo-agr.vl-ger-tot-des-rea @ tt-embarque-corpo.vl-ger-tot-des-rea
             tt-embarque-corpo-agr.vl-ger-tot-imp-rea @ tt-embarque-corpo.vl-ger-tot-imp-rea
             tt-embarque-corpo-agr.per-mercado @ tt-embarque-corpo.per-mercado
             tt-embarque-corpo-agr.per-despesa @ tt-embarque-corpo.per-despesa
             tt-embarque-corpo-agr.per-imposto @ tt-embarque-corpo.per-imposto
             tt-embarque-corpo-agr.total-vl-frete @ tt-embarque-corpo.total-vl-frete
             tt-embarque-corpo-agr.total-vl-embalagem @ tt-embarque-corpo.total-vl-embalagem
             tt-embarque-corpo-agr.total-vl-seguro @ tt-embarque-corpo.total-vl-seguro
             tt-embarque-corpo-agr.total-vl-outras @ tt-embarque-corpo.total-vl-outras
             with frame f-embarq-resum-2.
        down with frame f-embarq-resum-2.
    end.

    /* ITEM */
    if  tt-param.l-itens then do:
        if can-find(first tt-detalha-item
                    where tt-detalha-item.embarque    = tt-embarque-corpo-agr.embarque
                      and tt-detalha-item.cod-estabel = tt-embarque-corpo-agr.cod-estabel no-lock) then
            put skip(1).
        
        for each  tt-detalha-item
            where tt-detalha-item.embarque     = tt-embarque-corpo-agr.embarque
              and tt-detalha-item.cod-estabel  = tt-embarque-corpo-agr.cod-estabel
            use-index ch-codigo:

            if  i-desp-imposto = 1 then
                assign valor-total-despesa = tt-detalha-item.vl-des-rea
                                           + tt-detalha-item.vl-ii2-rea
                       valor-total-imposto = tt-detalha-item.vl-ipi-rea
                                           + tt-detalha-item.vl-icms-rea
                                           + tt-detalha-item.vl-pis-rea
                                           + tt-detalha-item.vl-cofins-rea.
            else
                assign valor-total-despesa = tt-detalha-item.vl-des-rea
                       valor-total-imposto = tt-detalha-item.vl-ii-rea
                                           + tt-detalha-item.vl-ipi-rea
                                           + tt-detalha-item.vl-icms-rea
                                           + tt-detalha-item.vl-pis-rea
                                           + tt-detalha-item.vl-cofins-rea.

            /*Busca a descri‡Æo do item**/
            FIND FIRST ITEM 
                 WHERE ITEM.it-codigo = tt-detalha-item.it-codigo NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
                ASSIGN c-desc-item = ITEM.desc-item.
            ELSE c-desc-item = "".
            IF tt-param.i-custo-item = 2 THEN
                RUN pi-altera-formato.

            if  l-desp-imposto then do:
                disp tt-detalha-item.it-codigo
                     tt-detalha-item.quantidade
                     tt-detalha-item.un
                     c-desc-item
                     tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
                     tt-detalha-item.vl-mercado-rea
                     valor-total-despesa
                     valor-total-imposto
                     tt-detalha-item.per-mercado-rea
                     tt-detalha-item.per-tot-des-rea
                     tt-detalha-item.per-tot-imp-rea
                     tt-detalha-item.aliquota-ii
                     tt-detalha-item.aliquota-ipi
                     tt-detalha-item.aliquota-pis
                     tt-detalha-item.aliquota-cofins
                     tt-detalha-item.aliquota-icms
                     tt-detalha-item.vl-ii-rea
                     tt-detalha-item.vl-ipi-rea
                     tt-detalha-item.vl-pis-rea
                     tt-detalha-item.vl-cofins-rea
                     tt-detalha-item.vl-icms-rea
                     tt-detalha-item.vl-des-frete
                     tt-detalha-item.vl-des-embalagem
                     tt-detalha-item.vl-des-seguro
                     tt-detalha-item.vl-des-outras
                     with frame f-item-resum.
                down with frame f-item-resum.
            end.
            else do:
                disp tt-detalha-item.it-codigo
                     tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
                     tt-detalha-item.vl-mercado-rea
                     valor-total-despesa
                     valor-total-imposto
                     tt-detalha-item.per-mercado-rea
                     tt-detalha-item.per-tot-des-rea
                     tt-detalha-item.per-tot-imp-rea
                     tt-detalha-item.quantidade
                     tt-detalha-item.un
                     tt-detalha-item.vl-des-frete
                     tt-detalha-item.vl-des-embalagem
                     tt-detalha-item.vl-des-seguro
                     tt-detalha-item.vl-des-outras
                     with frame f-item-resum-1.
                down with frame f-item-resum-1.
            end.
        end.
    end.
    
    /* ESTABELECIMENTO */
    assign tot-est-importado = tot-est-importado
                             + tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea
           tot-est-mercado   = tot-est-mercado
                             + tt-embarque-corpo-agr.vl-ger-mercado-rea
           tot-est-despesa   = tot-est-despesa
                             + tt-embarque-corpo-agr.vl-ger-tot-des-rea
           tot-est-imposto   = tot-est-imposto
                             + tt-embarque-corpo-agr.vl-ger-tot-imp-rea
           tot-est-ii        = tot-est-ii
                             + tt-embarque-corpo-agr.vl-ger-ii-rea
           tot-est-ipi       = tot-est-ipi
                             + tt-embarque-corpo-agr.vl-ger-ipi-rea
           tot-est-pis       = tot-est-pis
                             + tt-embarque-corpo-agr.vl-ger-pis-rea
           tot-est-cofins    = tot-est-cofins
                             + tt-embarque-corpo-agr.vl-ger-cofins-rea
           tot-est-icms      = tot-est-icms
                             + tt-embarque-corpo-agr.vl-ger-icms-rea
           tot-est-frete     = tot-est-frete
                             + tt-embarque-corpo-agr.total-vl-frete
           tot-est-embalagem = tot-est-embalagem
                             + tt-embarque-corpo-agr.total-vl-embalagem
           tot-est-seguro    = tot-est-seguro
                             + tt-embarque-corpo-agr.total-vl-seguro
           tot-est-outras    = tot-est-outras
                             + tt-embarque-corpo-agr.total-vl-outras.
    
    if  last-of (tt-embarque-corpo-agr.cod-estabel) then do:
        assign per-est-mercado = (tot-est-mercado * 100) / tot-est-importado
               per-est-despesa = (tot-est-despesa * 100) / tot-est-importado
               per-est-imposto = (tot-est-imposto * 100) / tot-est-importado
               per-est-ii      = (tot-est-imposto * 100) / tot-est-importado.
         put skip(1).
         put "Total Estabelecimento" at 01.
         disp tot-est-importado
              tot-est-mercado 
              tot-est-despesa
              tot-est-imposto
              tot-est-ii
              tot-est-ipi
              tot-est-pis
              tot-est-cofins
              tot-est-icms
              tot-est-frete
              tot-est-embalagem
              tot-est-seguro
              tot-est-outras
              per-est-mercado
              per-est-despesa
              per-est-imposto
              per-est-ii
              with frame f-tot-gerais.
         assign tot-est-importado   = 0
                tot-est-mercado     = 0
                tot-est-despesa     = 0
                tot-est-imposto     = 0
                tot-est-ii          = 0
                tot-est-ipi         = 0
                tot-est-pis         = 0
                tot-est-cofins      = 0
                tot-est-icms        = 0
                tot-est-frete       = 0
                tot-est-embalagem   = 0
                tot-est-seguro      = 0
                tot-est-outras      = 0
                per-est-mercado     = 0
                per-est-despesa     = 0
                per-est-imposto     = 0
                per-est-ii          = 0
                valor-total-despesa = 0
                valor-total-imposto = 0.
    end.

    /* TOTAL GERAL */
    assign tot-ger-importado = tot-ger-importado
                             + tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea
           tot-ger-mercado   = tot-ger-mercado
                             + tt-embarque-corpo-agr.vl-ger-mercado-rea
           tot-ger-despesa   = tot-ger-despesa
                             + tt-embarque-corpo-agr.vl-ger-tot-des-rea
           tot-ger-imposto   = tot-ger-imposto
                             + tt-embarque-corpo-agr.vl-ger-tot-imp-rea
           tot-ger-ii        = tot-ger-ii
                             + tt-embarque-corpo-agr.vl-ger-ii-rea
           tot-ger-ipi       = tot-ger-ipi
                             + tt-embarque-corpo-agr.vl-ger-ipi-rea
           tot-ger-pis       = tot-ger-pis
                             + tt-embarque-corpo-agr.vl-ger-pis-rea
           tot-ger-cofins    = tot-ger-cofins
                             + tt-embarque-corpo-agr.vl-ger-cofins-rea
           tot-ger-icms      = tot-ger-icms
                             + tt-embarque-corpo-agr.vl-ger-icms-rea
           tot-ger-frete     = tot-ger-frete
                             + tt-embarque-corpo-agr.total-vl-frete
           tot-ger-embalagem = tot-ger-embalagem
                             + tt-embarque-corpo-agr.total-vl-embalagem
           tot-ger-seguro    = tot-ger-seguro
                             + tt-embarque-corpo-agr.total-vl-seguro
           tot-ger-outras    = tot-ger-outras
                             + tt-embarque-corpo-agr.total-vl-outras.
end.

 assign per-ger-mercado = (tot-ger-mercado * 100) / tot-ger-importado
        per-ger-despesa = (tot-ger-despesa * 100) / tot-ger-importado
        per-ger-imposto = (tot-ger-imposto * 100) / tot-ger-importado
        per-ger-ii      = (tot-ger-imposto * 100) / tot-ger-importado.
 put skip(1).
 put "Total Geral" at 01.
 if tt-param.l-desp-imposto then
     disp tot-ger-importado
          tot-ger-mercado 
          tot-ger-despesa
          tot-ger-imposto
          tot-ger-frete
          tot-ger-embalagem
          tot-ger-seguro
          tot-ger-outras
          per-ger-mercado
          per-ger-despesa
          per-ger-imposto
          tot-ger-ii
          tot-ger-ipi
          tot-ger-pis
          tot-ger-cofins
          tot-ger-icms   
          per-ger-ii
          with frame f-tot-geral-1.
 else
     disp tot-ger-importado
          tot-ger-mercado 
          tot-ger-despesa
          tot-ger-imposto
          tot-ger-frete
          tot-ger-embalagem
          tot-ger-seguro
          tot-ger-outras
          per-ger-mercado
          per-ger-despesa
          per-ger-imposto
          with frame f-tot-geral-2.

/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIM0665C 2.00.00.015}  /*** 010015 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esim0665c MIM}
&ENDIF

/******************************************************************************
**
**       PROGRAMA:  ESIM0665C.P
**
**       DATA....:  FEVEREIRO DE 2003
**
**       OBJETIVO:  Custo Real de Importa‡Æo / Por Exportador - Resumido
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

for each tt-embarque-corpo:
    find first tt-embarque-corpo-agr
         where tt-embarque-corpo-agr.embarque    = tt-embarque-corpo.embarque
           and tt-embarque-corpo-agr.cod-estabel = tt-embarque-corpo.cod-estabel no-error.
    if  not avail tt-embarque-corpo-agr then do:
        create tt-embarque-corpo-agr.
        assign tt-embarque-corpo-agr.embarque      = tt-embarque-corpo.embarque
               tt-embarque-corpo-agr.cod-estabel   = tt-embarque-corpo.cod-estabel
               tt-embarque-corpo-agr.desc-estabel  = tt-embarque-corpo.desc-estabel
               tt-embarque-corpo-agr.cod-emitente  = tt-embarque-corpo.cod-emitente
               tt-embarque-corpo-agr.desc-emitente = tt-embarque-corpo.desc-emitente.
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
           tt-embarque-corpo-agr.vl-ger-pis-rea              = tt-embarque-corpo-agr.vl-ger-pis-rea
                                                             + tt-embarque-corpo.vl-ger-pis-rea
           tt-embarque-corpo-agr.vl-ger-cofins-rea           = tt-embarque-corpo-agr.vl-ger-cofins-rea
                                                             + tt-embarque-corpo.vl-ger-cofins-rea
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
    break by tt-embarque-corpo-agr.cod-emitente
          by tt-embarque-corpo-agr.cod-estabel
          by tt-embarque-corpo-agr.embarque:

    if  first-of (tt-embarque-corpo-agr.cod-emitente) then do:
        put skip(1).
        disp tt-embarque-corpo-agr.cod-emitente  @ tt-embarque-corpo.cod-emitente
             tt-embarque-corpo-agr.desc-emitente @ tt-embarque-corpo.desc-emitente
             with frame f-export.
    end.
    
    /*EMBARQUE*/
    if  tt-param.l-desp-imposto then do: /*Detalha Despesa x Imposto*/
        disp tt-embarque-corpo-agr.cod-estabel @ tt-embarque-corpo.cod-estabel 
             tt-embarque-corpo-agr.embarque @ tt-embarque-corpo.embarque 
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
             with frame f-export-resum-1.
        down with frame f-export-resum-1.
    end.
    else do:
        disp tt-embarque-corpo-agr.cod-estabel @ tt-embarque-corpo.cod-estabel
             tt-embarque-corpo-agr.embarque @ tt-embarque-corpo.embarque
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
             with frame f-export-resum-2.
        down with frame f-export-resum-2.
    end.

    /* ITEM */
    if  tt-param.l-itens then do:
        if can-find(first tt-detalha-item
                    where tt-detalha-item.embarque    = tt-embarque-corpo-agr.embarque
                      and tt-detalha-item.cod-estabel = tt-embarque-corpo-agr.cod-estabel) then do:
            put skip(1).
            if tt-param.l-desp-imposto then
                put "Al¡quotas"             to 131 skip
                    "---------------------" to 131 skip.
        end.
        
        for each  tt-detalha-item
            where tt-detalha-item.embarque    = tt-embarque-corpo-agr.embarque
              and tt-detalha-item.cod-estabel = tt-embarque-corpo-agr.cod-estabel
            use-index ch-codigo:

            if  tt-param.i-desp-imposto = 1 then
                assign valor-total-despesa = tt-detalha-item.vl-tot-despesa
                                           + tt-detalha-item.vl-ii2-rea
                       valor-total-imposto = tt-detalha-item.vl-ipi-rea
                                           + tt-detalha-item.vl-icms-rea
                                           + tt-detalha-item.vl-pis-rea
                                           + tt-detalha-item.vl-cofins-rea.
            else
                assign valor-total-despesa = tt-detalha-item.vl-tot-despesa
                       valor-total-imposto = tt-detalha-item.vl-ii-rea
                                           + tt-detalha-item.vl-ipi-rea
                                           + tt-detalha-item.vl-icms-rea
                                           + tt-detalha-item.vl-pis-rea
                                           + tt-detalha-item.vl-cofins-rea.
            
                IF tt-param.i-custo-item = 2 THEN
                    RUN pi-altera-formato.
                if  tt-param.l-desp-imposto then do:
                    disp tt-detalha-item.it-codigo
                         tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
                         tt-detalha-item.vl-mercado-rea
                         valor-total-despesa
                         valor-total-imposto
                         tt-detalha-item.per-mercado-rea
                         tt-detalha-item.per-tot-des-rea
                         tt-detalha-item.per-tot-imp-rea
                         tt-detalha-item.per-ii-rea
                         tt-detalha-item.aliquota-ii
                         tt-detalha-item.aliquota-ipi
                         tt-detalha-item.aliquota-pis
                         tt-detalha-item.aliquota-cofins
                         tt-detalha-item.aliquota-icms
                         tt-detalha-item.quantidade
                         tt-detalha-item.un
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
                ELSE DO:
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
                END.
        end.
    end.
    
    /* EXPORTADOR */
    assign tot-exp-importado = tot-exp-importado
                             + tt-embarque-corpo-agr.vl-ger-tot-com-ipi-icms-rea
           tot-exp-mercado   = tot-exp-mercado
                             + tt-embarque-corpo-agr.vl-ger-mercado-rea
           tot-exp-despesa   = tot-exp-despesa
                             + tt-embarque-corpo-agr.vl-ger-tot-des-rea
           tot-exp-imposto   = tot-exp-imposto
                             + tt-embarque-corpo-agr.vl-ger-tot-imp-rea
           tot-exp-ii        = tot-exp-ii
                             + tt-embarque-corpo-agr.vl-ger-ii-rea
           tot-exp-ipi       = tot-exp-ipi
                             + tt-embarque-corpo-agr.vl-ger-ipi-rea
           tot-exp-icms      = tot-exp-icms
                             + tt-embarque-corpo-agr.vl-ger-icms-rea
           tot-exp-pis       = tot-exp-pis
                             + tt-embarque-corpo-agr.vl-ger-pis-rea
           tot-exp-cofins    = tot-exp-cofins
                             + tt-embarque-corpo-agr.vl-ger-cofins-rea
           tot-exp-frete     = tot-exp-frete
                             + tt-embarque-corpo-agr.total-vl-frete
           tot-exp-embalagem = tot-exp-embalagem
                             + tt-embarque-corpo-agr.total-vl-embalagem
           tot-exp-seguro    = tot-exp-seguro
                             + tt-embarque-corpo-agr.total-vl-seguro
           tot-exp-outras    = tot-exp-outras
                             + tt-embarque-corpo-agr.total-vl-outras.
    if  last-of (tt-embarque-corpo-agr.cod-emitente) then do:
        assign per-exp-mercado = (tot-exp-mercado * 100) / tot-exp-importado
               per-exp-despesa = (tot-exp-despesa * 100) / tot-exp-importado
               per-exp-imposto = (tot-exp-imposto * 100) / tot-exp-importado
               per-exp-ii      = (tot-exp-imposto * 100) / tot-exp-importado.
         put skip(1).
         put "Total Exportador" at 01.
         disp tot-exp-importado     @   tot-est-importado
              tot-exp-mercado       @   tot-est-mercado  
              tot-exp-despesa       @   tot-est-despesa  
              tot-exp-imposto       @   tot-est-imposto  
              tot-exp-ii            @   tot-est-ii       
              tot-exp-ipi           @   tot-est-ipi      
              tot-exp-icms          @   tot-est-icms     
              tot-exp-pis           @   tot-est-pis     
              tot-exp-cofins        @   tot-est-cofins     
              tot-exp-frete         @   tot-est-frete    
              tot-exp-embalagem     @   tot-est-embalagem
              tot-exp-seguro        @   tot-est-seguro   
              tot-exp-outras        @   tot-est-outras   
              per-exp-mercado       @   per-est-mercado  
              per-exp-despesa       @   per-est-despesa  
              per-exp-imposto       @   per-est-imposto  
              per-exp-ii            @   per-est-ii       
              with frame f-tot-gerais.
         assign tot-exp-importado   = 0
                tot-exp-mercado     = 0
                tot-exp-despesa     = 0
                tot-exp-imposto     = 0
                tot-exp-ii          = 0
                tot-exp-ipi         = 0
                tot-exp-icms        = 0
                tot-exp-pis         = 0
                tot-exp-cofins      = 0
                tot-exp-frete       = 0
                tot-exp-embalagem   = 0
                tot-exp-seguro      = 0
                tot-exp-outras      = 0
                per-exp-mercado     = 0
                per-exp-despesa     = 0
                per-exp-imposto     = 0
                per-exp-ii          = 0
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
           tot-ger-icms      = tot-ger-icms
                             + tt-embarque-corpo-agr.vl-ger-icms-rea
           tot-ger-pis       = tot-ger-pis
                             + tt-embarque-corpo-agr.vl-ger-pis-rea
           tot-ger-cofins    = tot-ger-cofins
                             + tt-embarque-corpo-agr.vl-ger-cofins-rea
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

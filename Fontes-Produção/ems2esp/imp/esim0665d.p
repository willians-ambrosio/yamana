/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIM0665D 2.00.00.014}  /*** 010014 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esim0665d MIM}
&ENDIF

/******************************************************************************
**
**       PROGRAMA:  ESIM0665D.P
**
**       DATA....:  FEVEREIRO DE 2003
**
**       OBJETIVO:  Custo Real de Importa‡Æo / Por Item - Detalhado/Resumido
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

def var vl-item-ii-rea     like tt-detalha-item.vl-ii-rea  no-undo.
DEF VAR i-numero-itens     AS   INTEGER                    NO-UNDO.
DEF VAR de-unit-medio-item LIKE tot-item-importado         NO-UNDO.
DEF VAR de-tot-qtde-item   LIKE tt-detalha-item.quantidade NO-UNDO.

find first tt-param.
IF tt-param.i-custo-item = 2 THEN
    RUN pi-altera-formato.

if  tt-param.l-desp-imposto then do: /*DETALHADO*/
    for each tt-detalha-item
        use-index ch-item
        break by tt-detalha-item.it-codigo:

        if  first-of (tt-detalha-item.it-codigo) then do:
            disp tt-detalha-item.it-codigo
                 tt-detalha-item.descricao
                 with frame f-item.
        end.

        IF tt-detalha-item.i-numero-itens <> 0 THEN
            ASSIGN i-numero-itens   = tt-detalha-item.i-numero-itens.

        IF tt-detalha-item.tot-quantidade <> 0 THEN
            ASSIGN de-tot-qtde-item = tt-detalha-item.tot-quantidade.
        
        if  tt-param.i-custo-item = 1 then /*Despesa*/
            assign vl-item-ii-rea = tt-detalha-item.vl-ii2-rea.
        else
            assign vl-item-ii-rea = tt-detalha-item.vl-ii-rea.
            
        /* ESTABELECIMENTO */
        DISP tt-detalha-item.cod-estabel                        
             tt-detalha-item.embarque                            
             tt-detalha-item.un                                  
             tt-detalha-item.quantidade                          
             tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea    
             tt-detalha-item.vl-mercado-rea                      
             tt-detalha-item.vl-tot-des-rea                      
             tt-detalha-item.vl-tot-imp-rea                      
             tt-detalha-item.per-mercado-rea                     
             tt-detalha-item.per-tot-des-rea                     
             tt-detalha-item.per-tot-imp-rea                     
             tt-detalha-item.per-ii-rea                         
             tt-detalha-item.aliquota-ii                        
             tt-detalha-item.aliquota-ipi                    
             tt-detalha-item.aliquota-pis      /* aliquota-icms*/              
             tt-detalha-item.aliquota-cofins   /* aliquota-pis*/                 
             tt-detalha-item.aliquota-icms     /* aliquota-cofins*/            
             vl-item-ii-rea @ tt-detalha-item.vl-ii-rea        
             tt-detalha-item.vl-ipi-rea                          
             tt-detalha-item.vl-pis-rea                       
             tt-detalha-item.vl-cofins-rea                     
             tt-detalha-item.vl-icms-rea                     
             tt-detalha-item.vl-des-frete                    
             tt-detalha-item.vl-des-embalagem                
             tt-detalha-item.vl-des-seguro                    
             tt-detalha-item.vl-des-outras                    
           with frame f-item-resumido-1.
        down with frame f-item-resumido-1. 
  
        /* TOTAL ITEM */
        assign tot-item-importado = tot-item-importado
                                  + tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
               tot-item-mercado   = tot-item-mercado
                                  + tt-detalha-item.vl-mercado-rea
               tot-item-despesa   = tot-item-despesa
                                  + tt-detalha-item.vl-tot-des-rea
               tot-item-imposto   = tot-item-imposto
                                  + tt-detalha-item.vl-tot-imp-rea
               tot-item-ipi       = tot-item-ipi
                                  + tt-detalha-item.vl-ipi-rea
               tot-item-icms      = tot-item-icms
                                  + tt-detalha-item.vl-icms-rea
               tot-item-pis       = tot-item-pis
                                  + tt-detalha-item.vl-pis-rea
               tot-item-cofins    = tot-item-cofins
                                  + tt-detalha-item.vl-cofins-rea
               tot-item-frete     = tot-item-frete
                                  + tt-detalha-item.vl-des-frete
               tot-item-embalagem = tot-item-embalagem
                                  + tt-detalha-item.vl-des-embalagem
               tot-item-seguro    = tot-item-seguro
                                  + tt-detalha-item.vl-des-seguro
               tot-item-outras    = tot-item-outras
                                  + tt-detalha-item.vl-des-outras.

        IF  tt-param.i-custo-item = 1 THEN /*Despesa*/
            ASSIGN tot-item-ii    = tot-item-ii
                                  + tt-detalha-item.vl-ii2-rea.
        ELSE 
            ASSIGN tot-item-ii    = tot-item-ii
                                  + tt-detalha-item.vl-ii-rea.

        if  last-of (tt-detalha-item.it-codigo) then do:

            assign per-item-mercado   = (tot-item-mercado * 100) / tot-item-importado
                   per-item-despesa   = (tot-item-despesa * 100) / tot-item-importado
                   per-item-imposto   = (tot-item-imposto * 100) / tot-item-importado
                   per-item-ii        = (tot-item-imposto * 100) / tot-item-importado
                   de-unit-medio-item = (tot-item-importado * i-numero-itens) / de-tot-qtde-item.

            put skip(1).
            put "Total Item " at 01 skip.
            disp de-unit-medio-item    @   tot-est-importado
                 tot-item-mercado      @   tot-est-mercado
                 tot-item-despesa      @   tot-est-despesa
                 tot-item-imposto      @   tot-est-imposto
                 per-item-mercado      @   per-est-mercado
                 per-item-despesa      @   per-est-despesa
                 per-item-imposto      @   per-est-imposto
                 per-item-ii           @   per-est-ii
                 tot-item-ii           @   tot-est-ii
                 tot-item-ipi          @   tot-est-ipi
                 tot-item-pis          @   tot-est-pis
                 tot-item-cofins       @   tot-est-cofins
                 tot-item-icms         @   tot-est-icms
                 tot-item-frete        @   tot-est-frete
                 tot-item-embalagem    @   tot-est-embalagem
                 tot-item-seguro       @   tot-est-seguro
                 tot-item-outras       @   tot-est-outras
                 with frame f-tot-gerais.
             
            assign tot-item-importado = 0
                   tot-item-mercado   = 0
                   tot-item-despesa   = 0
                   tot-item-imposto   = 0
                   tot-item-ii        = 0
                   tot-item-ipi       = 0
                   tot-item-icms      = 0
                   tot-item-pis       = 0
                   tot-item-cofins    = 0
                   tot-item-frete     = 0
                   tot-item-embalagem = 0
                   tot-item-seguro    = 0
                   tot-item-outras    = 0
                   per-item-mercado   = 0
                   per-item-despesa   = 0
                   per-item-imposto   = 0
                   de-unit-medio-item = 0
                   de-tot-qtde-item   = 0
                   i-numero-itens     = 0.
        end.

        /* TOTAL GERAL */
        assign tot-ger-importado = tot-ger-importado
                                  + tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
               tot-ger-mercado   = tot-ger-mercado
                                  + tt-detalha-item.vl-mercado-rea
               tot-ger-despesa   = tot-ger-despesa
                                  + tt-detalha-item.vl-tot-des-rea
               tot-ger-imposto   = tot-ger-imposto
                                  + tt-detalha-item.vl-tot-imp-rea
               tot-ger-ii        = tot-ger-ii
                                  + tt-detalha-item.vl-ii-rea
               tot-ger-ipi       = tot-ger-ipi
                                  + tt-detalha-item.vl-ipi-rea
               tot-ger-icms      = tot-ger-icms
                                  + tt-detalha-item.vl-icms-rea
               tot-ger-pis       = tot-ger-pis
                                  + tt-detalha-item.vl-pis-rea
               tot-ger-cofins    = tot-ger-cofins
                                  + tt-detalha-item.vl-cofins-rea
               tot-ger-frete     = tot-ger-frete
                                  + tt-detalha-item.vl-des-frete
               tot-ger-embalagem = tot-ger-embalagem
                                  + tt-detalha-item.vl-des-embalagem
               tot-ger-seguro    = tot-ger-seguro
                                  + tt-detalha-item.vl-des-seguro
               tot-ger-outras    = tot-ger-outras
                                  + tt-detalha-item.vl-des-outras.
    end.
    
    assign per-ger-mercado = (tot-ger-mercado * 100) / tot-ger-importado
           per-ger-despesa = (tot-ger-despesa * 100) / tot-ger-importado
           per-ger-imposto = (tot-ger-imposto * 100) / tot-ger-importado
           per-ger-ii      = (tot-ger-imposto * 100) / tot-ger-importado.
    put skip(1).
    put "Total Geral" at 01 skip.
    disp tot-ger-importado   
         tot-ger-mercado     
         tot-ger-despesa     
         tot-ger-imposto     
         per-ger-mercado     
         per-ger-despesa     
         per-ger-imposto     
         per-ger-ii          
         tot-ger-ii          
         tot-ger-ipi         
         tot-ger-pis         
         tot-ger-cofins      
         tot-ger-icms        
         tot-ger-frete       
         tot-ger-embalagem   
         tot-ger-seguro      
         with frame f-tot-geral-1.  

end.

else do: /*RESUMIDO*/
    for each tt-detalha-item
        use-index ch-item
        break by tt-detalha-item.it-codigo:

        if  first-of (tt-detalha-item.it-codigo) then
            disp tt-detalha-item.it-codigo
                 tt-detalha-item.descricao
                 with frame f-item.

        IF tt-detalha-item.i-numero-itens <> 0 THEN
            ASSIGN i-numero-itens   = tt-detalha-item.i-numero-itens.

        IF tt-detalha-item.tot-quantidade <> 0 THEN
            ASSIGN de-tot-qtde-item = tt-detalha-item.tot-quantidade.

        /* ESTABELECIMENTO */
        disp tt-detalha-item.cod-estabel
             tt-detalha-item.embarque
             tt-detalha-item.un
             tt-detalha-item.quantidade
             tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
             tt-detalha-item.vl-mercado-rea
             tt-detalha-item.vl-tot-des-rea
             tt-detalha-item.vl-tot-imp-rea
             tt-detalha-item.per-mercado-rea
             tt-detalha-item.per-tot-des-rea
             tt-detalha-item.per-tot-imp-rea
             with frame f-item-resumido-2.
        down with frame f-item-resumido-2.
        
        /* TOTAL ITEM */
        assign tot-item-importado = tot-item-importado
                                  + tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
               tot-item-mercado   = tot-item-mercado
                                  + tt-detalha-item.vl-mercado-rea
               tot-item-despesa   = tot-item-despesa
                                  + tt-detalha-item.vl-tot-des-rea
               tot-item-imposto   = tot-item-imposto
                                  + tt-detalha-item.vl-tot-imp-rea.

        if  last-of (tt-detalha-item.it-codigo) then do:
            assign per-item-mercado   = (tot-item-mercado * 100) / tot-item-importado
                   per-item-despesa   = (tot-item-despesa * 100) / tot-item-importado
                   per-item-imposto   = (tot-item-imposto * 100) / tot-item-importado
                   de-unit-medio-item = (tot-item-importado * i-numero-itens) / de-tot-qtde-item.

            put skip(1).
            put "Total Item " at 01 skip.
            disp de-unit-medio-item @ tot-item-importado
                 tot-item-mercado
                 tot-item-despesa
                 tot-item-imposto
                 per-item-mercado
                 per-item-despesa
                 per-item-imposto
                 with frame f-tot-gerais-item.

            assign tot-item-importado = 0
                   tot-item-mercado   = 0
                   tot-item-despesa   = 0
                   tot-item-imposto   = 0
                   per-item-mercado   = 0
                   per-item-despesa   = 0
                   per-item-imposto   = 0
                   de-unit-medio-item = 0
                   i-numero-itens     = 0
                   de-tot-qtde-item   = 0.
        end.

        /* TOTAL GERAL */
        assign tot-ger-importado = tot-ger-importado
                                  + tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
               tot-ger-mercado   = tot-ger-mercado
                                  + tt-detalha-item.vl-mercado-rea
               tot-ger-despesa   = tot-ger-despesa
                                  + tt-detalha-item.vl-tot-des-rea
               tot-ger-imposto   = tot-ger-imposto
                                  + tt-detalha-item.vl-tot-imp-rea.
    end.
    
    assign per-ger-mercado = (tot-ger-mercado * 100) / tot-ger-importado
           per-ger-despesa = (tot-ger-despesa * 100) / tot-ger-importado
           per-ger-imposto = (tot-ger-imposto * 100) / tot-ger-importado.
    put skip(1).
    put "Total Geral" at 01 skip.
    disp tot-ger-importado
         tot-ger-mercado 
         tot-ger-despesa
         tot-ger-imposto
         per-ger-mercado
         per-ger-despesa
         per-ger-imposto
         with frame f-tot-geral-item.

end.

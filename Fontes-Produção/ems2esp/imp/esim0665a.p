/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIM0665A 2.00.00.019}  /*** 010019 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esim0665a MIM}
&ENDIF

/******************************************************************************
**
**       PROGRAMA:  ESIM0665A.P
**
**       DATA....:  FEVEREIRO DE 2003
**
**       OBJETIVO:  Custo Real de Importaá∆o / Por Embarque - Detalhado
**
**       VERSAO..:  1.00.000 - Ivonei Vock
**
******************************************************************************/

{cdp/cdcfgmat.i}
{imp/esim0665.i} /* Definiá∆o tt-param          **
               ** Definiá∆o tt-detalha-item   **
               ** Definiá∆o tt-embarque-cabec **
               ** Definiá∆o tt-embarque-corpo */
{imp/esim0665.i1} /* Definiá∆o das Frames */

def input param table for tt-param.
def input param table for tt-detalha-item.
def input param table for tt-embarque-cabec.
def input param table for tt-embarque-corpo.
def input param table for tt-embarque-fatura.
def input param table for tt-detalha-despesa.
def input param table for tt-detalha-despesa-aux.
def input param table for tt-embarque-rel.
def input param table for tt-nf-comp.
DEF INPUT PARAM TABLE FOR tt-embarque-nota-mae.

def var var-despesa-orc like tt-detalha-despesa.vl-des-orc.
def var per-despesa-orc like tt-detalha-despesa.per-des-orc.
def var var-despesa-rea like tt-detalha-despesa.vl-des-rea.
def var per-despesa-rea like tt-detalha-despesa.per-des-rea.
DEF VAR l-imprime       AS   LOGICAL.
DEF VAR de-tot-desp     LIKE tt-detalha-despesa.vl-des-rea NO-UNDO.
DEF VAR l-nota-mae      AS LOGICAL NO-UNDO.
find first tt-param.

for each tt-embarque-corpo
    break by tt-embarque-corpo.embarque:
    
    put skip(1).
    run display-cabecalho. /*Apresenta o Cabeáalho*/

    for each  tt-embarque-fatura
        where tt-embarque-fatura.embarque    = tt-embarque-corpo.embarque
          and tt-embarque-fatura.cod-estabel = tt-embarque-corpo.cod-estabel:
        disp tt-embarque-fatura.nr-fatura  
             tt-embarque-fatura.parcela    
             tt-embarque-fatura.dt-venc-fat
             WITH FRAME f-fatura.
        down with frame f-fatura.
    end.
    put skip(1).
    FOR EACH tt-embarque-nota-mae 
       WHERE tt-embarque-nota-mae.embarque    = tt-embarque-corpo.embarque
         AND tt-embarque-nota-mae.cod-estabel = tt-embarque-corpo.cod-estabel:
        ASSIGN l-nota-mae = YES.
        disp tt-embarque-nota-mae.nr-nota-fis
             tt-embarque-nota-mae.dt-emissao
             tt-embarque-nota-mae.nr-di
             tt-embarque-nota-mae.dt-di
             WITH FRAME f-nota-fiscal-2.
    END.
    IF l-nota-mae THEN
        disp tt-embarque-corpo.nr-nota-fis 
             tt-embarque-corpo.dt-emissao  
             tt-embarque-corpo.nr-di       
             tt-embarque-corpo.dt-di       
             WITH FRAME f-nota-fiscal-3.
    ELSE
        disp tt-embarque-corpo.nr-nota-fis 
             tt-embarque-corpo.dt-emissao  
             tt-embarque-corpo.nr-di       
             tt-embarque-corpo.dt-di       
             WITH FRAME f-nota-fiscal.
    PUT SKIP(1).
    ASSIGN l-nota-mae = NO.
    
    IF  tt-param.l-nf-compl THEN DO: /*Considera Nota Complementar*/
        FOR EACH  tt-nf-comp
            WHERE tt-nf-comp.embarque    = tt-embarque-corpo.embarque
              AND tt-nf-comp.cod-estabel = tt-embarque-corpo.cod-estabel
            BREAK BY tt-nf-comp.nr-nf-comp:

            ASSIGN l-imprime = YES.
            IF LAST-OF(tt-nf-comp.nr-nf-comp) THEN DO:
                DISP tt-nf-comp.nr-nf-comp
                     tt-nf-comp.dt-emis-comp
                     WITH FRAME f-nf-compl.
                DOWN WITH FRAME f-nf-compl.
            END.
        END.
        IF  l-imprime = NO THEN DO:
            VIEW FRAME f-nf-compl.
            ASSIGN l-imprime = NO.
        END.
        ELSE PUT SKIP(1).
    END.


    /*if  tt-param.l-desemb-parc then do:*/ /*Retirado esse comando, pois s¢ ir† criar tt-embarque-rel se esse parametro for v†lido ou se for nota m∆e*/
    for each  tt-embarque-rel
        where tt-embarque-rel.embarque    = tt-embarque-corpo.embarque
          and tt-embarque-rel.cod-estabel = tt-embarque-corpo.cod-estabel:
        ASSIGN l-imprime = YES.
        disp tt-embarque-rel.embarque-rel
             tt-embarque-rel.nr-nota-rel
             tt-embarque-rel.dt-emis-rel
             tt-embarque-rel.nr-di-rel
             tt-embarque-rel.dt-di-rel
             WITH FRAME f-embarq-rel.
        down with frame f-embarq-rel.
    end.
    IF  l-imprime = NO THEN DO:
        VIEW FRAME f-nf-compl.
        ASSIGN l-imprime = NO.
    END.
    ELSE PUT SKIP(1).
    /*end.*/
    
    
    IF tt-param.l-desp-imp    AND
       NOT tt-param.l-orc-rea THEN DO:
        /* Impress∆o do cabeáalho */
        DISP tt-embarque-corpo.vl-ger-mercado-rea
             tt-embarque-corpo.per-ger-mercado-rea
             WITH FRAME f-cab-realizado.
        
        IF tt-param.i-desp-imposto = 1 THEN  /* Despesa */
            disp tt-embarque-corpo.vl-ger-ipi-rea
                 tt-embarque-corpo.per-ger-ipi-rea
                 tt-embarque-corpo.vl-ger-pis-rea
                 tt-embarque-corpo.per-ger-pis-rea 
                 tt-embarque-corpo.vl-ger-cofins-rea  
                 tt-embarque-corpo.per-ger-cofins-rea 
                 tt-embarque-corpo.vl-ger-icms-rea
                 tt-embarque-corpo.per-ger-icms-rea
                 WITH FRAME f-ipi-icms-realizado.
        ELSE /* Imposto */
            disp tt-embarque-corpo.vl-ger-ii-rea 
                 tt-embarque-corpo.per-ger-ii-rea
                 tt-embarque-corpo.vl-ger-ipi-rea
                 tt-embarque-corpo.per-ger-ipi-rea
                 tt-embarque-corpo.vl-ger-pis-rea
                 tt-embarque-corpo.per-ger-pis-rea
                 tt-embarque-corpo.vl-ger-cofins-rea
                 tt-embarque-corpo.per-ger-cofins-rea
                 tt-embarque-corpo.vl-ger-icms-rea
                 tt-embarque-corpo.per-ger-icms-rea
                 WITH FRAME f-ipi-icms-realizado2.
        
        /* Total de Impostos */ 
            
        disp tt-embarque-corpo.vl-ger-tot-imp-rea
             tt-embarque-corpo.per-ger-tot-imp-rea
             WITH FRAME f-tot-imp-realizado.
                           
        if tt-param.i-desp-imposto = 1 then /*Despesa*/
            disp tt-embarque-corpo.vl-ger-ii2-rea
                 tt-embarque-corpo.per-ger-ii2-rea
                 with frame f-despesa-realizado.
        
        /* Impress∆o das despesas */
        for each tt-detalha-despesa-aux
           where tt-detalha-despesa-aux.embarque      = tt-embarque-corpo.embarque
             and tt-detalha-despesa-aux.cod-estabel   = tt-embarque-corpo.cod-estabel
           break by tt-detalha-despesa-aux.cod-despesa:

           IF LAST-OF(tt-detalha-despesa-aux.cod-despesa) THEN DO:
               disp  tt-detalha-despesa-aux.desc-despesa
                     tt-detalha-despesa-aux.vl-des-rea
                     tt-detalha-despesa-aux.per-des-rea
                     with frame f-det-desp-imp.
                down with frame f-det-desp-imp.
                ASSIGN de-tot-desp = 0.
           END.
        end.
        
        /* Total */ 
        disp tt-embarque-corpo.vl-ger-tot-des-rea
             tt-embarque-corpo.per-ger-tot-des-rea
             tt-embarque-corpo.vl-ger-tot-imp-des-rea
             tt-embarque-corpo.per-ger-tot-imp-des-rea
             tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea
             tt-embarque-corpo.per-ger-tot-com-ipi-icms-rea
             tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea
             tt-embarque-corpo.per-ger-tot-sem-ipi-icms-rea
             WITH FRAME f-tot-realizado.
    END.

    if  tt-param.l-orc-real then do: /*Oráado x Realizado*/
        disp tt-embarque-corpo.vl-ger-mercado-orc
             tt-embarque-corpo.per-ger-mercado-orc
             tt-embarque-corpo.vl-ger-mercado-rea
             tt-embarque-corpo.per-ger-mercado-rea
             WITH FRAME f-cab-orc-real.
        
        /*IMPOSTOS*/
        if  tt-param.l-desp-imposto then do: /*Detalha Despesa-Imposto*/
            if  tt-param.i-desp-imposto = 2 then /*Imposto*/
                disp tt-embarque-corpo.vl-ger-ii-orc
                     tt-embarque-corpo.per-ger-ii-orc
                     tt-embarque-corpo.vl-ger-ii-rea
                     tt-embarque-corpo.per-ger-ii-rea
                     WITH FRAME f-imposto.

            disp tt-embarque-corpo.vl-ger-ipi-orc
                 tt-embarque-corpo.per-ger-ipi-orc
                 tt-embarque-corpo.vl-ger-ipi-rea
                 tt-embarque-corpo.per-ger-ipi-rea
                 tt-embarque-corpo.vl-ger-pis-orc
                 tt-embarque-corpo.per-ger-pis-orc
                 tt-embarque-corpo.vl-ger-pis-rea
                 tt-embarque-corpo.per-ger-pis-rea
                 tt-embarque-corpo.vl-ger-cofins-orc
                 tt-embarque-corpo.per-ger-cofins-orc
                 tt-embarque-corpo.vl-ger-cofins-rea
                 tt-embarque-corpo.per-ger-cofins-rea
                 tt-embarque-corpo.vl-ger-icms-orc
                 tt-embarque-corpo.per-ger-icms-orc
                 tt-embarque-corpo.vl-ger-icms-rea
                 tt-embarque-corpo.per-ger-icms-rea
                 WITH FRAME f-ipi-icms.
        end.

        disp tt-embarque-corpo.vl-ger-tot-imp-orc
             tt-embarque-corpo.per-ger-tot-imp-orc
             tt-embarque-corpo.vl-ger-tot-imp-rea
             tt-embarque-corpo.per-ger-tot-imp-rea
             WITH FRAME f-tot-imp.

        /*DESPESAS*/
        if  tt-param.l-desp-imposto then do: /*Detalha Despesa-Imposto*/
            if  tt-param.i-desp-imposto = 1 then do: /*Despesa*/
                
                disp tt-embarque-corpo.vl-ger-ii2-orc
                     tt-embarque-corpo.per-ger-ii2-orc
                     tt-embarque-corpo.vl-ger-ii2-rea
                     tt-embarque-corpo.per-ger-ii2-rea
                     with frame f-despesa.

                for each tt-detalha-despesa-aux
                   where tt-detalha-despesa-aux.embarque      = tt-embarque-corpo.embarque
                     and tt-detalha-despesa-aux.cod-estabel   = tt-embarque-corpo.cod-estabel
                   break by tt-detalha-despesa-aux.cod-despesa:

                   ASSIGN de-tot-desp = de-tot-desp + tt-detalha-despesa-aux.vl-des-rea .

                   IF LAST-OF(tt-detalha-despesa-aux.cod-despesa) THEN DO:
                        disp tt-detalha-despesa-aux.desc-despesa
                             tt-detalha-despesa-aux.vl-des-orc
                             tt-detalha-despesa-aux.per-des-orc
                             de-tot-desp @ tt-detalha-despesa-aux.vl-des-rea 
                             tt-detalha-despesa-aux.per-des-rea
                             with frame f-det-desp-i2.
                        down with frame f-det-desp-i2.
                        ASSIGN de-tot-desp = 0.
                   END.
                end.
            END.
            ELSE DO: /*Imposto*/
                for each tt-detalha-despesa-aux
                   where tt-detalha-despesa-aux.embarque      = tt-embarque-corpo.embarque
                     and tt-detalha-despesa-aux.cod-estabel   = tt-embarque-corpo.cod-estabel
                     AND tt-detalha-despesa-aux.vl-des-rea    <> tt-embarque-corpo.vl-ger-ii-rea
                   break by tt-detalha-despesa-aux.cod-despesa:
    
                   IF LAST-OF(tt-detalha-despesa-aux.cod-desp) THEN DO:
                        disp tt-detalha-despesa-aux.desc-despesa
                             tt-detalha-despesa-aux.vl-des-orc
                             tt-detalha-despesa-aux.per-des-orc
                             tt-detalha-despesa-aux.vl-des-rea 
                             tt-detalha-despesa-aux.per-des-rea
                             with frame f-det-desp-i2.
                        down with frame f-det-desp-i2.
                        ASSIGN de-tot-desp = 0.
                   END.
                end.
            END.
        end.
        
        disp tt-embarque-corpo.vl-ger-tot-des-orc
             tt-embarque-corpo.per-ger-tot-des-orc
             tt-embarque-corpo.vl-ger-tot-des-rea
             tt-embarque-corpo.per-ger-tot-des-rea
             tt-embarque-corpo.vl-ger-tot-imp-des-orc
             tt-embarque-corpo.per-ger-tot-imp-des-orc
             tt-embarque-corpo.vl-ger-tot-imp-des-rea
             tt-embarque-corpo.per-ger-tot-imp-des-rea
             tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc
             tt-embarque-corpo.per-ger-tot-com-ipi-icms-orc
             tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea
             tt-embarque-corpo.per-ger-tot-com-ipi-icms-rea
             tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-orc
             tt-embarque-corpo.per-ger-tot-sem-ipi-icms-orc
             tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea
             tt-embarque-corpo.per-ger-tot-sem-ipi-icms-rea
             WITH FRAME f-tot-orc-real.
    end.

    if  tt-param.l-itens then do: /*Detalha Item*/
        for each  tt-detalha-item
            where tt-detalha-item.embarque     = tt-embarque-corpo.embarque
              and tt-detalha-item.cod-estabel  = tt-embarque-corpo.cod-estabel
            use-index ch-codigo:

            disp tt-detalha-item.it-codigo
                 tt-detalha-item.descricao
                 tt-detalha-item.quantidade
                 tt-detalha-item.un
                 WITH FRAME f-det-item.
            down with frame f-det-item.
           
            IF tt-param.i-custo-item = 2 THEN
                RUN pi-altera-formato.

            if  tt-param.l-orc-real then do: /*Oráado x Realizado*/
                put skip(1).
               
                disp tt-detalha-item.vl-mercado-orc   
                     tt-detalha-item.per-mercado-orc
                     tt-detalha-item.vl-mercado-rea   
                     tt-detalha-item.per-mercado-rea
                     WITH FRAME f-cab-orc-real-i.
        
                /*IMPOSTOS*/
                if  tt-param.l-desp-imposto then do: /*Detalha Despesa-Imposto*/
                    if  tt-param.i-desp-imposto = 2 then  /*Imposto*/
                        disp tt-detalha-item.aliquota-ii
                             tt-detalha-item.vl-ii-orc   
                             tt-detalha-item.per-ii-orc
                             tt-detalha-item.vl-ii-rea   
                             tt-detalha-item.per-ii-rea
                             WITH FRAME f-imposto-i.        
                    
                    disp tt-detalha-item.aliquota-ipi
                         tt-detalha-item.vl-ipi-orc   
                         tt-detalha-item.per-ipi-orc
                         tt-detalha-item.vl-ipi-rea   
                         tt-detalha-item.per-ipi-rea
                         tt-detalha-item.aliquota-pis 
                         tt-detalha-item.vl-pis-orc    
                         tt-detalha-item.per-pis-orc  
                         tt-detalha-item.vl-pis-rea    
                         tt-detalha-item.per-pis-rea    
                         tt-detalha-item.aliquota-COFINS
                         tt-detalha-item.vl-COFINS-orc  
                         tt-detalha-item.per-COFINS-orc 
                         tt-detalha-item.vl-COFINS-rea  
                         tt-detalha-item.per-COFINS-rea 
                         tt-detalha-item.aliquota-icms
                         tt-detalha-item.vl-icms-orc
                         tt-detalha-item.per-icms-orc
                         tt-detalha-item.vl-icms-rea
                         tt-detalha-item.per-icms-rea
                         WITH FRAME f-ipi-icm-i.
                end.
                
                disp tt-detalha-item.vl-tot-imp-orc
                     tt-detalha-item.per-tot-imp-orc
                     tt-detalha-item.vl-tot-imp-rea
                     tt-detalha-item.per-tot-imp-rea
                     WITH FRAME f-tot-imp-i.
        
                /*DESPESAS*/
                if  tt-param.l-desp-imposto then do: /*Detalha Despesa-Imposto*/
                    
                    if  tt-param.i-desp-imposto = 1 then /*Despesa*/
                        disp tt-detalha-item.aliquota-ii2
                             tt-detalha-item.vl-ii2-orc
                             tt-detalha-item.per-ii2-orc
                             tt-detalha-item.vl-ii2-rea
                             tt-detalha-item.per-ii2-rea
                             WITH FRAME f-despesa-i.
        
                    for each  tt-detalha-despesa
                        where tt-detalha-despesa.numero-ordem = tt-detalha-item.numero-ordem
                          and tt-detalha-despesa.embarque     = tt-detalha-item.embarque
                          and tt-detalha-despesa.cod-estabel  = tt-detalha-item.cod-estabel
                          and tt-detalha-despesa.it-codigo    = tt-detalha-item.it-codigo
                          and tt-detalha-despesa.sequencia    = tt-detalha-item.sequencia
                        break by tt-detalha-despesa.cod-despesa:
                        
                        ASSIGN de-tot-desp = de-tot-desp + tt-detalha-despesa.vl-des-rea.
                        IF LAST-OF(tt-detalha-despesa.cod-despesa) THEN DO:
                            disp tt-detalha-despesa.desc-despesa
                                 tt-detalha-despesa.vl-des-orc
                                 tt-detalha-despesa.per-des-orc
                                 de-tot-desp @ tt-detalha-despesa.vl-des-rea
                                 tt-detalha-despesa.per-des-rea
                                 with frame f-det-desp-i.
                            down with frame f-det-desp-i.
                            ASSIGN de-tot-desp = 0.
                        END.
                    end.
                end.
        
                disp tt-detalha-item.vl-tot-des-orc
                     tt-detalha-item.per-tot-des-orc
                     tt-detalha-item.vl-tot-des-rea
                     tt-detalha-item.per-tot-des-rea
                     tt-detalha-item.vl-tot-imp-des-orc
                     tt-detalha-item.per-tot-imp-des-orc
                     tt-detalha-item.vl-tot-imp-des-rea
                     tt-detalha-item.per-tot-imp-des-rea
                     tt-detalha-item.vl-tot-com-ipi-icms-orc-total
                     tt-detalha-item.per-tot-com-ipi-icms-orc
                     tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
                     tt-detalha-item.per-tot-com-ipi-icms-rea
                     tt-detalha-item.vl-tot-sem-ipi-icms-orc-total
                     tt-detalha-item.per-tot-sem-ipi-icms-orc
                     tt-detalha-item.vl-tot-sem-ipi-icms-dsp-merc-rea
                     tt-detalha-item.per-tot-sem-ipi-icms-rea
                     WITH FRAME f-tot-orc-real-i.
            end.

            if  l-itens and l-orc-real then
            put skip(1).
        END.
        
        put skip(1).
    end.
    PAGE.
end.

procedure display-cabecalho:

    find first tt-embarque-cabec
         where tt-embarque-cabec.embarque     = tt-embarque-corpo.embarque
           and tt-embarque-cabec.cod-estabel  = tt-embarque-corpo.cod-estabel
           and tt-embarque-cabec.cod-emitente = tt-embarque-corpo.cod-emitente
         no-lock no-error.
    disp tt-embarque-cabec.cod-estabel
         tt-embarque-cabec.desc-estab
         tt-embarque-cabec.embarque
         tt-embarque-cabec.cod-emitente
         tt-embarque-cabec.desc-emit
         tt-embarque-cabec.pais
         tt-embarque-cabec.cod-incoterm
         tt-embarque-cabec.cod-via-transp
         tt-embarque-cabec.pto-embarque
         tt-embarque-cabec.data-embarque
         tt-embarque-cabec.pto-nacional
         tt-embarque-cabec.data-nacional
         tt-embarque-cabec.data-chegada
         tt-embarque-cabec.data-cotacao
         tt-embarque-cabec.desc-moeda
         tt-embarque-cabec.mo-taxa
         tt-embarque-cabec.data-convercao with frame f-cabec-embarque.

end procedure.



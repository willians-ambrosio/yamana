/*********************************************************************************
**
** rip/ri0404rp.i1 - geracao dos dados
**
*********************************************************************************/    

/* continuaá∆o do for each */
    BREAK BY {3}    
    BY {1}
    BY {4}
    WITH FRAME {2}:

    case tt-param.tipo-se:
        when 1 then assign c-valor-aux = TRIM(ri-bem.desc-bem).
        when 2 then assign c-valor-aux = ri-bem.nr-nota-fis /*+ "/" + ri-bem.serie*/.
        when 3 then assign c-valor-aux = trim(string(ri-bem.nr-patrimonio)) + " / " + trim(string(ri-bem.seq-patrimonio)).
        when 4 then assign c-valor-aux = trim(string(ri-bem.nr-ord-produ)).
    end case.        
    
    RUN pi-acompanhar IN h-acomp (INPUT "Bem: " + STRING(ri-bem.cod-identific-bem)).

    RUN rip/ri9999.p (INPUT  ri-bem-grupo.data-1, /* data de inicio de credito */ 
                      INPUT  tt-param.dt-corte, 
                      OUTPUT i-nr-parc-cred).

    ASSIGN dat-inic-cred = ri-bem-grupo.data-1.

    ASSIGN l-baixado-no-periodo = NO.

    /* Decurso de Prazo */
    /*IF  ri-bem-grupo.num-meses >= i-nr-parc-cred THEN 
    */
    DO:
        run pi-calcula-valores-bem (INPUT i-nr-parc-cred,
                                    INPUT tt-param.dt-corte,
                                    INPUT ?,
                                    output de-vl-tot-cred, 
                                    output de-vl-tot-recuperado,
                                    output de-vl-tot-nao-recuperado,
                                    output de-vl-tot-baixa,
                                    OUTPUT de-vl-saldo-cred).
                                    
        IF  ri-bem-grupo.num-meses = i-nr-parc-cred
        AND de-vl-saldo-cred > 0 THEN
            ASSIGN de-vl-tot-nao-recuperado = de-vl-tot-nao-recuperado + de-vl-saldo-cred
                   de-vl-saldo-cred         = 0.
    
        RUN pi-calcula-valor-atual-bem (INPUT  tt-param.dt-corte,
                                        OUTPUT de-valor-atual-bem).

        FOR FIRST ped-curva USE-INDEX ch-curva NO-LOCK 
            WHERE ped-curva.vl-aberto = 412
            AND   ped-curva.codigo  = ri-bem-grupo.id-bem
            AND   ped-curva.tot-ped = ri-bem-grupo.cod-grupo
            AND   ped-curva.regiao = STRING(YEAR(tt-param.dt-corte), "9999") + "/" +
                                     STRING(MONTH(tt-param.dt-corte), "99") + "/01":
        END.
    
        if  de-vl-saldo-cred = 0 then                            
            assign i-nr-parc-cred = 0.

        IF  de-valor-atual-bem = 0 THEN
            FOR LAST ri-movto-bem NO-LOCK
                WHERE ri-movto-bem.id-bem         = ri-bem-grupo.id-bem 
                AND YEAR(ri-movto-bem.dat-movto)  = YEAR(tt-param.dt-corte)
                AND MONTH(ri-movto-bem.dat-movto) = MONTH(tt-param.dt-corte)
                AND (   ri-movto-bem.idi-tipo-movto = 4    /* Vendas          */
                     OR ri-movto-bem.idi-tipo-movto = 5    /* Transferencia   */
                     OR ri-movto-bem.idi-tipo-movto = 6    /* Baixa por Perda */
                     OR ri-movto-bem.idi-tipo-movto = 7    /* Devolucao       */
                     OR ri-movto-bem.idi-tipo-movto = 8    /* Inutilizacao    */
                     OR ri-movto-bem.idi-tipo-movto = 14)  /* Exaustao        */ 
                
                /* log-1 = YES significa que Ç movto de baixa por exaust∆o gerado automaticamente */ 
                AND ri-movto-bem.log-1 = NO: 
                
                ASSIGN l-baixado-no-periodo = YES.
            END.
        
        /*IF  AVAIL ped-curva
        OR  l-baixado-no-periodo
        OR  (    de-vl-saldo-cred   <> 0 
             AND de-valor-atual-bem <> 0) THEN*/ 
        DO:
    
            IF  ri-bem-grupo.num-meses <> 0 AND ri-estab-grupos.qtde-meses-lg-prazo + ri-estab-grupos.qtde-meses-ct-prazo <> ri-bem-grupo.num-meses THEN
                ASSIGN i-qtd-meses-lg-prazo = ri-bem-grupo.num-meses - ri-estab-grupos.qtde-meses-ct-prazo
                       i-qtd-meses-ct-prazo = ri-estab-grupos.qtde-meses-ct-prazo.                        
            ELSE
                ASSIGN i-qtd-meses-lg-prazo = ri-estab-grupos.qtde-meses-lg-prazo
                       i-qtd-meses-ct-prazo = ri-estab-grupos.qtde-meses-ct-prazo.
           
         
            ASSIGN l-icms = NO.
            FOR FIRST ri-grupos NO-LOCK
                WHERE ri-grupos.cod-grupo = ri-bem-grupo.cod-grupo
                AND   ri-grupos.log-ciap:
                ASSIGN l-icms = YES. 
            END.
    
            RUN pi-calcula-valor-parcela-mensal (INPUT  l-icms,
                                                 INPUT  de-valor-atual-bem,
                                                 INPUT  i-nr-parc-cred,
                                                 INPUT  ri-bem-grupo.num-meses,
                                                 OUTPUT de-vl-parcela).

            RUN pi-retorna-cp-lp(INPUT  i-qtd-meses-ct-prazo,
                                 INPUT  i-qtd-meses-lg-prazo,
                                 INPUT  i-nr-parc-cred,
                                 INPUT  de-valor-atual-bem,
                                 INPUT  de-vl-parcela,
                                 INPUT  de-vl-saldo-cred,
                                 OUTPUT de-vl-saldo-cp,
                                 OUTPUT de-vl-saldo-lp).
                                 
           ASSIGN de-valor-atual-bem-aux       = de-valor-atual-bem-aux       +   de-valor-atual-bem      
                  de-vl-parcela-aux            = de-vl-parcela-aux            +   de-vl-parcela           
                  de-vl-tot-cred-aux           = de-vl-tot-cred-aux           +   de-vl-tot-cred          
                  de-vl-tot-recuperado-aux     = de-vl-tot-recuperado-aux     +   de-vl-tot-recuperado    
                  de-vl-tot-nao-recuperado-aux = de-vl-tot-nao-recuperado-aux +   de-vl-tot-nao-recuperado
                  de-vl-tot-baixa-aux          = de-vl-tot-baixa-aux          +   de-vl-tot-baixa         
                  de-vl-saldo-cred-aux         = de-vl-saldo-cred-aux         +   de-vl-saldo-cred        
                  de-vl-saldo-cp-aux           = de-vl-saldo-cp-aux           +   de-vl-saldo-cp          
                  de-vl-saldo-lp-aux           = de-vl-saldo-lp-aux           +   de-vl-saldo-lp
                  de-val-contabil-aux          = de-val-contabil-aux          +   ri-bem.val-contabil
                  de-val-contabil              = ri-bem.val-contabil.                            
                  
           ASSIGN de-val-contabil-doc          = de-val-contabil-doc          + de-val-contabil         
                  de-valor-atual-bem-doc       = de-valor-atual-bem-doc       + de-valor-atual-bem      
                  de-vl-parcela-doc            = de-vl-parcela-doc            + de-vl-parcela           
                  de-vl-tot-cred-doc           = de-vl-tot-cred-doc           + de-vl-tot-cred          
                  de-vl-tot-recuperado-doc     = de-vl-tot-recuperado-doc     + de-vl-tot-recuperado                 
                  de-vl-tot-nao-recuperado-doc = de-vl-tot-nao-recuperado-doc + de-vl-tot-nao-recuperado               
                  de-vl-tot-baixa-doc          = de-vl-tot-baixa-doc          + de-vl-tot-baixa                                          
                  de-vl-saldo-cred-doc         = de-vl-saldo-cred-doc         + de-vl-saldo-cred                                         
                  de-vl-saldo-cp-doc           = de-vl-saldo-cp-doc           + de-vl-saldo-cp                                           
                  de-vl-saldo-lp-doc           = de-vl-saldo-lp-doc           + de-vl-saldo-lp.      
             
           ASSIGN t-de-valor-atual-bem         = t-de-valor-atual-bem         + de-valor-atual-bem
                  t-de-vl-parcela              = t-de-vl-parcela              + de-vl-parcela
                  t-de-vl-tot-cred             = t-de-vl-tot-cred             + de-vl-tot-cred  
                  t-de-vl-tot-recuperado       = t-de-vl-tot-recuperado       + de-vl-tot-recuperado            
                  t-de-vl-tot-nao-recuperado   = t-de-vl-tot-nao-recuperado   + de-vl-tot-nao-recuperado         
                  t-de-vl-tot-baixa            = t-de-vl-tot-baixa            + de-vl-tot-baixa                                    
                  t-de-vl-saldo-cred           = t-de-vl-saldo-cred           + de-vl-saldo-cred                                   
                  t-de-vl-saldo-cp             = t-de-vl-saldo-cp             + de-vl-saldo-cp                                     
                  t-de-vl-saldo-lp             = t-de-vl-saldo-lp             + de-vl-saldo-lp
                  t-de-val-contabil            = t-de-val-contabil            + de-val-contabil.                                                                                         
                    
           IF tt-param.i-tip-relat = 1 /*Resumido*/
              AND (tt-param.tipo-sel = 1 /*bem*/ OR tt-param.tipo-sel = 2 /*Docto*/ ) THEN DO:                                            
               CASE tt-param.tipo-sel:
        
                 WHEN 1 /*Bem*/ THEN DO:                                                                                                                                 
                    
                    IF LAST-OF({1}) THEN DO:                                                
                            /*
                            ASSIGN de-valor-atual-bem       = de-valor-atual-bem-aux       
                                   de-vl-parcela            = de-vl-parcela-aux            
                                   de-vl-tot-cred           = de-vl-tot-cred-aux           
                                   de-vl-tot-recuperado     = de-vl-tot-recuperado-aux     
                                   de-vl-tot-nao-recuperado = de-vl-tot-nao-recuperado-aux 
                                   de-vl-tot-baixa          = de-vl-tot-baixa-aux          
                                   de-vl-saldo-cred         = de-vl-saldo-cred-aux         
                                   de-vl-saldo-cp           = de-vl-saldo-cp-aux           
                                   de-vl-saldo-lp           = de-vl-saldo-lp-aux.             
                            */       
                            DISP ri-estab-grupos.cod-estabel                         
                                 ri-bem-grupo.id-bem
                                 i-nr-parc-cred
                                 de-valor-atual-bem
                                 de-vl-parcela FORMAT "->>>>>,>>9.99"  
                                 de-vl-tot-cred  
                                 de-vl-tot-recuperado                  
                                 de-vl-tot-nao-recuperado                
                                 de-vl-tot-baixa                                           
                                 de-vl-saldo-cred                                          
                                 de-vl-saldo-cp                                            
                                 de-vl-saldo-lp                                        
                                 WITH FRAME {2}-resum.
                                DOWN WITH FRAME {2}-resum.
                            IF NOT tt-param.l-gera-planilha THEN                                
                                    ASSIGN de-valor-atual-bem-aux        = 0
                                           de-vl-parcela-aux             = 0
                                           de-vl-tot-cred-aux            = 0
                                           de-vl-tot-recuperado-aux      = 0
                                           de-vl-tot-nao-recuperado-aux  = 0
                                           de-vl-tot-baixa-aux           = 0
                                           de-vl-saldo-cred-aux          = 0
                                           de-vl-saldo-cp-aux            = 0
                                           de-vl-saldo-lp-aux            = 0.
                    END.            
                                                
                 END.
                 WHEN 2 /*Docto - Resumido*/ THEN DO: /* R2 Resumido */
                 
                    IF LAST-OF({3}) /*LAST-OF({1})*/ THEN DO:                                                
                            /* Resumido deve agrupar */
                            ASSIGN de-valor-atual-bem       = de-valor-atual-bem-aux       
                                   de-vl-parcela            = de-vl-parcela-aux            
                                   de-vl-tot-cred           = de-vl-tot-cred-aux           
                                   de-vl-tot-recuperado     = de-vl-tot-recuperado-aux     
                                   de-vl-tot-nao-recuperado = de-vl-tot-nao-recuperado-aux 
                                   de-vl-tot-baixa          = de-vl-tot-baixa-aux          
                                   de-vl-saldo-cred         = de-vl-saldo-cred-aux         
                                   de-vl-saldo-cp           = de-vl-saldo-cp-aux           
                                   de-vl-saldo-lp           = de-vl-saldo-lp-aux           
                                   de-val-contabil          = de-val-contabil-aux.             
                            DISP 
                                 ri-estab-grupos.cod-estabel
                                 /*c-valor-aux                                                     */
                                 de-val-contabil                         
                                 de-valor-atual-bem
                                 de-vl-parcela FORMAT "->>>>>,>>9.99"  
                                 de-vl-tot-cred  
                                 de-vl-tot-recuperado                  
                                 de-vl-tot-nao-recuperado                
                                 de-vl-tot-baixa                                           
                                 de-vl-saldo-cred                                          
                                 de-vl-saldo-cp                                            
                                 de-vl-saldo-lp                                        
                                 WITH FRAME {2}-resum.
                                DOWN WITH FRAME {2}-resum.
                            IF NOT tt-param.l-gera-planilha THEN                                
                                    ASSIGN de-valor-atual-bem-aux        = 0
                                           de-vl-parcela-aux             = 0
                                           de-vl-tot-cred-aux            = 0
                                           de-vl-tot-recuperado-aux      = 0
                                           de-vl-tot-nao-recuperado-aux  = 0
                                           de-vl-tot-baixa-aux           = 0
                                           de-vl-saldo-cred-aux          = 0
                                           de-vl-saldo-cp-aux            = 0
                                           de-vl-saldo-lp-aux            = 0
                                           de-val-contabil-aux           = 0.
                    END.                 
                 END.     
                 
                 /*
                 WHEN 4 THEN DO:
                     DISP 
                           ri-estab-grupos.cod-estabel          
                           c-valor-aux                                                     
                           dat-inic-cred                        
                           i-nr-parc-cred                       
                           de-valor-atual-bem                   
                           de-vl-parcela                        FORMAT "->>>>>,>>9.99"  
                           de-vl-tot-cred                       
                           de-vl-tot-recuperado                  
                           de-vl-tot-nao-recuperado                
                           de-vl-tot-baixa                                           
                           de-vl-saldo-cred                                          
                           de-vl-saldo-cp                                            
                           de-vl-saldo-lp                                                            
                           WITH FRAME {2}. 
                      DOWN WITH FRAME {2}.
                 END.            
                 
                 OTHERWISE DO:
                     DISP ri-estab-grupos.cod-estabel          
                          c-valor-aux                          
                          ri-bem.id-bem                        
                          dat-inic-cred                        
                          i-nr-parc-cred                       
                          de-valor-atual-bem                   
                          de-vl-parcela                        FORMAT "->>>>>,>>9.99"  
                          de-vl-tot-cred                       
                          de-vl-tot-recuperado                  
                          de-vl-tot-nao-recuperado                
                          de-vl-tot-baixa                                           
                          de-vl-saldo-cred                                          
                          de-vl-saldo-cp                                            
                          de-vl-saldo-lp                                        
                          WITH FRAME {2}.
                     DOWN WITH FRAME {2}.
                 END.       
                 */
               END CASE.                                                                     
            
               IF  tt-param.l-gera-planilha THEN DO:
               
                   IF LAST-OF({3}) THEN DO:     
                
                           PUT STREAM s-planilha UNFORMATTED                         
                               ri-estab-grupos.cod-estabel ";".     
                               
                           if tt-param.tipo-sel = 2 then
                                PUT STREAM s-planilha UNFORMATTED    
                                           /*ri-bem.val-contabil @ */ de-val-contabil    ";"                          
                                           de-valor-atual-bem ";"      
                                           de-vl-parcela ";"            
                                           de-vl-tot-cred ";"           
                                           de-vl-tot-recuperado ";"     
                                           de-vl-tot-nao-recuperado ";" 
                                           de-vl-tot-baixa ";"          
                                           de-vl-saldo-cred ";"         
                                           de-vl-saldo-cp ";"           
                                           de-vl-saldo-lp SKIP.
                           else PUT STREAM s-planilha UNFORMATTED                        
                                           de-valor-atual-bem ";"      
                                           de-vl-parcela ";"            
                                           de-vl-tot-cred ";"           
                                           de-vl-tot-recuperado ";"     
                                           de-vl-tot-nao-recuperado ";" 
                                           de-vl-tot-baixa ";"          
                                           de-vl-saldo-cred ";"         
                                           de-vl-saldo-cp ";"           
                                           de-vl-saldo-lp SKIP.
                               
                           ASSIGN de-val-contabil-aux           = 0
                                  de-valor-atual-bem-aux        = 0
                                  de-vl-parcela-aux             = 0
                                  de-vl-tot-cred-aux            = 0
                                  de-vl-tot-recuperado-aux      = 0
                                  de-vl-tot-nao-recuperado-aux  = 0
                                  de-vl-tot-baixa-aux           = 0
                                  de-vl-saldo-cred-aux          = 0
                                  de-vl-saldo-cp-aux            = 0
                                  de-vl-saldo-lp-aux            = 0.
                   END.            
               END.
           END.
                                 
           ELSE /*IF tt-param.i-tip-relat = 2 /*Detalhado*/ THEN*/ 
           DO:
               IF tt-param.tipo-sel = 1 THEN DO:                                                             
                       DISP ri-estab-grupos.cod-estabel
                            c-valor-aux
                            ri-bem.nr-nota-fis                         
                            ri-bem.nr-seq-docto
                            ri-bem.id-bem              
                            dat-inic-cred     
                            i-nr-parc-cred
                            de-valor-atual-bem
                            de-vl-parcela
                            de-vl-tot-cred  
                            de-vl-tot-recuperado                  
                            de-vl-tot-nao-recuperado                
                            de-vl-tot-baixa                                           
                            de-vl-saldo-cred                                          
                            de-vl-saldo-cp                                            
                            de-vl-saldo-lp                                        
                            WITH FRAME {2}.
                       DOWN WITH FRAME {2}.
               END.      
               ELSE IF tt-param.tipo-sel = 2 /* Docto - Detalhado */ THEN DO:
                       IF LAST-OF({1}) /*and LAST-OF({4})*/ THEN DO: /* R2 Detalhado */
                           ASSIGN de-val-contabil           = de-val-contabil-doc          
                                  de-valor-atual-bem        = de-valor-atual-bem-doc       
                                  de-vl-parcela             = de-vl-parcela-doc            
                                  de-vl-tot-cred            = de-vl-tot-cred-doc           
                                  de-vl-tot-recuperado      = de-vl-tot-recuperado-doc     
                                  de-vl-tot-nao-recuperado  = de-vl-tot-nao-recuperado-doc 
                                  de-vl-tot-baixa           = de-vl-tot-baixa-doc          
                                  de-vl-saldo-cred          = de-vl-saldo-cred-doc         
                                  de-vl-saldo-cp            = de-vl-saldo-cp-doc           
                                  de-vl-saldo-lp            = de-vl-saldo-lp-doc.               
                           DISP ri-estab-grupos.cod-estabel 
                                c-valor-aux                 
                                /*ri-bem.id-bem            */
                                dat-inic-cred               
                                /*i-nr-parc-cred           */
                                ri-bem.val-contabil @ de-val-contabil 
                                de-valor-atual-bem       
                                de-vl-parcela            
                                de-vl-tot-cred           
                                de-vl-tot-recuperado     
                                de-vl-tot-nao-recuperado 
                                de-vl-tot-baixa                         
                                de-vl-saldo-cred                        
                                de-vl-saldo-cp                          
                                de-vl-saldo-lp                      
                                WITH FRAME {2}.
                           DOWN WITH FRAME {2}.

                           ASSIGN de-val-contabil-doc          = 0
                                  de-valor-atual-bem-doc       = 0
                                  de-vl-parcela-doc            = 0
                                  de-vl-tot-cred-doc           = 0
                                  de-vl-tot-recuperado-doc     = 0
                                  de-vl-tot-nao-recuperado-doc = 0
                                  de-vl-tot-baixa-doc          = 0
                                  de-vl-saldo-cred-doc         = 0
                                  de-vl-saldo-cp-doc           = 0
                                  de-vl-saldo-lp-doc           = 0.
       
                       END.             
               END.              
               /*
               ELSE if tt-param.tipo-sel = 4 THEN  DO:                                                                  
                      DISP ri-estab-grupos.cod-estabel          
                           c-valor-aux                                                     
                           dat-inic-cred                        
                           i-nr-parc-cred                       
                           de-valor-atual-bem                   
                           de-vl-parcela                        
                           de-vl-tot-cred                       
                           de-vl-tot-recuperado                  
                           de-vl-tot-nao-recuperado                
                           de-vl-tot-baixa                                           
                           de-vl-saldo-cred                                          
                           de-vl-saldo-cp                                            
                           de-vl-saldo-lp                                                            
                           WITH FRAME {2}.
                       
                           DOWN WITH FRAME {2}.
               END.    
               ELSE DO:                                                                  
                      DISP ri-estab-grupos.cod-estabel          
                           c-valor-aux                          
                           ri-bem.id-bem                        
                           dat-inic-cred                        
                           i-nr-parc-cred                       
                           de-valor-atual-bem                   
                           de-vl-parcela                        
                           de-vl-tot-cred                       
                           de-vl-tot-recuperado                  
                           de-vl-tot-nao-recuperado                
                           de-vl-tot-baixa                                           
                           de-vl-saldo-cred                                          
                           de-vl-saldo-cp                                            
                           de-vl-saldo-lp                                                            
                           WITH FRAME {2}.
                           DOWN WITH FRAME {2}.
               END.      
               */                                                                                                                                                                                                        
               /*END CASE.*/
             
               /*ASSIGN t-de-valor-atual-bem       = t-de-valor-atual-bem         + de-valor-atual-bem
                      t-de-vl-parcela              = t-de-vl-parcela              + de-vl-parcela
                      t-de-vl-tot-cred             = t-de-vl-tot-cred             + de-vl-tot-cred  
                      t-de-vl-tot-recuperado       = t-de-vl-tot-recuperado       + de-vl-tot-recuperado            
                      t-de-vl-tot-nao-recuperado   = t-de-vl-tot-nao-recuperado   + de-vl-tot-nao-recuperado         
                      t-de-vl-tot-baixa            = t-de-vl-tot-baixa            + de-vl-tot-baixa                                    
                      t-de-vl-saldo-cred           = t-de-vl-saldo-cred           + de-vl-saldo-cred                                   
                      t-de-vl-saldo-cp             = t-de-vl-saldo-cp             + de-vl-saldo-cp                                     
                      t-de-vl-saldo-lp             = t-de-vl-saldo-lp             + de-vl-saldo-lp.*/
            
               IF  tt-param.l-gera-planilha THEN DO:
                   PUT STREAM s-planilha UNFORMATTED                         
                        ri-estab-grupos.cod-estabel ";".     
                        
                   IF  tt-param.tipo-sel = 1 /*Bem*/ then
                       PUT STREAM s-planilha UNFORMATTED
                           ri-bem.id-bem ";"
                           c-valor-aux ";"
                           ri-bem.nr-nota-fis "/" ri-bem.nr-seq-docto ";".             
                   ELSE
                       PUT STREAM s-planilha UNFORMATTED
                           c-valor-aux ";"             
                           /*ri-bem.id-bem ";"*/ .
                           
                   if  tt-param.tipo-sel = 2 then
                       PUT STREAM s-planilha UNFORMATTED                                       
                                  dat-inic-cred ";"    
                                  /*i-nr-parc-cred ";"        */
                                  ri-bem.val-contabil /*@ de-val-contabil */   ";"   
                                  de-valor-atual-bem ";"      
                                  de-vl-parcela ";"            
                                  de-vl-tot-cred ";"           
                                  de-vl-tot-recuperado ";"     
                                  de-vl-tot-nao-recuperado ";" 
                                  de-vl-tot-baixa ";"          
                                  de-vl-saldo-cred ";"         
                                  de-vl-saldo-cp ";"           
                                  de-vl-saldo-lp SKIP.
                   else PUT STREAM s-planilha UNFORMATTED
                                  dat-inic-cred ";"    
                                  i-nr-parc-cred ";"           
                                  de-valor-atual-bem ";"      
                                  de-vl-parcela ";"            
                                  de-vl-tot-cred ";"           
                                  de-vl-tot-recuperado ";"     
                                  de-vl-tot-nao-recuperado ";" 
                                  de-vl-tot-baixa ";"          
                                  de-vl-saldo-cred ";"         
                                  de-vl-saldo-cp ";"           
                                  de-vl-saldo-lp SKIP.
               END. /* IF  tt-param.l-gera-planilha THEN DO: */
            END.
        end.
    END.

    if  last({1}) then do:
        {esp/esri0404rp.i2 "{2}"}
    end.
                    
/* rip/ri0404.i1 */

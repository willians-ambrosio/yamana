/*******************************************************************************
* esIN0518.I - ImpressÆo
********************************************************************************/
run pi-inicializar in h-acomp(input c-imprimindo).
for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
      break by controle-verba.ep-codigo
            by controle-verba.cod-est-exec
            by controle-verba.num-projeto
            by controle-verba.num-ordem:   
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [controle-verba.int-1].         
 

      if tt-param.atualiza then do:
          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      {esp/esin0518.i4}
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock {1} {2} {3} {4} {5} {6} {7} {8}:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.
           if line-counter < 4 then do:
              {esp/esin0518.i4}
           end.
       
           
             
           disp t-controle.dt-trans   
                t-controle.it-codigo
                t-controle.num-ord-magnus
                t-controle.tipo-doc
                t-controle.solicitacao      
                t-controle.num-ord-comp
                t-controle.num-pedido
                t-controle.esp-docto
                t-controle.num-docto
                t-controle.ent-comp   
                t-controle.sai-comp   
                t-controle.ent-real   
                t-controle.sai-real 
                t-controle.acum-comp  
                t-controle.acum-real
                with frame f-controle.
                down with frame f-controle.


            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if line-counter + 4 > page-size then do:
            page.
            {esp/esin0518.i4}
         end.
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         disp c-lb-total
              de-acum-tot
              with frame f-total. 
      end.        
      page. 
end.

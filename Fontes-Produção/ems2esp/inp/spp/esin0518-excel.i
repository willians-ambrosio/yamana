/*******************************************************************************
* ESIN0518.I - Impress�o
********************************************************************************/

OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.

run pi-inicializar in h-acomp(input c-imprimindo).
for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim 
      AND controle-verba.dt-ini-validade >= tt-param.d-data-ini
      AND controle-verba.dt-fim-validade <= tt-param.d-data-fim NO-LOCK
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
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_Neg�cio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_Neg�cio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      for each tt-impressao:
          delete tt-impressao.
      end.
      {inp/spp/esin0518-excel.i4 tt-param.delimitador}
      find first tt-impressao no-lock no-error.
      if avail tt-impressao then
          PUT STREAM s-planilha UNFORMATTED
               tt-impressao.linha-1 skip
               tt-impressao.linha-2 skip
               tt-impressao.linha-3 skip
               tt-impressao.linha-4 skip
               tt-impressao.linha-5 skip
               tt-impressao.linha-6 skip
               tt-impressao.linha-7 skip
               tt-impressao.linha-8 skip
               tt-impressao.linha-9 skip(2)
               tt-impressao.colunas SKIP.
           
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

           PUT STREAM s-planilha UNFORMATTED 
               t-controle.dt-trans       tt-param.delimitador
               t-controle.it-codigo      tt-param.delimitador
               t-controle.num-ord-magnus tt-param.delimitador
               t-controle.tipo-doc       tt-param.delimitador
               t-controle.solicitacao    tt-param.delimitador  
               t-controle.num-ord-comp   tt-param.delimitador
               t-controle.num-pedido     tt-param.delimitador
               t-controle.esp-docto      tt-param.delimitador
               t-controle.num-docto      tt-param.delimitador
               t-controle.ent-comp       tt-param.delimitador
               t-controle.sai-comp       tt-param.delimitador
               t-controle.ent-real       tt-param.delimitador
               t-controle.sai-real       tt-param.delimitador
               t-controle.acum-comp      tt-param.delimitador
               t-controle.acum-real      tt-param.delimitador 
               SKIP.
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
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         PUT STREAM s-planilha UNFORMATTED 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador 
              tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total 
              tt-param.delimitador de-acum-tot SKIP(2).
      end.        
      PUT STREAM s-planilha UNFORMATTED SKIP(10).
end.
OUTPUT STREAM s-planilha CLOSE.

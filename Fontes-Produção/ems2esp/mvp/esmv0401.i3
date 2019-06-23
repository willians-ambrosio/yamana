/********************************************************************************
**
**  Include: mvp/esmv0401.i2
**
**  Imprime Relat�rio Listagem de T�cnico por ordem de manuten��o
**
**  Autor: Daniel Steiner 
**
********************************************************************************/
procedure pi-empresa:
def variable iEmp   as integer no-undo.
def variable cEqpto as char    no-undo.
assign iEmp   = ?
       cEqpto = "":U.


              
DEFINE VARIABLE contador-previsto AS INTEGER    NO-UNDO.
              



for each mmv-ord-manut 
   where mmv-ord-manut.nr-ord-produ        >= tt-param.i-ordem-ini        
    and  mmv-ord-manut.nr-ord-produ        <= tt-param.i-ordem-fim        
    and  mmv-ord-manut.ep-codigo           >= tt-param.i-empresa-ini      
    and  mmv-ord-manut.ep-codigo           <= tt-param.i-empresa-fim      
    and  mmv-ord-manut.cod-eqpto           >= tt-param.c-equipamento-ini  
    and  mmv-ord-manut.cod-eqpto           <= tt-param.c-equipamento-fim  
    and  mmv-ord-manut.dat-abert           >= tt-param.dt-abertura-ini    
    and  mmv-ord-manut.dat-abert           <= tt-param.dt-abertura-fim    
    and  mmv-ord-manut.dat-entr            >= tt-param.dt-entrada-ini     
    and  mmv-ord-manut.dat-entr            <= tt-param.dt-entrada-fim     
    and  mmv-ord-manut.cd-tipo             >= tt-param.i-tipo-manut-ini   
    and  mmv-ord-manut.cd-tipo             <= tt-param.i-tipo-manut-fim   
    and  mmv-ord-manut.cod-plandor         >= tt-param.i-planejador-ini 
    and  mmv-ord-manut.cod-plandor         <= tt-param.i-planejador-fim    
    and  mmv-ord-manut.cod-ofici           >= tt-param.c-oficina-ini      
    and  mmv-ord-manut.cod-ofici           <= tt-param.c-oficina-fim  
    and  mmv-ord-manut.dat-prev-term       >= tt-param.dt-prev-termino-ini
    and  mmv-ord-manut.dat-prev-term       <= tt-param.dt-prev-termino-fim
                break by mmv-ord-manut.ep-codigo :
                  

   if mmv-ord-manut.dat-term <> ?
      and not(mmv-ord-manut.dat-term    >= tt-param.dt-termino-ini and
      mmv-ord-manut.dat-term            <= tt-param.dt-termino-fim) then next.
   
   
    /**Estado**/
    IF  tt-param.estado = 1 AND 
      mmv-ord-manut.estado <> 1 THEN
      NEXT.
   IF  tt-param.estado = 2 AND 
      mmv-ord-manut.estado <> 2 THEN
      NEXT.
   IF  tt-param.estado = 3 AND 
      mmv-ord-manut.estado <> 3 THEN
      NEXT.
   IF  tt-param.estado = 4 AND 
      mmv-ord-manut.estado <> 4 THEN
      NEXT.
   IF  tt-param.estado = 5 AND 
      mmv-ord-manut.estado <> 5 THEN
      NEXT.
   IF  tt-param.estado = 6 AND 
      mmv-ord-manut.estado <> 6 THEN
      NEXT.
   IF  tt-param.estado = 7 AND 
      mmv-ord-manut.estado <> 7 THEN
      NEXT.
   IF  tt-param.estado = 8 AND 
      mmv-ord-manut.estado <> 8 THEN
      NEXT.
      
   /**Tipo**/
   if tt-param.tipo = 1 and 
      mmv-ord-manut.idi-tip-ord <> 1 then
       next.
   if tt-param.tipo = 2 and 
      mmv-ord-manut.idi-tip-ord <> 2 then
       next.
   if tt-param.tipo = 3 and 
      mmv-ord-manut.idi-tip-ord <> 3 then
       next.


     
        /**Ordem**/
        assign c-equipto        = ""
               i-ordem          = 0    
               c-estado         = ""     
               c-equipto        = ""
               dt-Abertura      = ""
               dt-Entrada       = ""
               c-TipoManutencao = ""
               c-Planejador     = ""
               c-Oficina        = ""
               dt-PrevTermino   = ""
               dt-Termino       = ""
               c-TipoOrdem      = 0
               i-tarefa         = 0  
               c-descricao      = "" 
               c-estado-om      = "" 
               dt-tempo-real    = "" 
               psegundo         = 0
               dt-tempo-total   = 0
               c-tecnico        = ""
               c-equipe-setor   = "" 
               c-funcionario    = ""
               hr-inicio        = "" 
               hr-final         = "" 
               c-equipe         = "". 
                                        
     

        
        for first mab-eqpto
            where mab-eqpto.ep-codigo = mmv-ord-manut.ep-codigo
            and   mab-eqpto.cod-eqpto = mmv-ord-manut.cod-eqpto no-lock:
     
            for first mab-model
                where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                assign c-Equipto = STRING(mmv-ord-manut.ep-codigo) + "-" + STRING(mmv-ord-manut.cod-eqpto) + "-" + string(mab-model.des-model).
            end.
                       
        end.
        for first tipo-manut
            where tipo-manut.cd-tipo = mmv-ord-manut.cd-tipo no-lock:
        end.
        
        assign  i-Ordem              = mmv-ord-manut.nr-ord-produ
                c-estado             = {ininc~/i01in271.i 4 mmv-ord-manut.estado}
                dt-Abertura          = string(mmv-ord-manut.dat-abert) + "-" + substring(mmv-ord-manut.hra-abert,1,2) + ":" + substring(mmv-ord-manut.hra-abert,3,2)
                dt-Entrada           = STRING(mmv-ord-manut.dat-entr) + "-" + substring(mmv-ord-manut.hra-entr,1,2) + ":" + substring(mmv-ord-manut.hra-entr,3,2)
                c-TipoManutencao     = string(mmv-ord-manut.cd-tipo) + " " + (tipo-manut.descricao) 
                c-Planejador         = mmv-ord-manut.cod-plandor
                c-Oficina            = mmv-ord-manut.cod-ofici
                dt-PrevTermino       = string(mmv-ord-manut.dat-prev-term) + "-" + substring(mmv-ord-manut.hra-prev-term,1,2) + ":" + substring(mmv-ord-manut.hra-prev-term,3,2)
                dt-Termino           = string(mmv-ord-manut.dat-term) + "-" + substring(mmv-ord-manut.hra-term,1,2) + ":" + substring(mmv-ord-manut.hra-term,3,2)
                c-TipoOrdem          = mmv-ord-manut.idi-tip-ord.
        
             display Ordem
                     Estado     
                     Equipto    
                     DtAbertura 
                     DtEntrada  
                                
                     TpOrdem    
                     TipoManut  
                     Planejador 
                     Oficina    
                     PrevTermino
                     Termino    

                     i-Ordem
                     c-estado
                     c-Equipto       
                     dt-Abertura     
                     dt-Entrada
                     c-TipoOrdem  
                     c-TipoManutencao
                     c-Planejador    
                     c-Oficina       
                     dt-PrevTermino  
                     dt-Termino   
                     with frame f-ordem.
                down with frame f-ordem.
                put skip(1).
                         


        
        assign c-equipe = "".                 
        /**Tarefa**/
        for each mmv-tar-ord-manut
            where mmv-tar-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ no-lock:
                
                assign i-tarefa            = mmv-tar-ord-manut.num-seq   
                       c-estado-om         = {frinc~/i01fr073.i 4 mmv-tar-ord-manut.estado}. 
                       

                for each mab-event
                    where mab-event.cod-evento = mmv-tar-ord-manut.cod-evento no-lock:
                        assign c-descricao = mab-event.des-evento.
                end.
                
                for first mmv-movto-mdo
                    where mmv-movto-mdo.nr-ord-produ = mmv-ord-manut.nr-ord-produ
                    and   mmv-movto-mdo.num-seq      = mmv-tar-ord-manut.num-seq no-lock:
                    
                        assign psegundo = (integer(substring(mmv-movto-mdo.hra-final,1,2)) * 3600
                                  + integer(substring(mmv-movto-mdo.hra-final,3,2)) * 60
                                  + integer(substring(mmv-movto-mdo.hra-final,5,2))) -
                                    
                                    (integer(substring(mmv-movto-mdo.hra-inicial,1,2)) * 3600
                                  + integer(substring(mmv-movto-mdo.hra-inicial,3,2)) * 60
                                  + integer(substring(mmv-movto-mdo.hra-inicial,5,2))). 
                                  
                        assign dt-tempo-real = string(psegundo, "HH:MM:SS").
                        
                end.
                
                for first mmv-tecnico-tarefa-om
                    where mmv-tecnico-tarefa-om.nr-ord-produ = mmv-ord-manut.nr-ord-produ no-lock:
                        assign dt-tempo-total = mmv-tecnico-tarefa-om.tempo-previsto.
                end.
                       
                display tarefa       
                        descricao 
                        ch-estado   
                        tempoTotal  
                        tempoReal 
                        i-tarefa    
                        c-estado-om
                        dt-tempo-total
                        dt-tempo-real
                        c-descricao 
                        with frame f-tarefa.                      
                   down with frame f-tarefa. 
                   put skip(1).            
                


              /**Previsto**/
              for each mmv-tecnico-tarefa-om
                    where mmv-tecnico-tarefa-om.nr-ord-produ = mmv-ord-manut.nr-ord-produ
                    and   mmv-tecnico-tarefa-om.num-seq      = mmv-tar-ord-manut.num-seq:
                        
                        assign contador-previsto = 0. 
                                       

                        /**Reportado**/
                        for each mmv-movto-mdo
                            where mmv-movto-mdo.num-seq          =  mmv-tar-ord-manut.num-seq
                            and   mmv-movto-mdo.nr-ord-produ     =  mmv-ord-manut.nr-ord-produ
                            and   mmv-movto-mdo.cod-matr         =  mmv-tecnico-tarefa-om.cod-matr no-lock:
                            


                             assign contador-previsto = contador-previsto + 1.
                                             
                             if contador-previsto = 1 then do:
                                

                                
                                 for first mmv-func-ofici
                                     where mmv-func-ofici.cod-matr = mmv-tecnico-tarefa-om.cod-matr no-lock:
                                     assign c-tecnico = mmv-tecnico-tarefa-om.cod-matr  + " " +  mmv-func-ofici.nom-func.
                                 end.
                        
                                 for first mmv-setor-ofici
                                     where mmv-setor-ofici.cod-setor-ofici = mmv-tar-ord-manut.cod-setor-ofici no-lock:
                                     assign c-equipe-setor =  mmv-setor-ofici.cod-setor-ofici + " " + mmv-setor-ofici.des-setor-ofici.
                                 end.
                                 assign dt-data-previs    =  mmv-tecnico-tarefa-om.dt-prevista.
                                    
                                 display previsto    
                                         tecnico     
                                         dtPrevisto  
                                         equipe-setor
                                         c-tecnico     
                                         dt-data-previs
                                         c-equipe-setor 
                                         with frame f-previsto.                                       
                                    down with frame f-previsto. 
                                    put skip(1).
                                
                             end.


                             for first mmv-func-ofici
                                 where mmv-func-ofici.cod-matr = mmv-movto-mdo.cod-matr no-lock:
                                 assign c-funcionario = mmv-movto-mdo.cod-matr  + " " +  mmv-func-ofici.nom-func.
                             end.
                             for first mmv-setor-ofici
                                 where mmv-setor-ofici.cod-setor-ofici = mmv-tar-ord-manut.cod-setor-ofici no-lock:
                                 assign c-equipe =  mmv-setor-ofici.cod-setor-ofici + " " + mmv-setor-ofici.des-setor-ofici.
                                      
                             end.
                             assign dt-realizado  = mmv-movto-mdo.dat-movto   
                                    hr-inicio     = mmv-movto-mdo.hra-inicial 
                                    hr-final      = mmv-movto-mdo.hra-final.
                                         

                             display reportado       
                                     funcionario  
                                     dtRealizado  
                                     hrInicio     
                                     hrFinal      
                                     Equipe_Setor 
                                     c-funcionario 
                                     dt-realizado 
                                     hr-inicio    
                                     hr-final
                                     c-equipe
                                     with frame f-reportado.                                       
                                down with frame f-reportado. 
                                put skip(1). 
                                
                        end.
              end.
              
                
              
              FOR each mmv-tecnico-tarefa-om
                  WHERE mmv-tecnico-tarefa-om.num-seq          = mmv-tar-ord-manut.num-seq
                  AND   mmv-tecnico-tarefa-om.nr-ord-produ     = mmv-ord-manut.nr-ord-produ:
                      if not can-find (first mmv-movto-mdo  WHERE mmv-movto-mdo.cod-matr = mmv-tecnico-tarefa-om.cod-matr   
                                       and  mmv-movto-mdo.nr-ord-produ = mmv-tecnico-tarefa-om.nr-ord-produ  
                                       and mmv-movto-mdo.num-seq =  mmv-tar-ord-manut.num-seq no-lock) THEN DO:
                                       

                          
                           
                          for first mmv-func-ofici
                              where mmv-func-ofici.cod-matr = mmv-tecnico-tarefa-om.cod-matr no-lock:
                              assign c-tecnico = mmv-tecnico-tarefa-om.cod-matr  + " " +  mmv-func-ofici.nom-func.
                          end.
                        
                          for first mmv-setor-ofici
                              where mmv-setor-ofici.cod-setor-ofici = mmv-tar-ord-manut.cod-setor-ofici no-lock:
                              assign c-equipe-setor =  mmv-setor-ofici.cod-setor-ofici + " " + mmv-setor-ofici.des-setor-ofici.
                          end.
                          assign dt-data-previs    =  mmv-tecnico-tarefa-om.dt-prevista.
                                 
                          display previsto    
                                  tecnico     
                                  dtPrevisto  
                                  equipe-setor
                                  c-tecnico     
                                  dt-data-previs
                                  c-equipe-setor 
                                  with frame f-previsto.                                       
                             down with frame f-previsto. 
                             put skip(1).
                             
                      end.
              end.

              FOR each mmv-movto-mdo
                  WHERE mmv-movto-mdo.num-seq          = mmv-tar-ord-manut.num-seq
                  AND   mmv-movto-mdo.nr-ord-produ     = mmv-ord-manut.nr-ord-produ:
                                    
                         if not can-find (first mmv-tecnico-tarefa-om  WHERE mmv-tecnico-tarefa-om.cod-matr = mmv-movto-mdo.cod-matr
                                       and mmv-tecnico-tarefa-om.nr-ord-produ = mmv-movto-mdo.nr-ord-produ 
                                       and mmv-tecnico-tarefa-om.num-seq = mmv-movto-mdo.num-seq no-lock) THEN DO:
                                       
                        
                          for first mmv-func-ofici
                              where mmv-func-ofici.cod-matr = mmv-movto-mdo.cod-matr no-lock:
                              assign c-funcionario = mmv-movto-mdo.cod-matr  + " " +  mmv-func-ofici.nom-func.
                          end.
                          for first mmv-setor-ofici
                              where mmv-setor-ofici.cod-setor-ofici = mmv-tar-ord-manut.cod-setor-ofici no-lock:
                              assign c-equipe =  mmv-setor-ofici.cod-setor-ofici + " " + mmv-setor-ofici.des-setor-ofici.
                          end.
                          assign dt-realizado  = mmv-movto-mdo.dat-movto   
                                 hr-inicio     = mmv-movto-mdo.hra-inicial 
                                 hr-final      = mmv-movto-mdo.hra-final.
                                                 
                                 display reportado       
                                         funcionario  
                                         dtRealizado  
                                         hrInicio     
                                         hrFinal      
                                         Equipe_Setor 
                                         c-funcionario 
                                         dt-realizado 
                                         hr-inicio    
                                         hr-final
                                         c-equipe
                                         with frame f-reportado.                                       
                                    down with frame f-reportado. 
                                    put skip(1).
                      
                      end.
              END.
              
        end.
end.                                
     

return 'OK'.

end procedure.
/* FIM */    


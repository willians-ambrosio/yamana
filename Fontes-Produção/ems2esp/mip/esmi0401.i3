
/********************************************************************************
**
**  Include: mip/esmi0401.i2
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
    
    
    for each ord-manut 
       where ord-manut.nr-ord-produ        >= tt-param.i-ordem-ini        
        and  ord-manut.nr-ord-produ        <= tt-param.i-ordem-fim       
        and  ord-manut.ep-codigo           >= tt-param.i-empresa-ini      
        and  ord-manut.ep-codigo           <= tt-param.i-empresa-fim      
        and  ord-manut.cd-equipto          >= tt-param.c-equipamento-ini  
        and  ord-manut.cd-equipto          <= tt-param.c-equipamento-fim  
        and  ord-manut.dt-manut            >= tt-param.dt-manut-ini    
        and  ord-manut.dt-manut            <= tt-param.dt-manut-fim   
        and  ord-manut.dt-ini-cedo         >= tt-param.dt-ini-cedo-ini     
        and  ord-manut.dt-ini-cedo         <= tt-param.dt-ini-cedo-fim     
        and  ord-manut.cd-tipo             >= tt-param.i-tipo-manut-ini   
        and  ord-manut.cd-tipo             <= tt-param.i-tipo-manut-fim   
        and  ord-manut.cd-planejado        >= tt-param.i-planejador-ini 
        and  ord-manut.cd-planejado        <= tt-param.i-planejador-fim    
        /*and  ord-manut.dt-ini-tarde      >= tt-param.dt-ini-tarde-ini      
        and  ord-manut.dt-ini-tarde        <= tt-param.dt-ini-tarde-fim */
        and  ord-manut.dt-prev             >= tt-param.dt-prev-termino-ini
        and  ord-manut.dt-prev             <= tt-param.dt-prev-termino-fim
                    break by ord-manut.ep-codigo:
                
 
    if ord-manut.dt-ini-tarde <> ?
      and not (ord-manut.dt-ini-tarde    >= tt-param.dt-ini-tarde-ini) 
      and not(ord-manut.dt-ini-tarde    <= tt-param.dt-ini-tarde-fim) then next.
   

        
    /**Estado**/
    IF  tt-param.estado = 1 AND 
       ord-manut.estado <> 1 THEN
       NEXT.
    IF  tt-param.estado = 2 AND 
       ord-manut.estado <> 2 THEN
       NEXT.
    IF  tt-param.estado = 3 AND 
       ord-manut.estado <> 3 THEN
       NEXT.
    IF  tt-param.estado = 4 AND 
       ord-manut.estado <> 4 THEN
       NEXT.
    IF  tt-param.estado = 5 AND 
       ord-manut.estado <> 5 THEN
       NEXT.
    IF  tt-param.estado = 6 AND 
       ord-manut.estado <> 6 THEN
       NEXT.
    IF  tt-param.estado = 7 AND 
       ord-manut.estado <> 7 THEN
       NEXT.
    IF  tt-param.estado = 8 AND 
       ord-manut.estado <> 8 THEN
       NEXT.
      

    
        assign c-equipto        = ""
               i-ordem          = 0
               c-estado         = ""      
               c-equipto        = ""      
               c-TipoManutencao = ""
               c-Planejador     = ""   
               c-TipoOrdem      = "" 
               c-plano          = ""   
               i-tarefa         = 0  
               c-evento         = ""   
               c-estado-om      = ""   
               h-reportadas     = 0   
               psegundo         = 0
               c-tecnico        = "" 
               c-equipe-setor   = ""
               c-funcionario    = ""
               hr-inicio        = ""
               hr-final         = ""   
               c-equipe         = "". 
         
        for first equipto
          where equipto.ep-codigo  = ord-manut.ep-codigo
          and   equipto.cd-equipto = ord-manut.cd-equipto no-lock:
               assign c-Equipto = STRING(ord-manut.ep-codigo) + "-" + STRING(ord-manut.cd-equipto) + "-" + string(equipto.descricao).
        end.
        
        for first tipo-manut
          where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock:
        end.
        

        assign  i-Ordem              = ord-manut.nr-ord-produ
                c-estado             = {ininc~/i01in270.i 4 ord-manut.estado-om}
                dt-manut             = ord-manut.dt-manut
                dt-ini-cedo          = ord-manut.dt-ini-cedo
                c-TipoManutencao     = string(ord-manut.cd-tipo) + "-" + (tipo-manut.descricao)
                c-Planejador         = ord-manut.cd-planejado
                c-plano              = ord-manut.plano-orig
                dt-ini-tarde         = ord-manut.dt-ini-tarde
                dt-PrevTermino       = ord-manut.dt-prev
                dt-Termino           = ord-manut.dt-prev-manut
                c-TipoOrdem          =  {ininc~/i02in270.i 4 ord-manut.tipo}.
                

             display Ordem
                     Estado     
                     Equipto    
                     DtManut 
                     DtIniCedo
                     DtIniTarde  
                                
                     TpOrdem    
                     TipoManut  
                     Planejador
                     plano 
                     dtprev
                     dtPrevManut    

                     i-Ordem
                     c-estado
                     c-Equipto       
                     dt-manut     
                     dt-ini-cedo
                     dt-ini-tarde
                     
                     c-TipoOrdem 
                     c-TipoManutencao
                     c-Planejador
                     c-plano
                     dt-PrevTermino  
                     dt-Termino
                 with frame f-ordem.
             down with frame f-ordem.
             put skip(1).
                         

       /**Tarefa**/
        for each ord-taref
            where ord-taref.nr-ord-produ = ord-manut.nr-ord-produ no-lock:
            
                assign i-tarefa            = ord-taref.cd-tarefa   
                       c-estado-om         = {mninc~/i01mn136.i 4 ord-taref.estado-taref}
                       c-evento            = ord-taref.descricao
                       /*h-tempo             = ord-taref.tempo*/
                       h-reportadas        = ord-taref.tempo-real.
                       

               
                /*for first mmv-tecnico-tarefa-om
                    where mmv-tecnico-tarefa-om.nr-ord-produ = mmv-ord-manut.nr-ord-produ :
                        assign dt-tempo-total = mmv-tecnico-tarefa-om.tempo-previsto.
                end.*/
                       
                display tarefa       
                        evento 
                        ch-estado   
                        /*planoManut  */
                        HReporte
                          i-tarefa    
                          c-estado-om
                          /*h-tempo*/
                          h-reportadas
                          c-evento
                          with frame f-tarefa.
                    down with frame f-tarefa. 
                    put skip(1).            
                
                
                /**Previsto**/

                for each mmi-tecnico-tarefa-om
                    where mmi-tecnico-tarefa-om.nr-ord-produ = ord-manut.nr-ord-produ
                    and   mmi-tecnico-tarefa-om.num-seq      = ord-taref.cd-tarefa no-lock:
                        
                    assign contador-previsto = 0.
                            
                    /**Reportado**/
                    for each ord-mob
                        where ord-mob.nr-ord-produ   = ord-manut.nr-ord-produ
                        and   ord-mob.cd-tarefa      = ord-taref.cd-tarefa 
                        and   ord-mob.cd-tecnico     = mmi-tecnico-tarefa-om.cd-tecnico no-lock:
                            
                        assign contador-previsto = contador-previsto + 1.
                                             
                        if contador-previsto = 1 then do:
                        
                             for first tecn-mi
                                 where tecn-mi.cd-tecnico = mmi-tecnico-tarefa-om.cd-tecnico :
                                 assign c-tecnico = mmi-tecnico-tarefa-om.cd-tecnico + " " + tecn-mi.nome-compl.
                             end.
                             
                             
                             for first tecn-mi
                                 where tecn-mi.cd-tecnico = mmi-tecnico-tarefa-om.cd-tecnico no-lock:
                                 for first equipe
                                     where equipe.cd-equipe = tecn-mi.cd-equipe no-lock:
                                 assign c-equipe-setor = tecn-mi.cd-equipe + " - " + equipe.desc-equipe.
                                 end.
                             end.
                             
                             
                             
                               assign dt-data-previs    =  mmi-tecnico-tarefa-om.dt-prevista.
                                      
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
                        END.
                          

                          for first tecn-mi
                              WHERE tecn-mi.cd-tecnico = ord-mob.cd-tecnico no-lock:
                              assign c-funcionario = string(ord-mob.cd-tecnico) + "-" + tecn-mi.nome-compl.
                          end.
                    
                          for first tecn-mi
                              where tecn-mi.cd-tecnico = mmi-tecnico-tarefa-om.cd-tecnico no-lock:
                              for first equipe
                                  where equipe.cd-equipe = tecn-mi.cd-equipe no-lock:
                                  assign c-equipe = tecn-mi.cd-equipe + " - " + equipe.desc-equipe.
                               end.
                          end.
                          assign dt-realizado  = ord-mob.dt-trans   
                                 hr-inicio     = ord-mob.hora-inicio
                                 hr-final      = ord-mob.hora-termino.
                           
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
                
                        

                FOR EACH mmi-tecnico-tarefa-om
                    WHERE mmi-tecnico-tarefa-om.num-seq          =  ord-taref.cd-tarefa
                    AND   mmi-tecnico-tarefa-om.nr-ord-produ     =  ord-manut.nr-ord-produ no-lock:
                        if not can-find (first ord-mob  WHERE ord-mob.cd-tecnico = mmi-tecnico-tarefa-om.cd-tecnico   
                                         and  ord-mob.nr-ord-produ = mmi-tecnico-tarefa-om.nr-ord-produ
                                         and ord-mob.cd-tarefa = mmi-tecnico-tarefa-om.num-seq no-lock) THEN DO:
                                         
                        
                            for first tecn-mi
                                where tecn-mi.cd-tecnico = mmi-tecnico-tarefa-om.cd-tecnico :
                                assign c-tecnico = mmi-tecnico-tarefa-om.cd-tecnico + " " + tecn-mi.nome-compl.
                            end.
                        
                        
                            for first tecn-mi
                                where tecn-mi.cd-tecnico = mmi-tecnico-tarefa-om.cd-tecnico no-lock:
                                for first equipe
                                    where equipe.cd-equipe = tecn-mi.cd-equipe no-lock:
                                assign c-equipe-setor = tecn-mi.cd-equipe + " - " + equipe.desc-equipe.
                                end.
                            end.
                            
                            assign dt-data-previs    =  mmi-tecnico-tarefa-om.dt-prevista.
                                 
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
                
                
                FOR EACH ord-mob
                    WHERE ord-mob.cd-tarefa        =  ord-taref.cd-tarefa   
                    AND   ord-mob.nr-ord-produ     =  ord-manut.nr-ord-produ:
                        if not can-find (first mmi-tecnico-tarefa-om  WHERE mmi-tecnico-tarefa-om.cd-tecnico  =  ord-mob.cd-tecnico
                                           and mmi-tecnico-tarefa-om.nr-ord-produ = ord-mob.nr-ord-produ 
                                           and mmi-tecnico-tarefa-om.num-seq = ord-mob.cd-tarefa no-lock) THEN DO: 
                                         
                            for first tecn-mi                                                                
                                WHERE tecn-mi.cd-tecnico = ord-mob.cd-tecnico no-lock:                
                                assign c-funcionario = string(ord-mob.cd-tecnico) + "-" + tecn-mi.nome-compl.
                            end.                                                                             
                                                                                                             
                            for first tecn-mi                                                                
                                where tecn-mi.cd-tecnico = mmi-tecnico-tarefa-om.cd-tecnico no-lock:  
                                for first equipe                                                             
                                    where equipe.cd-equipe = tecn-mi.cd-equipe no-lock:               
                                    assign c-equipe = tecn-mi.cd-equipe + " - " + equipe.desc-equipe.        
                                 end.                                                                        
                            end.                                                                             
                            assign dt-realizado  = ord-mob.dt-trans                                          
                                   hr-inicio     = ord-mob.hora-inicio                                       
                                   hr-final      = ord-mob.hora-termino.                                     
                                                                                                             
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
        end.
end.                                
                                                

   

return 'OK'.

end procedure.
/* FIM */    



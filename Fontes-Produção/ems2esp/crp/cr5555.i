/***************************************************************************
***
*** crp/cr5555.i - Trata Chave Alteranativa.
***
***************************************************************************/

    if  search("crp/cr5555.r") = ? and 
        search("crp/cr5555.w") = ? then do:
        run utp/ut-msgs.p (input "show",
                           input 4,
                           input "crp/cr5555.w").
    end.
    else do:    
        
        run crp/cr5555.w (Output v-row-titulo).
        
        if  v-row-titulo <> ? then do:
            
            run pi-reposiciona-query in {1} (Input v-row-titulo).
            
        end.
        
    end.
    
/* Fim de Include */
    

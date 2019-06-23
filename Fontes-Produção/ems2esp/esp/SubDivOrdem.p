/*******************************************************************************
Extracao de Sub Divisäes da Ordem
Daniel Pereira - Programa criado para atender projeto de Change Badget
21/06/2012
*******************************************************************************/

{esp\KRAFT.I}

PROCEDURE pi-delete.
    FIND FIRST param-global NO-LOCK NO-ERROR.

    FOR EACH es_sub_div_ordem
       WHERE es_sub_div_ordem.codEmpresa = param-global.empresa-pri:
        IF AVAIL es_sub_div_ordem THEN
          DELETE es_sub_div_ordem.
    END.
END PROCEDURE.

RUN pi-delete.

FIND FIRST param-global NO-LOCK NO-ERROR.

FOR EACH ordem-inv WHERE ordem-inv.ep-codigo = param-global.empresa-pri USE-INDEX emp-est-proj NO-LOCK:
    FOR EACH sub-div-ordem  WHERE
            sub-div-ordem.ep-codigo      = ordem-inv.ep-codigo         
        AND sub-div-ordem.cod-est-exec   = ordem-inv.cod-est-exec      
        AND sub-div-ordem.num-projeto    = ordem-inv.num-projeto       
        AND sub-div-ordem.num-ordem      = ordem-inv.num-ordem         
        USE-INDEX emp-est-proj NO-LOCK.

        /*DISP
             sub-div-ordem.ep-codigo   
             sub-div-ordem.cod-est-exec
             sub-div-ordem.num-projeto 
             sub-div-ordem.num-ordem   
             sub-div-ordem.num-ord-magnus
            
            WITH 1 COL WIDTH 300.*/

        CREATE es_sub_div_ordem.
        ASSIGN 

           es_sub_div_ordem.codEmpresa          =  sub-div-ordem.ep-codigo      
           es_sub_div_ordem.codEstabelecimento  =  sub-div-ordem.cod-est-exec   
           es_sub_div_ordem.numProjeto          =  sub-div-ordem.num-projeto    
           es_sub_div_ordem.codigo              =  sub-div-ordem.num-ordem      
           es_sub_div_ordem.numOrdMagnus        =  sub-div-ordem.num-ord-magnus.


    END.
END.


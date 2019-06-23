/*******************************************************************************
Extracao de Centro de Custos
Kraft Consulting
29/12/2010
*******************************************************************************/

{esp\KRAFT.I}

PROCEDURE pi-delete.
    FIND FIRST param-global NO-LOCK NO-ERROR.

    FOR EACH es_ordem_inv
       WHERE es_ordem_inv.codEmpresa = param-global.empresa-pri:
        IF AVAIL es_ordem_inv THEN
          DELETE es_ordem_inv.
    END.
END PROCEDURE.


RUN pi-delete.

FIND FIRST param-global NO-LOCK NO-ERROR.

FOR EACH ordem-inv WHERE ordem-inv.ep-codigo = param-global.empresa-pri USE-INDEX emp-est-proj NO-LOCK:
    FOR EACH controle-verba  
      WHERE controle-verba.ep-codigo    = ordem-inv.ep-codigo   
        AND controle-verba.cod-est-exec = ordem-inv.cod-est-exec 
        AND controle-verba.num-projeto  = ordem-inv.num-projeto 
        AND controle-verba.num-ordem    = ordem-inv.num-ordem   
        USE-INDEX emp-ord NO-LOCK:

    Create es_ordem_inv.
    Assign es_ordem_inv.codEmpresa     = ordem-inv.ep-codigo
           es_ordem_inv.codEstabel     = ordem-inv.cod-est-exec                                                 /*Especifico modificado em 15/05/2012*/
           es_ordem_inv.NumeroProjeto  = ordem-inv.num-projeto                                                  /*para que atenda a necessidade de novos campos*/
           es_ordem_inv.codigo         = ordem-inv.num-ordem                                                    /* Valor Compromissado e Valor Realizado*/
           es_ordem_inv.Descricao      = ordem-inv.descricao                                                   
           es_ordem_inv.Conta          = ordem-inv.ct-codigo
           es_ordem_inv.SubConta       = ordem-inv.sc-codigo
           es_ordem_inv.ContaContabil  = ordem-inv.conta-contabil
           es_ordem_inv.valorVerba1    = ordem-inv.vl-verba[1] 
           es_ordem_inv.valorVerba2    = ordem-inv.vl-verba[2] 
           es_ordem_inv.valorVerba3    = ordem-inv.vl-verba[3] 
           es_ordem_inv.numordmagnus   = ordem-inv.num-ordem
           es_ordem_inv.valorVerbaOri1 = controle-verba.vl-verba-orig[1]
           es_ordem_inv.valorComp1     = controle-verba.vl-comp[1] 
           es_ordem_inv.valorComp2     = controle-verba.vl-comp[2] 
           es_ordem_inv.valorComp3     = controle-verba.vl-comp[3] 
           es_ordem_inv.valorReal1     = controle-verba.vl-real[1] 
           es_ordem_inv.valorReal2     = controle-verba.vl-real[2] 
           es_ordem_inv.valorReal3     = controle-verba.vl-real[3] 
        .

    END.
END.


/*
DELETE FROM es_ordem_inv.
FOR EACH ordem-inv FIELDS(ep-codigo cod-est-exec num-projeto num-ordem descricao ct-codigo sc-codigo conta-contabil vl-verba num-ordem) NO-LOCK:

    Create es_ordem_inv.
    Assign es_ordem_inv.codEmpresa     = ordem-inv.ep-codigo
           es_ordem_inv.codEstabel     = ordem-inv.cod-est-exec                     Antigo Fonte desenvolvido pela Kraft
           es_ordem_inv.NumeroProjeto  = ordem-inv.num-projeto
           es_ordem_inv.codigo         = ordem-inv.num-ordem
           es_ordem_inv.Descricao      = ordem-inv.descricao
           es_ordem_inv.Conta          = ordem-inv.ct-codigo
           es_ordem_inv.SubConta       = ordem-inv.sc-codigo
           es_ordem_inv.ContaContabil  = ordem-inv.conta-contabil
           es_ordem_inv.valorVerba1    = DECIMAL(ordem-inv.vl-verba[1])
           es_ordem_inv.valorVerba2    = DECIMAL(ordem-inv.vl-verba[2])
           es_ordem_inv.valorVerba3    = DECIMAL(ordem-inv.vl-verba[3])
           es_ordem_inv.numordmagnus   = ordem-inv.num-ordem.

END.*/




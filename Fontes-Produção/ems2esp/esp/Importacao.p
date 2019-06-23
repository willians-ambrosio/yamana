/*******************************************************************************
Extracao de Importacao
Kraft Consulting
29/12/2010
*******************************************************************************/

{esp\KRAFT.I}

DEFINE INPUT PARAMETER p-dt-inicial AS DATE NO-UNDO.
DEFINE INPUT PARAMETER p-dt-final AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pHandle AS HANDLE NO-UNDO.


DEF VAR c-arquivo            AS CHAR                        NO-UNDO.
DEF VAR c-return-ordens-emb  AS CHAR                        NO-UNDO.
DEF VAR c-return-ordem-compr AS CHAR                        NO-UNDO.
DEF VAR i-numero-ordem       LIKE ordem-compra.numero-ordem NO-UNDO.
DEF VAR i-cod-emitente       LIKE ordem-compra.cod-emitente NO-UNDO.
DEF VAR h-boin274            AS HANDLE                      NO-UNDO.
DEF VAR h-bocx225            AS HANDLE                      NO-UNDO.

IF NOT VALID-HANDLE(h-boin274) THEN
    RUN inbo/boin274.p PERSISTENT SET h-boin274.

IF NOT VALID-HANDLE(h-bocx225) THEN
    RUN cxbo/bocx225.p PERSISTENT SET h-bocx225.


FOR EACH embarque-imp       
    WHERE embarque-imp.data-DI >= p-dt-inicial 
      AND embarque-imp.data-DI <= p-dt-final NO-LOCK:

    RUN pi-acompanhar IN pHandle (INPUT "Imp. N.Embarque " + embarque-imp.embarque).

    RUN pi-get-fornecedor.

    FOR EACH desp-embarque NO-LOCK
       WHERE desp-embarque.cod-estabel       = embarque-imp.cod-estabel 
         AND desp-embarque.embarque          = embarque-imp.embarque
         AND desp-embarque.cod-emitente-desp = embarque-imp.cod-desp USE-INDEX despesa:            
    
        FIND desp-imp NO-LOCK WHERE 
             desp-imp.cod-desp = desp-embarque.cod-desp USE-INDEX codigo NO-ERROR.

        FIND FIRST historico-embarque OF embarque-imp NO-LOCK NO-ERROR.

        FIND FIRST es_importacao 
            WHERE es_importacao.codEstabelecimento  = embarque-imp.cod-estabel
              AND es_importacao.numeroEmbarque      = embarque-imp.embarque 
              AND es_importacao.codigoDespesa       = desp-embarque.cod-desp USE-INDEX PK_es_importacao NO-ERROR.

        IF NOT AVAIL es_importacao THEN DO:
            CREATE es_importacao.
            ASSIGN es_importacao.codEstabelecimento = embarque-imp.cod-estabel  /*chave primaria e unica*/     
                   es_importacao.NumeroEmbarque     = embarque-imp.embarque     /*chave primaria e unica*/     
                   es_importacao.CodigoDespesa      = desp-embarque.cod-desp.
        END.

        ASSIGN es_importacao.codEmitente        = i-cod-emitente       
               es_importacao.DataDI             = embarque-imp.data-DI           
               es_importacao.DataEmbarque       = IF AVAIL historico-embarque THEN (historico-embarque.dt-efetiva) ELSE ?
               es_importacao.NumeroDI           = embarque-imp.declaracao-import 
               es_importacao.CodigoINCONTERM    = embarque-imp.cod-incoterm      
               es_importacao.DescricaoDespesa   = IF AVAIL desp-imp THEN desp-imp.descricao ELSE ""
               es_importacao.ValorDespesa       = desp-embarque.val-desp         
               es_importacao.codMoedaDespesa    = desp-embarque.mo-codigo        
               es_importacao.ValorImpostos      = 0  
               es_importacao.DespesasRateadas   = 0. 

    END. /* FOR EACH desp-embarque */
END. /* FOR EACH embarque-imp */


IF  VALID-HANDLE(h-boin274) THEN DO:
    DELETE PROCEDURE h-boin274.
    ASSIGN h-boin274 = ?.
END.

IF  VALID-HANDLE(h-bocx225) THEN DO:
    DELETE PROCEDURE h-bocx225.
    ASSIGN h-bocx225 = ?.
END.
  

PROCEDURE pi-get-fornecedor :

    ASSIGN i-cod-emitente = 0.
    IF  VALID-HANDLE(h-bocx225) AND VALID-HANDLE(h-boin274) THEN DO:
        RUN findEmbarque IN h-bocx225 (INPUT embarque-imp.cod-estabel,
                                       INPUT embarque-imp.embarque,
                                       OUTPUT c-return-ordens-emb). 
        RUN getintfield IN h-bocx225 ("numero-ordem", OUTPUT i-numero-ordem).
        RUN findOrdem   IN h-boin274 (INPUT i-numero-ordem,
                                      OUTPUT c-return-ordem-compr).               
        RUN getintfield IN h-boin274 ("cod-emitente":U, OUTPUT i-cod-emitente). 
    END.

END PROCEDURE.

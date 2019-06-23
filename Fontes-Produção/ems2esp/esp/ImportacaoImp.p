/*******************************************************************************
Extracao de Importacao
Kraft Consulting
29/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE INPUT  PARAMETER dir-extrat AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER dias-extrat AS integer   NO-UNDO.

DEF VAR c-arquivo            AS CHAR                        NO-UNDO.
DEF VAR c-return-ordens-emb  AS CHAR                        NO-UNDO.
DEF VAR c-return-ordem-compr AS CHAR                        NO-UNDO.
DEF VAR i-numero-ordem       LIKE ordem-compra.numero-ordem NO-UNDO.
DEF VAR i-cod-emitente       LIKE ordem-compra.cod-emitente NO-UNDO.
DEF VAR h-boin274            AS HANDLE                      NO-UNDO.
DEF VAR h-bocx225            AS HANDLE                      NO-UNDO.

DEFINE TEMP-TABLE ttExtrator   NO-UNDO
       FIELD IdEmitente        LIKE emitente.cod-emitente
       FIELD IdEstabelecimento LIKE embarque-imp.cod-estabel
       FIELD NumeroEmbarque    LIKE embarque-imp.embarque
       FIELD DataDI            LIKE embarque-imp.data-DI
       FIELD DataEmbarque      AS DATE
       FIELD NumeroDI          LIKE embarque-imp.declaracao-import
       FIELD CodigoINCOTERM    LIKE embarque-imp.cod-incoterm
       FIELD CodigoDespesa     LIKE desp-embarque.cod-desp
       FIELD DescricaoDespesa  LIKE desp-imp.descricao
       FIELD ValorDespesa      LIKE desp-embarque.val-desp
       FIELD IdMoedaDespesa    LIKE desp-embarque.mo-codigo
       FIELD ValorImpostos     AS DECIMAL 
       FIELD DespesasRateadas  AS DECIMAL
       FIELD aliquota-ipi  AS DEC
       FIELD aliquota-icm  AS DEC
       FIELD valor-ipi     AS DEC
       FIELD valor-icm     AS DEC
    . /* (no embarque) */

IF NOT VALID-HANDLE(h-boin274) THEN
    RUN inbo/boin274.p PERSISTENT SET h-boin274.

IF NOT VALID-HANDLE(h-bocx225) THEN
    RUN cxbo/bocx225.p PERSISTENT SET h-bocx225.

FOR EACH embarque-imp       
    WHERE embarque-imp.data-DI >= (TODAY - dias-extrat) NO-LOCK:

    RUN pi-get-fornecedor.

    for EACH docum-est
             where trim(substr(docum-est.char-1,1,12)) = embarque-imp.embarque no-lock:

        for each  item-doc-est fields( serie-docto   nro-docto      cod-emitente   nat-operacao  sequencia
                                       it-codigo     preco-unit[1]  numero-ordem   quantidade    qt-do-forn
                                       aliquota-ipi  aliquota-icm   valor-ipi[1]   valor-icm[1]  un parcela
                                       char-2 )
            where item-doc-est.serie-docto  =  docum-est.serie-docto
              and item-doc-est.nro-docto    =  docum-est.nro-docto  
              and item-doc-est.cod-emitente =  docum-est.cod-emitente
              and item-doc-est.nat-operacao =  docum-est.nat-operacao no-lock
            use-index documento                
            break by item-doc-est.it-codigo    
                  BY int(substr(item-doc-est.char-2,145,8))
                     by item-doc-est.numero-ordem:
    
            FIND FIRST item-doc-est-cex OF item-doc-est NO-LOCK NO-ERROR.

            FOR EACH desp-embarque NO-LOCK
               WHERE desp-embarque.cod-estabel = embarque-imp.cod-estabel 
                 AND desp-embarque.embarque    = embarque-imp.embarque:            
            
                FIND desp-imp NO-LOCK WHERE 
                     desp-imp.cod-desp = desp-embarque.cod-desp NO-ERROR.
        
                FIND FIRST historico-embarque OF embarque-imp NO-LOCK NO-ERROR.
        
                CREATE ttExtrator.
                ASSIGN ttExtrator.IdEmitente        = i-cod-emitente       
                       ttExtrator.IdEstabelecimento = embarque-imp.cod-estabel       
                       ttExtrator.NumeroEmbarque    = embarque-imp.embarque          
                       ttExtrator.DataDI            = embarque-imp.data-DI           
                       ttExtrator.DataEmbarque      = historico-embarque.dt-efetiva
                       ttExtrator.NumeroDI          = embarque-imp.declaracao-import 
                       ttExtrator.CodigoINCOTERM    = embarque-imp.cod-incoterm      
                       ttExtrator.CodigoDespesa     = desp-embarque.cod-desp         
                       ttExtrator.DescricaoDespesa  = IF AVAIL desp-imp THEN desp-imp.descricao ELSE ""
                       ttExtrator.ValorDespesa      = desp-embarque.val-desp         
                       ttExtrator.IdMoedaDespesa    = desp-embarque.mo-codigo        
                       ttExtrator.aliquota-ipi      = item-doc-est.aliquota-ipi
                       ttExtrator.aliquota-icm      = item-doc-est.aliquota-icm
                       ttExtrator.valor-ipi         = item-doc-est.valor-ipi[1]
                       ttExtrator.valor-icm         = item-doc-est.valor-icm[1]
                       ttExtrator.ValorImpostos     = 0  
                       ttExtrator.DespesasRateadas  = 0. 
        
            END. /* FOR EACH desp-embarque */
    
        END.
    end.

    
END. /* FOR EACH embarque-imp */

IF VALID-HANDLE(h-boin274) THEN
DO:
   DELETE PROCEDURE h-boin274.
   ASSIGN h-boin274 = ?.
END.

IF VALID-HANDLE(h-bocx225) THEN
DO:
   DELETE PROCEDURE h-bocx225.
   ASSIGN h-bocx225 = ?.
END.
   

ASSIGN c-arquivo = TRIM(dir-extrat) + "\BIImpostosImp.txt".
ASSIGN c-arquivo = REPLACE(c-arquivo,"\\","\").

RUN piExportaExtrator(INPUT "FOR EACH ttExtrator", "ttExtrator", c-arquivo).

FOR EACH ttExtrator:
    DELETE ttExtrator.
END.

PROCEDURE pi-get-fornecedor :

    ASSIGN i-cod-emitente = 0.
    IF VALID-HANDLE(h-bocx225) AND VALID-HANDLE(h-boin274) THEN
    DO:
        RUN findEmbarque IN h-bocx225 (INPUT embarque-imp.cod-estabel,
                                       INPUT embarque-imp.embarque,
                                       OUTPUT c-return-ordens-emb). 
        RUN getintfield IN h-bocx225 ("numero-ordem", OUTPUT i-numero-ordem).
        RUN findOrdem IN h-boin274 (INPUT i-numero-ordem,
                                    OUTPUT c-return-ordem-compr).               
        RUN getintfield IN h-boin274 ("cod-emitente":U, OUTPUT i-cod-emitente). 
    END.
END PROCEDURE.

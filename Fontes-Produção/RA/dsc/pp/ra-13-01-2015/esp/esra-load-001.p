
/*****************************************************************************
**  Programa: esra-load-001.p
**     Autor: 
**      Data: 
** Descricao: Carrega Parametros do RA pelo cruzamento do XML e Recebimento 
              Item Fornecedor e CFOP x Natureza
** Alteracao: 
******************************************************************************/
DEFINE VARIABLE c-diretorio AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-diretorio-dest AS CHARACTER   NO-UNDO.
DEFINE VARIABLE de-variacao AS DECIMAL     NO-UNDO.



{dsc/ra/include/esra-load-001.i}




/*Localizar Arquivos XML em uma pasta e definir a variacao */
ASSIGN c-diretorio      = "C:\temp\dsc\xmls\TODOS"
       c-diretorio-dest = "C:\temp\dsc\xmls\TODOS\lidos"
       de-variacao      = 10. 

RUN pi-localiza-arquivos(c-diretorio).


/* FOR EACH tt-arquivo-xml:                                                */
/*     DISP tt-arquivo-xml.nome             FORMAT "x(60)"                 */
/*          tt-arquivo-xml.caminho-completo FORMAT "x(80)" WITH WIDTH 300. */
/* END.                                                                    */


/*Carregar os XMLs para tt-nfe e tt-nfe-item*/

RUN dsc/ra/esp/esra-load-002.p(INPUT  TABLE tt-arquivo-xml,
                               OUTPUT TABLE tt-nfe,
                               OUTPUT TABLE tt-nfe-item,
                               OUTPUT TABLE tt-erros-leitura-xml).



FOR EACH tt-nfe :

    OS-COMMAND SILENT MOVE VALUE(c-diretorio + "\" + tt-nfe.arquivo) VALUE(c-diretorio-dest) .


END.


IF CAN-FIND (FIRST tt-erros-leitura-xml )THEN DO:

    OUTPUT TO "C:\temp\yamana\erros-leitura.csv".

    FOR EACH tt-erros-leitura-xml:

       PUT UNFORMATTED 
           tt-erros-leitura-xml.nome ";"
           tt-erros-leitura-xml.erro SKIP.
    END.

    OUTPUT CLOSE.

END.





/*Realizar Relacionamento na TT-planilha*/
RUN pi-relaciona-nfe-rec.




/*Gerar Excel*/

RUN dsc/ra/esp/esra-load-003.p(INPUT TABLE tt-planilha).






PROCEDURE pi-relaciona-nfe-rec:

    /*DEFINE VARIABLE de-varicar-aux AS DECIMAL     NO-UNDO.*/

    /*Seleciona as notas/itensque vamos usar*/
    FOR EACH tt-nfe:

        FIND FIRST emitente NO-LOCK
            WHERE emitente.cgc = tt-nfe.emit-cnpj NO-ERROR.

        FIND FIRST estabelec NO-LOCK
            WHERE estabelec.cgc = tt-nfe.dest-cnpj NO-ERROR.

        FOR EACH docum-est NO-LOCK
            WHERE docum-est.serie-docto  = tt-nfe.ide-serie
            AND   docum-est.nro-docto    = tt-nfe.ide-nnf
            AND   docum-est.cod-emitente = emitente.cod-emitente,
            EACH  item-doc-est OF docum-est:
            
            CREATE tt-item-doc-est-used.
            BUFFER-COPY item-doc-est TO tt-item-doc-est-used.
            ASSIGN tt-item-doc-est-used.chave-acesso = tt-nfe.chave-acesso
                   tt-item-doc-est-used.preco-max    = tt-item-doc-est-used.preco-total[1] + (tt-item-doc-est-used.preco-total[1] * (de-variacao / 100))
                   tt-item-doc-est-used.preco-min    = tt-item-doc-est-used.preco-total[1] - (tt-item-doc-est-used.preco-total[1] * (de-variacao / 100)).
                   


            


        END.
        /*Se caso nao encontre o documento no datasul*/
        IF NOT CAN-FIND(FIRST tt-item-doc-est-used
                        WHERE tt-item-doc-est-used.chave-acesso = tt-nfe.chave-acesso) THEN DO:

            ASSIGN tt-nfe.sem-doc = YES.


            CREATE tt-xml-sem-doc.
            ASSIGN tt-xml-sem-doc.chave-acesso = tt-nfe.chave-acesso
                   tt-xml-sem-doc.emit-cnpj    = tt-nfe.emit-cnpj 
                   tt-xml-sem-doc.emit-xnome   = tt-nfe.emit-xnome
                   tt-xml-sem-doc.ide-serie    = tt-nfe.ide-serie 
                   tt-xml-sem-doc.ide-nnf      = tt-nfe.ide-nnf   
                   tt-xml-sem-doc.ide-natOp    = tt-nfe.ide-natOp 
                   tt-xml-sem-doc.arquivo      = tt-nfe.arquivo   .



/*             OUTPUT TO VALUE(c-diretorio + "\" + "xml-sem-recebimento.txt") APPEND. */
/*                                                                                    */
/*                 PUT UNFORMATTED                                                    */
/*                     tt-nfe.emit-cnpj   ";"                                         */
/*                     tt-nfe.emit-xnome  ";"                                         */
/*                     tt-nfe.ide-serie   ";"                                         */
/*                     tt-nfe.ide-nnf     ";"                                         */
/*                     tt-nfe.ide-natOp   ";"                                         */
/*                     tt-nfe.arquivo    ";"  SKIP.                                   */
/*                                                                                    */
/*                                                                                    */
/*                                                                                    */
/*                                                                                    */
/*             OUTPUT CLOSE.                                                          */

        END.

    END.    


    /*Realizar o Relacionamento para Gerar a Planilha*/


    FOR EACH tt-nfe-item,
        FIRST tt-nfe
        WHERE tt-nfe.chave-acesso = tt-nfe-item.chave-acesso
        AND   tt-nfe.sem-doc      = NO:

        /*Primeiro vamos tentar encontrar usando a Sequencia e o Valor Total*/
        FIND FIRST tt-item-doc-est-used
            WHERE tt-item-doc-est-used.chave-acesso   = tt-nfe.chave-acesso
            AND   tt-item-doc-est-used.sequencia      = tt-nfe-item.det-nitem * 10
            /*AND   tt-item-doc-est-used.preco-total[1] = tt-nfe-item.det-vprod /*depois colocar a variacao*/ */
            AND   tt-item-doc-est-used.preco-max     >= tt-nfe-item.det-vprod
            AND   tt-item-doc-est-used.preco-min     <= tt-nfe-item.det-vprod
            AND   tt-item-doc-est-used.usado          = NO NO-ERROR.

        IF AVAIL  tt-item-doc-est-used THEN DO:

            FIND FIRST docum-est OF tt-item-doc-est-used NO-LOCK NO-ERROR.
                
            FIND FIRST emitente NO-LOCK
                WHERE emitente.cod-emitente = docum-est.cod-emitente NO-ERROR.

            
            

            CREATE tt-planilha.
            /*XML*/
            ASSIGN tt-planilha.chave-acesso = tt-nfe.chave-acesso
                   tt-planilha.ide-serie    = tt-nfe.ide-serie
                   tt-planilha.ide-nnf      = tt-nfe.ide-nnf
                   tt-planilha.cnpj         = tt-nfe.emit-cnpj
                   tt-planilha.xnome        = tt-nfe.emit-xnome  
                   tt-planilha.det-nitem    = tt-nfe-item.det-nitem 
                   tt-planilha.det-cfop     = tt-nfe-item.det-cfop  
                   tt-planilha.det-cprod    = tt-nfe-item.det-cprod 
                   tt-planilha.det-xprod    = tt-nfe-item.det-xprod 
                   tt-planilha.det-vprod    = tt-nfe-item.det-vprod .

            /*Datasul*/
            ASSIGN tt-planilha.cod-estabel  = docum-est.cod-estabel
                   tt-planilha.serie        = tt-item-doc-est-used.serie-docto       
                   tt-planilha.nr-docto     = tt-item-doc-est-used.nro-docto  
                   tt-planilha.cod-emitente = tt-item-doc-est-used.cod-emitente
                   tt-planilha.nome-abrev   = emitente.nome-abrev  
                   tt-planilha.seq          = tt-item-doc-est-used.sequencia         
                   tt-planilha.it-codigo    = tt-item-doc-est-used.it-codigo   
                   tt-planilha.nat-operacao = tt-item-doc-est-used.nat-operacao
                   tt-planilha.preco-tot    = tt-item-doc-est-used.preco-total[1] .

            /*Situacao*/
            ASSIGN tt-planilha.sit-seq   = "OK"
                   tt-planilha.sit-preco = "OK"
                   tt-planilha.var-preco = 100 - ((tt-nfe-item.det-vprod / tt-item-doc-est-used.preco-total[1]) * 100) . 

            ASSIGN tt-item-doc-est-used.usado     = YES
                   tt-item-doc-est-used.det-nitem = tt-nfe-item.det-nitem.

        END.
        ELSE DO:

            /*Se nao vamos tentar encontrar usando so o Valor Total*/

            FIND FIRST tt-item-doc-est-used
                WHERE tt-item-doc-est-used.chave-acesso   = tt-nfe.chave-acesso
                /*AND   tt-item-doc-est-used.preco-total[1] = tt-nfe-item.det-vprod /*depois colocar a variacao*/ */
                AND   tt-item-doc-est-used.preco-max     >= tt-nfe-item.det-vprod
                AND   tt-item-doc-est-used.preco-min     <= tt-nfe-item.det-vprod
                AND   tt-item-doc-est-used.usado          = NO NO-ERROR.

            IF AVAIL tt-item-doc-est-used THEN DO:

                FIND FIRST docum-est OF tt-item-doc-est-used NO-LOCK NO-ERROR.
                
                FIND FIRST emitente NO-LOCK
                    WHERE emitente.cod-emitente = docum-est.cod-emitente NO-ERROR.
    
                CREATE tt-planilha.
                /*XML*/
                ASSIGN tt-planilha.chave-acesso = tt-nfe.chave-acesso
                       tt-planilha.ide-serie    = tt-nfe.ide-serie
                       tt-planilha.ide-nnf      = tt-nfe.ide-nnf
                       tt-planilha.cnpj         = tt-nfe.emit-cnpj
                       tt-planilha.xnome        = tt-nfe.emit-xnome  
                       tt-planilha.det-nitem    = tt-nfe-item.det-nitem 
                       tt-planilha.det-cfop     = tt-nfe-item.det-cfop  
                       tt-planilha.det-cprod    = tt-nfe-item.det-cprod 
                       tt-planilha.det-xprod    = tt-nfe-item.det-xprod 
                       tt-planilha.det-vprod    = tt-nfe-item.det-vprod .
    
                /*Datasul*/
                ASSIGN tt-planilha.cod-estabel  = docum-est.cod-estabel
                       tt-planilha.serie        = tt-item-doc-est-used.serie-docto       
                       tt-planilha.nr-docto     = tt-item-doc-est-used.nro-docto    
                       tt-planilha.cod-emitente = tt-item-doc-est-used.cod-emitente
                       tt-planilha.nome-abrev   = emitente.nome-abrev  
                       tt-planilha.seq          = tt-item-doc-est-used.sequencia         
                       tt-planilha.it-codigo    = tt-item-doc-est-used.it-codigo   
                       tt-planilha.nat-operacao = tt-item-doc-est-used.nat-operacao
                       tt-planilha.preco-tot    = tt-item-doc-est-used.preco-total[1] .
    
                /*Situacao*/
                ASSIGN tt-planilha.sit-seq   = "NO"
                       tt-planilha.sit-preco = "OK"
                       tt-planilha.var-preco = 0 . /*Colocar*/
    
                ASSIGN tt-item-doc-est-used.usado     = YES
                       tt-item-doc-est-used.det-nitem = tt-nfe-item.det-nitem.


            END.                                                              

            ELSE DO:

                /*Colocar na tabela de erros de falha de relac*/

                CREATE tt-sem-relac.
                ASSIGN tt-sem-relac.chave-acesso = tt-nfe.chave-acesso
                       tt-sem-relac.ide-serie    = tt-nfe.ide-serie
                       tt-sem-relac.ide-nnf      = tt-nfe.ide-nnf  
                       tt-sem-relac.cnpj         = tt-nfe.emit-cnpj     
                       tt-sem-relac.xnome        = tt-nfe.emit-xnome 
                       tt-sem-relac.det-nitem    = tt-nfe-item.det-nitem 
                       tt-sem-relac.det-cfop     = tt-nfe-item.det-cfop  
                       tt-sem-relac.det-cprod    = tt-nfe-item.det-cprod 
                       tt-sem-relac.det-xprod    = tt-nfe-item.det-xprod 
                       tt-sem-relac.det-vprod    = tt-nfe-item.det-vprod .


            END.

        END.

    END.

    /*Sera tirada dos logs os itens que ja foram relacionadas*/

    DEFINE VARIABLE l-vai-sair-da-planilha  AS LOGICAL     NO-UNDO.

    FOR EACH tt-sem-relac:

        FIND FIRST tt-planilha 
            WHERE tt-planilha.det-cprod = tt-sem-relac.det-cprod NO-ERROR.

        IF AVAIL tt-planilha THEN
            DELETE tt-sem-relac.
    END.    
    /**/

    /*Tambem Sera retirada da planilha dos documentos que nao foram encontrados e seus itens ja foram relacionados*/

    FOR EACH tt-xml-sem-doc:

        FOR EACH tt-nfe 
            WHERE tt-nfe.chave-acesso = tt-xml-sem-doc.chave-acesso:

            ASSIGN l-vai-sair-da-planilha = YES.

            FOR EACH tt-nfe-item 
                WHERE tt-nfe-item.chave-acesso = tt-nfe.chave-acesso:

                FIND FIRST tt-planilha
                    WHERE tt-planilha.det-cprod = tt-nfe-item.det-cprod NO-ERROR.

                IF NOT AVAIL tt-planilha THEN
                    ASSIGN l-vai-sair-da-planilha = NO.

            END.

            IF l-vai-sair-da-planilha = YES THEN DO:

                ASSIGN tt-xml-sem-doc.com-principal = NO.

            END.
            ELSE 
                ASSIGN tt-xml-sem-doc.com-principal = YES.

        END.

    END.


    /**/


    OUTPUT TO VALUE(c-diretorio + "\" + "Itens-sem-relacionamento.txt").

    PUT "Item sem Relacionamento (Com Documento encontrado no Datasul)"
        SKIP
        "Chave Acesso;Serie;Nr.Nota;CNPJ Emitente;Nome Emitente;Sequencia;CFOP;Item;Descricao;Valor Total"
        SKIP.
    FOR EACH tt-sem-relac:


        PUT UNFORMATTED
            tt-sem-relac.chave-acesso ";"
            tt-sem-relac.ide-serie    ";"
            tt-sem-relac.ide-nnf      ";"
            tt-sem-relac.cnpj         ";"
            tt-sem-relac.xnome        ";"
            tt-sem-relac.det-nitem    ";"
            tt-sem-relac.det-cfop     ";"
            tt-sem-relac.det-cprod    ";"
            tt-sem-relac.det-xprod    ";"
            tt-sem-relac.det-vprod    ";" SKIP.


    END.
    OUTPUT CLOSE.

        
    OUTPUT TO VALUE(c-diretorio + "\" + "xml-sem-recebimento.txt").
    PUT "XMLs Sem Recebimento"
        SKIP
        "CNPJ;Fornecedor;Serie;Numero;Descricao Natureza;Chave"
        SKIP.

    FOR EACH tt-xml-sem-doc:

        PUT UNFORMATTED
            tt-xml-sem-doc.emit-cnpj   ";"
            tt-xml-sem-doc.emit-xnome  ";"
            tt-xml-sem-doc.ide-serie   ";"
            tt-xml-sem-doc.ide-nnf     ";"
            tt-xml-sem-doc.ide-natOp   ";"
            tt-xml-sem-doc.arquivo     ";" SKIP.




    END.
        
    OUTPUT CLOSE.                                                         






END PROCEDURE.













PROCEDURE pi-localiza-arquivos.

    DEFINE INPUT PARAMETER p-diretorio AS CHAR NO-UNDO.

    DEFINE VARIABLE c-arq   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-atrib AS CHARACTER   NO-UNDO.

    INPUT FROM OS-DIR (p-diretorio).

    REPEAT:
        IMPORT c-arq ^ c-atrib.
       

        IF c-atrib            = "f"   AND
           entry(2,c-arq,".") = "xml" THEN DO:

            CREATE tt-arquivo-xml.
            ASSIGN tt-arquivo-xml.nome             = c-arq
                   tt-arquivo-xml.caminho-completo = p-diretorio + "\" + c-arq.


        END.
    END.
    


END PROCEDURE.










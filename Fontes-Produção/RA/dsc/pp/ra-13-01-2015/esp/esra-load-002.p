/*****************************************************************************
**  Programa: esra-load-002.p
**     Autor: 
**      Data: 
** Descricao: Carrega Notas Fiscais Eletr?nicas para temp-table
** Alteracao: 
******************************************************************************/


{dsc/ra/include/esra-load-001.i}
{dsc/ra/include/fc-ponto-dec.i}

    
DEFINE INPUT  PARAMETER TABLE FOR tt-arquivo-xml.
DEFINE OUTPUT PARAMETER TABLE FOR tt-nfe.
DEFINE OUTPUT PARAMETER TABLE FOR tt-nfe-item.
DEFINE OUTPUT PARAMETER TABLE FOR tt-erros-leitura-xml.
                                              

/* DEFINE VARIABLE c-dt-aux AS CHARACTER   NO-UNDO. */


DEFINE VARIABLE hDoc              AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRoot             AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade01        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade02        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade03        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade04        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade05        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade06        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade07        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCascade08        AS HANDLE     NO-UNDO.
DEFINE VARIABLE no-1              AS INTEGER    NO-UNDO.
DEFINE VARIABLE no-2              AS INTEGER    NO-UNDO.
DEFINE VARIABLE no-3              AS INTEGER    NO-UNDO.
DEFINE VARIABLE no-4              AS INTEGER    NO-UNDO.
DEFINE VARIABLE no-5              AS INTEGER    NO-UNDO.
DEFINE VARIABLE no-6              AS INTEGER    NO-UNDO.
DEFINE VARIABLE no-7              AS INTEGER    NO-UNDO.
DEFINE VARIABLE no-8              AS INTEGER    NO-UNDO.

DEFINE VARIABLE r-aux-nfe-item     AS ROWID       NO-UNDO.

/*** Variaveis de Acompanhamento ***/

/* DEF VAR h-acomp    AS HANDLE      NO-UNDO.                */
/*                                                           */
/* RUN utp/ut-acomp.p PERSISTENT SET h-acomp.                */
/*                                                           */
/* RUN pi-inicializar IN h-acomp (INPUT "Carregando XML's"). */





FOR EACH tt-arquivo-xml :

    
    CREATE X-DOCUMENT hDoc.
    CREATE X-NODEREF  hRoot.
    CREATE X-NODEREF  hCascade01.
    CREATE X-NODEREF  hCascade02.
    CREATE X-NODEREF  hCascade03.
    CREATE X-NODEREF  hCascade04.
    CREATE X-NODEREF  hCascade05.
    CREATE X-NODEREF  hCascade06.
    CREATE X-NODEREF  hCascade07.
    CREATE X-NODEREF  hCascade08.

             

    
    hDoc:LOAD ("file", tt-arquivo-xml.caminho-completo, FALSE) NO-ERROR.
    
                      
        /* --- Verifica Erro de Estrutura --- */
        IF ERROR-STATUS:GET-NUMBER(1) <> 0 THEN 
        DO:
            /*"Erro leitura XML"*/
            CREATE tt-erros-leitura-xml.
            ASSIGN tt-erros-leitura-xml.nome             = tt-arquivo-xml.nome            
                   tt-erros-leitura-xml.caminho-completo = tt-arquivo-xml.caminho-completo
                   tt-erros-leitura-xml.erro             = "Nao Foi Possivel leitura do XML".

            NEXT.
            
        END.
        hDoc:GET-DOCUMENT-ELEMENT (hRoot).

        
        REPEAT no-1 = 1 TO hRoot:NUM-CHILDREN:
            
            
            hRoot:GET-CHILD (hCascade01,no-1).

            REPEAT no-2 = 1 TO hCascade01:NUM-CHILDREN:
                hCascade01:GET-CHILD (hCascade02,no-2).

                IF hCascade02:NAME = "infNFe" THEN DO: 
                    
                    IF SUBSTRING(hCascade02:GET-ATTRIBUTE("Id"), 1,3) = "NFE" THEN DO:

                        CREATE tt-nfe.
                        ASSIGN tt-nfe.chave-acesso = SUBSTRING(hCascade02:GET-ATTRIBUTE("Id"), 4,44)
                               tt-nfe.arquivo      = tt-arquivo-xml.nome.


                    END.
                    ELSE DO:

                        CREATE tt-erros-leitura-xml.
                        ASSIGN tt-erros-leitura-xml.nome             = tt-arquivo-xml.nome            
                               tt-erros-leitura-xml.caminho-completo = tt-arquivo-xml.caminho-completo
                               tt-erros-leitura-xml.erro             = "Nao encotrei a Chave de acesso".

                        NEXT.

                    END.
                        
                END.

                REPEAT no-3 = 1 to hCascade02:NUM-CHILDREN:
                        hCascade02:GET-CHILD (hCascade03,no-3).
                            
                        REPEAT no-4 = 1 to hCascade03:NUM-CHILDREN:
                            hCascade03:GET-CHILD (hCascade04,no-4).
                                
                            REPEAT no-5 = 1 to hCascade04:NUM-CHILDREN:
                                hCascade04:GET-CHILD (hCascade05,no-5).
                                    
                                CASE hCascade03:NAME:
                                    WHEN "ide" THEN  /* ===> IDE - Identificacao da NF <=== */
                                    DO:
                                        CASE hCascade04:NAME:
                                            WHEN "serie"    THEN ASSIGN tt-nfe.ide-serie   = hCascade05:NODE-VALUE.
                                            WHEN "nNF"      THEN ASSIGN tt-nfe.ide-nnf     = FILL("0",7 - LENGTH(TRIM(hCascade05:NODE-VALUE))) + TRIM(hCascade05:NODE-VALUE).
                                            WHEN "natOp"    THEN ASSIGN tt-nfe.ide-natOp   = hCascade05:NODE-VALUE.
                                        END CASE.
                                    END.

                                    WHEN "emit" THEN  /* ===> EMIT - Emitente da NF <=== */
                                    DO:
                                        CASE hCascade04:NAME:
                                            WHEN "CNPJ"  THEN ASSIGN tt-nfe.emit-cnpj = hCascade05:NODE-VALUE.
                                            WHEN "xNome" THEN ASSIGN tt-nfe.emit-xnome = hCascade05:NODE-VALUE.
                                            
                                        END CASE. 
                                    END.
                                        
                                    WHEN "dest" THEN  /* ===> DEST - DESTINATARIO da NF <=== */
                                    DO:
                                        CASE hCascade04:NAME:
                                            WHEN "CNPJ"  THEN ASSIGN tt-nfe.dest-cnpj   = hCascade05:NODE-VALUE.
                                            
                                        END CASE.
                                        
                                    END.
                                    
                                END. /* CASE hCascade03:NAME.... */

                                REPEAT no-6 = 1 to hCascade05:NUM-CHILDREN:
                                    hCascade05:GET-CHILD(hCascade06,no-6).
                                    CASE hCascade03:NAME:
        
                                        WHEN "total" THEN  /* ===> TOTAL DA NF-e <=== */
                                        DO:
                                            CASE hCascade04:NAME:
                                                WHEN "ICMSTot" THEN  /* ===> TOTAL DO ICMS <=== */
                                                DO:
                                                    CASE hCascade05:NAME:
                                                        WHEN "vProd"    THEN ASSIGN tt-nfe.tot-vProd    = DECI(fc-ponto-dec(hCascade06:NODE-VALUE)).
                                                        WHEN "vNF"      THEN ASSIGN tt-nfe.tot-vNF      = DECI(fc-ponto-dec(hCascade06:NODE-VALUE)).
                                                        
                                                    END CASE.
                                                END.
                                                    
                                                
                                            END.
                                        END.
                                            
                                        WHEN "det" THEN  /* ===> Itens <=== */
                                        DO:
                                            CASE hCascade04:NAME:
                                                WHEN "prod" THEN
                                                DO:
                                                    
                                                    CASE hCascade05:NAME:
                                                        WHEN "cProd" THEN 
                                                        DO:

                                                            FIND FIRST tt-nfe-item 
                                                                WHERE  tt-nfe-item.chave-acesso  = tt-nfe.chave-acesso
                                                                AND    tt-nfe-item.det-nitem     = INTE(hCascade03:GET-ATTRIBUTE("nItem"))  NO-ERROR.

                                                            IF NOT AVAIL tt-nfe-item THEN 
                                                            DO:
                                                                CREATE tt-nfe-item.

                                                                ASSIGN tt-nfe-item.chave-acesso = tt-nfe.chave-acesso
                                                                       tt-nfe-item.det-nitem    = INTE(hCascade03:GET-ATTRIBUTE("nItem")).

                                                                ASSIGN tt-nfe-item.det-cprod  = hCascade06:NODE-VALUE
                                                                       r-aux-nfe-item         = ROWID(tt-nfe-item).
                                                            END.
                                                            ELSE
                                                                ASSIGN r-aux-nfe-item = ROWID(tt-nfe-item).
                                                        END.
                                                            
                                                        WHEN "xProd    " THEN 
                                                        DO:
                                                            RUN pi-posiciona-item (INPUT r-aux-nfe-item).

                                                            ASSIGN tt-nfe-item.det-xProd = hCascade06:NODE-VALUE.
                                                        END.
                                                            
                                                        WHEN "vUnTrib  " THEN ASSIGN tt-nfe-item.det-vuntrib  = DECI(fc-ponto-dec(hCascade06:NODE-VALUE)).
                                                        WHEN "vUnCom   " THEN ASSIGN tt-nfe-item.det-vuncom   = DECI(fc-ponto-dec(hCascade06:NODE-VALUE)).
                                                        WHEN "vProd    " THEN ASSIGN tt-nfe-item.det-vProd    = DECI(fc-ponto-dec(hCascade06:NODE-VALUE)).
                                                        WHEN "uTrib    " THEN ASSIGN tt-nfe-item.det-uTrib    = hCascade06:NODE-VALUE.
                                                        WHEN "uCom     " THEN ASSIGN tt-nfe-item.det-uCom     = hCascade06:NODE-VALUE.
                                                        WHEN "qTrib    " THEN ASSIGN tt-nfe-item.det-qTrib    = DECI(fc-ponto-dec(hCascade06:NODE-VALUE)).
                                                        WHEN "qCom     " THEN ASSIGN tt-nfe-item.det-qCom     = DECI(fc-ponto-dec(hCascade06:NODE-VALUE)).
                                                        WHEN "CFOP     " THEN ASSIGN tt-nfe-item.det-CFOP    = hCascade06:NODE-VALUE.
                                                        

                                                    END CASE.

                                                END.

                                            END CASE.

                                        END.

                                    END.    

                                END.

                            END.

                        END. 
                        
                    END. 

            END.

        END.

END.






PROCEDURE pi-posiciona-item:
    
    DEF INPUT PARAM pr-item AS ROWID NO-UNDO.
        
    FIND FIRST tt-nfe-item WHERE 
        ROWID(tt-nfe-item) = pr-item EXCLUSIVE-LOCK NO-ERROR.
        
END PROCEDURE.




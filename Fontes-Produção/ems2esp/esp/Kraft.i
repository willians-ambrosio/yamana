/* Procedures para auxilio na exportacao do arquivo texto de extracao */

DEFINE VARIABLE gSeparadorDecimal AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gDataAmericana AS LOGICAL  NO-UNDO.

ASSIGN 
    gSeparadorDecimal = ","
    gDataAmericana = NO.

FUNCTION fnRetiraAcentos RETURNS char (INPUT p-string AS char ).
    define var c-free-accent as char case-sensitive no-undo.
    
    assign c-free-accent = p-string
           c-free-accent =  replace(c-free-accent, 'ﬂ', 'A')
           c-free-accent =  replace(c-free-accent, '?', 'A')
           c-free-accent =  replace(c-free-accent, '?', 'A')
           c-free-accent =  replace(c-free-accent, 'õ', 'A')
           c-free-accent =  replace(c-free-accent, '?', 'A')
           c-free-accent =  replace(c-free-accent, '?', 'E')
           c-free-accent =  replace(c-free-accent, 'É', 'E')
           c-free-accent =  replace(c-free-accent, ' ', 'E')
           c-free-accent =  replace(c-free-accent, '?', 'E')
           c-free-accent =  replace(c-free-accent, '?', 'I')
           c-free-accent =  replace(c-free-accent, 'Ç', 'I')
           c-free-accent =  replace(c-free-accent, 'á', 'I')
           c-free-accent =  replace(c-free-accent, 'Õ', 'I')
           c-free-accent =  replace(c-free-accent, '?', 'O')
           c-free-accent =  replace(c-free-accent, 'ñ', 'O')
           c-free-accent =  replace(c-free-accent, 'Å', 'O')
           c-free-accent =  replace(c-free-accent, 'ı', 'O')
           c-free-accent =  replace(c-free-accent, '≤', 'O')
           c-free-accent =  replace(c-free-accent, '˘', 'U')
           c-free-accent =  replace(c-free-accent, 'Ï', 'U')
           c-free-accent =  replace(c-free-accent, '™', 'U')
           c-free-accent =  replace(c-free-accent, 'º', 'U')
           c-free-accent =  replace(c-free-accent, '–', 'Y')
           c-free-accent =  replace(c-free-accent, '?', 'Y')
           c-free-accent =  replace(c-free-accent, '‹', 'C')
           c-free-accent =  replace(c-free-accent, 'Ω', 'N')
           c-free-accent =  replace(c-free-accent, 'Œ', 'a')
           c-free-accent =  replace(c-free-accent, '?', 'a')
           c-free-accent =  replace(c-free-accent, '±', 'a')
           c-free-accent =  replace(c-free-accent, '„', 'a')
           c-free-accent =  replace(c-free-accent, '¡', 'a')
           c-free-accent =  replace(c-free-accent, '£', 'e')
           c-free-accent =  replace(c-free-accent, 'ç', 'e')
           c-free-accent =  replace(c-free-accent, '¶', 'e')
           c-free-accent =  replace(c-free-accent, 'ó', 'e')
           c-free-accent =  replace(c-free-accent, '∞', 'i')
           c-free-accent =  replace(c-free-accent, '—', 'i')
           c-free-accent =  replace(c-free-accent, 'ì', 'i')
           c-free-accent =  replace(c-free-accent, 'ë', 'i')
           c-free-accent =  replace(c-free-accent, '⁄', 'o')
           c-free-accent =  replace(c-free-accent, '©', 'o')
           c-free-accent =  replace(c-free-accent, 'Ö', 'o')
           c-free-accent =  replace(c-free-accent, 'ã', 'o')
           c-free-accent =  replace(c-free-accent, 'ä', 'o')
           c-free-accent =  replace(c-free-accent, '∫', 'u')
           c-free-accent =  replace(c-free-accent, '†', 'u')
           c-free-accent =  replace(c-free-accent, '˛', 'u')
           c-free-accent =  replace(c-free-accent, 'œ', 'u')
           c-free-accent =  replace(c-free-accent, 'Ä', 'y')
           c-free-accent =  replace(c-free-accent, 'ø', 'y')
           c-free-accent =  replace(c-free-accent, 'å', 'c')
           c-free-accent =  replace(c-free-accent, 'Ú', 'n')
           c-free-accent =  replace(c-free-accent, '≠', 'a')
           c-free-accent =  replace(c-free-accent, '¥', 'o')
           c-free-accent =  replace(c-free-accent, '&', 'E').
     
    return c-free-accent.
end function.


PROCEDURE piOutput.
    DEFINE INPUT PARAMETER pValor AS CHAR.
    DEFINE INPUT PARAMETER pSkip AS LOGICAL.

    ASSIGN pValor = REPLACE(pValor, CHR(13), " ").
    ASSIGN pValor = REPLACE(pValor, CHR(10), " ").
    ASSIGN pValor = REPLACE(pValor, CHR(09), " ").
    ASSIGN pValor = REPLACE(pValor, CHR(34), " ").
    
    DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
    DEFINE VARIABLE cMascara AS CHARACTER NO-UNDO.

    IF LENGTH(pValor) = 0 OR (pValor = ?) THEN
        ASSIGN cMascara = "X(1)".
    ELSE
        ASSIGN cMascara = "X(" + TRIM(STRING(LENGTH(pValor))) + ")".
        
    IF pSkip THEN
        PUT pValor FORMAT cMascara SKIP.
    ELSE
        PUT pValor FORMAT cMascara.
END.

PROCEDURE piAcertaSaida.
    DEFINE INPUT PARAMETER pTipo AS CHAR.
    DEFINE INPUT-OUTPUT PARAMETER pValor AS CHAR.

    DEFINE VARIABLE dtValor AS DATE       NO-UNDO.

    /* retornar 1 ou 0 para campos logicos */
    /*IF pTipo = "LOGICAL" THEN DO:
        IF LOGICAL(pValor) THEN
            ASSIGN pValor = "1".
        ELSE
            ASSIGN pValor = "0".
    END.*/
    
    /*IF pTipo = "CHARACTER" THEN DO:
       ASSIGN pValor = SUBSTRING(TRIM(pValor), 1, 255).
    END*/.

    /* retornar Mes/Dia/Ano para datas */
    IF pTipo = "DATE" THEN DO:
        ASSIGN dtValor = ?.
        ASSIGN dtValor = DATE(pValor).
        IF dtValor = ? THEN
            ASSIGN dtValor = DATE(01,01,1900).

        IF dtValor < 01/01/1900 THEN
            ASSIGN dtValor = DATE(01,01,1900).

        IF gDataAmericana THEN
            ASSIGN pValor = STRING(MONTH(dtValor), "99") + "/" + STRING(DAY(dtValor), "99") + "/" + STRING(YEAR(dtValor), "9999").
        ELSE
            ASSIGN pValor = STRING(DAY(dtValor), "99") + "/" + STRING(MONTH(dtValor), "99") + "/" + STRING(YEAR(dtValor), "9999").
    END.

    /* Retornar Campos numericos com o ponto decimals no lugar de virgula */
    IF pTipo = "DECIMAL" THEN DO:
        IF pValor = ? THEN ASSIGN pValor = "0".
        ASSIGN 
            pValor = REPLACE(pValor, "?", "0")
            pValor = REPLACE(pValor, SESSION:NUMERIC-SEPARATOR, "")
            pValor = REPLACE(pValor, SESSION:NUMERIC-DECIMAL-POINT, gSeparadorDecimal).
    END.

    /* Retornar campos inteiros sem o separador de milhar */
    IF pTipo = "INTEGER" THEN DO:
        ASSIGN pValor = REPLACE(pValor, SESSION:NUMERIC-SEPARATOR, "").
    END.
END.

PROCEDURE piExportaExtrator.
    DEFINE INPUT PARAMETER pQueryString AS CHAR.
    DEFINE INPUT PARAMETER pTabela AS CHAR.
    DEFINE INPUT PARAMETER pArquivo AS CHAR.

    /* definindo variaveis */
    DEFINE VARIABLE hQuery AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffer AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE hField AS WIDGET NO-UNDO.
    DEFINE VARIABLE iField AS INTEGER NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSkip AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cValor AS CHARACTER  NO-UNDO.

    /* criando o buffer */
    CREATE BUFFER hBuffer FOR TABLE pTabela.

    /* criando a query */
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE(pQueryString).
    hQuery:QUERY-OPEN.

    /* criando arquivo de saida */
    /*OUTPUT TO VALUE("D:\DadosBI\TXT\" + pArquivo) NO-CONVERT.*/

    OUTPUT TO VALUE(pArquivo) NO-CONVERT.

    /* imprimindo o cabecalho */
    /* Aqui pega-se o fied-name do nome do campo da temptable*/
    DO iField = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN hField = hBuffer:BUFFER-FIELD(iField).

        ASSIGN lSkip = (iField = hBuffer:NUM-FIELDS).

        IF hField:EXTENT > 0 THEN DO:
            DO iExtent = 1 TO hField:EXTENT:
                IF iExtent = hField:EXTENT THEN
                    RUN piOutput(INPUT hField:NAME + "_" + TRIM(STRING(iExtent)), lSkip).
                ELSE
                    RUN piOutput(INPUT hField:NAME + "_" + TRIM(STRING(iExtent)), NO).

                IF iExtent < hField:EXTENT THEN RUN piOutput(INPUT "|", NO).
            END.
        END. ELSE DO:
            RUN piOutput(INPUT hField:NAME, lSkip).
        END.

        
        IF iField < hBuffer:NUM-FIELDS THEN 
            RUN piOutput(INPUT "|", NO).
        /*ELSE
            RUN piOutput(INPUT "", YES).
        */            
    END.


    /* imprindo os valores */
    hQuery:GET-FIRST().
    DO WHILE NOT hQuery:QUERY-OFF-END:
        DO iField = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN hField = hBuffer:BUFFER-FIELD(iField).
    
            ASSIGN 
                lSkip = (iField = hBuffer:NUM-FIELDS)
                cValor = "".

            IF hField:EXTENT > 0 THEN DO:
                DO iExtent = 1 TO hField:EXTENT:
                    ASSIGN cValor = TRIM(REPLACE(STRING(hField:BUFFER-VALUE(iExtent)), "|", "/")).
                    /*habilitar para retirar acentos caso necessario*/
                    ASSIGN cValor = fnRetiraAcentos(cValor).

                    RUN piAcertaSaida (INPUT hField:DATA-TYPE, INPUT-OUTPUT cValor).

                    IF iExtent = hField:EXTENT THEN
                        RUN piOutput(INPUT cValor, lSkip).
                    ELSE
                        RUN piOutput(INPUT cValor, NO).

                    
                    IF iExtent < hField:EXTENT THEN RUN piOutput(INPUT "|", NO).
                END.
            END. ELSE DO:
                ASSIGN cValor = TRIM(REPLACE(STRING(hField:BUFFER-VALUE),  "|", "/")).
                /*habilitar para retirar acentos caso necessario*/
                ASSIGN cValor = fnRetiraAcentos(cValor).

                RUN piAcertaSaida (INPUT hField:DATA-TYPE, INPUT-OUTPUT cValor).

                RUN piOutput(INPUT cValor, lSkip).
            END.
    
            
            IF iField < hBuffer:NUM-FIELDS THEN 
                RUN piOutput(INPUT "|", NO).
            /*ELSE
                RUN piOutput(INPUT ?, YES).
            */                
        END.

        hQuery:GET-NEXT().
    END.

    /* fechando o arquivo de saida */
    OUTPUT CLOSE.

    /* fechando a query */
    hQuery:QUERY-CLOSE.

    /* excluindo os handles dos objetos */
    DELETE WIDGET hQuery.
    DELETE WIDGET hBuffer.
END PROCEDURE.



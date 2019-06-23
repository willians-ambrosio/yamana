DEFINE INPUT PARAMETER p-cod-beneficio LIKE es-beneficio-ncm.cod-beneficio.

DEFINE BUFFER b-es-beneficio-ncm FOR es-beneficio-ncm.

DEFINE VARIABLE l-continua   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-linha-cont AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-nula AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-erros      AS INTEGER     NO-UNDO.
DEFINE VARIABLE h-acomp      AS HANDLE      NO-UNDO.

DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE wc-arq AS CHARACTER   NO-UNDO.

DEFINE VARIABLE ncm-ini AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ncm-fim AS CHARACTER   NO-UNDO.

DEFINE VARIABLE l-aux  AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE tt-import NO-UNDO
    FIELD ncm-ini     AS CHAR
    FIELD ncm-fim     AS CHAR.


DEFINE TEMP-TABLE tt-error NO-UNDO
    FIELD descricao AS CHAR.


  EMPTY TEMP-TABLE tt-import.

  ASSIGN ncm-ini  = ""
         ncm-fim  = ""
         wc-arq   = "".

  SYSTEM-DIALOG GET-FILE wc-arq        
        TITLE      "Escolha o Arquivo de Origem"
        FILTERS    "Arquivos Excel (*.xlsx)"   "*.xlsx", 
                   "Todos os Arquivos (*.*)"   "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE l-aux.


    IF l-aux THEN DO:

        RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
        {utp/ut-liter.i Verificando Excel *}
    
        RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    
        CREATE "Excel.Application"  chExcelApplication.
        chWorkbook  = chExcelApplication:workBooks:ADD(wc-arq).
    
        chExcelApplication:Visible = FALSE NO-ERROR.
    
        chWorkSheet = chExcelApplication:Sheets:Item(1).
    
        ASSIGN i-linha-cont = 0
               i-erros      = 0
               l-continua   = YES.
        
        DO WHILE l-continua :


            i-linha-cont = i-linha-cont + 1.

            IF i-linha-cont <= 1 THEN NEXT.

            IF chWorkSheet:Range('A' + STRING(i-linha-cont)):VALUE = " " OR chWorkSheet:Range('A' + STRING(i-linha-cont)):VALUE = ? THEN DO:
                ASSIGN i-linha-nula = i-linha-nula + 1.
                    ASSIGN l-continua = FALSE.
                NEXT.
            END.

            ASSIGN ncm-ini = string(chWorkSheet:Range('A' + STRING(i-linha-cont)):VALUE,"99999999")            
                   ncm-fim = string(chWorkSheet:Range('B' + STRING(i-linha-cont)):VALUE,"99999999").

            run pi-acompanhar in h-acomp (input "Linha: " + string(i-linha-cont)).

            CREATE tt-import.
            ASSIGN tt-import.ncm-ini  = ncm-ini
                   tt-import.ncm-fim  = ncm-fim. 

        END.


        RUN pi-finalizar IN h-acomp.

    END.

    
    IF VALID-HANDLE(chexcelApplication) THEN
        RELEASE OBJECT chexcelApplication.
        
    IF VALID-HANDLE(chWorkbook) THEN
        RELEASE OBJECT chWorkbook.
    
    IF VALID-HANDLE(chWorkSheet) THEN
        RELEASE OBJECT chWorksheet.

    FOR EACH tt-import NO-LOCK:

        IF tt-import.ncm-ini > tt-import.ncm-fim THEN DO:
            CREATE tt-error.
            ASSIGN tt-error.descrica = "NCM Inicial: " + tt-import.ncm-ini +  " Maior que NCM Final: " + tt-import.ncm-fim. 
            NEXT.
        END.

        FIND FIRST classif-fisc NO-LOCK
             WHERE classif-fisc.class-fiscal = tt-import.ncm-ini NO-ERROR.
        IF NOT AVAIL(classif-fisc) THEN DO:
            CREATE tt-error.
            ASSIGN tt-error.descrica = "NCM Inexistente: " + tt-import.ncm-ini.
            NEXT.
        END.
        ELSE DO:

            FIND FIRST classif-fisc NO-LOCK
                 WHERE classif-fisc.class-fiscal = tt-import.ncm-fim NO-ERROR.
            IF NOT AVAIL(classif-fisc) THEN DO:
                CREATE tt-error.
                ASSIGN tt-error.descrica = "NCM Inexistente: " + tt-import.ncm-fim.
                NEXT.
            END.

        END.

        FIND FIRST b-es-beneficio-ncm
             WHERE b-es-beneficio-ncm.class-fisc-ini = tt-import.ncm-ini
               AND b-es-beneficio-ncm.class-fisc-fim = tt-import.ncm-fim
               AND b-es-beneficio-ncm.cod-beneficio  = p-cod-beneficio NO-ERROR.
        IF AVAIL(b-es-beneficio-ncm) THEN DO:
            CREATE tt-error.
            ASSIGN tt-error.descrica = "Class. Fiscal Ini: " + tt-import.ncm-ini + " Class. Fiscal Fim: " + tt-import.ncm-fim + " Beneficio: " + string(p-cod-beneficio) + " j  existente". 
            NEXT.
        END.
        ELSE DO:

            CREATE b-es-beneficio-ncm.
            ASSIGN b-es-beneficio-ncm.class-fisc-ini  = tt-import.ncm-ini
                   b-es-beneficio-ncm.class-fisc-fim  = tt-import.ncm-fim
                   b-es-beneficio-ncm.cod-beneficio   = p-cod-beneficio.


        END.

    END. 


    FIND FIRST tt-error NO-LOCK NO-ERROR.
    IF AVAIL(tt-error) THEN DO:
        OUTPUT TO value(SESSION:TEMP-DIRECTORY + "tt-erros-ymof0103.csv").
        FOR EACH tt-error:
            EXPORT DELIMITER ';' tt-error.
        END.
        OUTPUT CLOSE.

        OS-COMMAND SILENT START VALUE(SESSION:TEMP-DIRECTORY + "tt-erros-ymof0103.csv").
    END.


/*****************************************************************************

    Programa    : esin001i01.i
    
    Objetivo    : Include para Abrir, Fechar a Planilha Excel ou Mudar de Pasta
    
    Autor       : DatasulWA - Minas Gerais - Eduardo Leite
    
    Data        : 12/03/2009
    
    Revis∆o     :
    
*****************************************************************************/    

case "{1}":

    /*** Novo arquivo - Sem Modelo ***/
    WHEN "Novo" THEN DO:

        CREATE "Excel.Application" chExcel. /* Cria a Planilha */
        ASSIGN ChBook  = chExcel:Workbooks:ADD()
               chsheet = chExcel:sheets:item(1).
        
    END.

    /* salva o excel */
    WHEN "Salvar" THEN DO:
        
        ChExcel:VISIBLE = FALSE.
        ChExcel:ActiveWorkbook:CLOSE(YES,cNomeArqDestino).

        ChExcel:QUIT().
        RELEASE OBJECT ChExcel NO-ERROR.
       
    END.

    WHEN "Open" THEN DO:

       ChExcel:APPLICATION:DisplayAlerts = FALSE.

       ChExcel:Cells:SELECT.
       ChExcel:Cells:EntireColumn:AutoFit.
       ChExcel:VISIBLE = TRUE.
        
       RELEASE OBJECT chsheet NO-ERROR.
       RELEASE OBJECT ChBook  NO-ERROR.
       RELEASE OBJECT ChExcel  NO-ERROR.
       
    END.
end.


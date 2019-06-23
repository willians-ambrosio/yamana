DEFINE VARIABLE c-cgc            LIKE  emitente.cgc        NO-UNDO.
DEFINE VARIABLE c-arquivo        AS    CHARACTER           NO-UNDO.
DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE    NO-UNDO.
DEFINE VARIABLE i-cont-linha     AS    INTEGER             NO-UNDO.
DEFINE VARIABLE i-restante       AS    INTEGER             NO-UNDO.


FUNCTION fcLabel RETURN CHARACTER (INPUT ip-banco   AS CHARACTER,
                                   INPUT ip-tabela  AS CHARACTER,
                                   INPUT ip-campo   AS CHARACTER):

   {utp/ut-field.i value(ip-banco) value(ip-tabela) value(ip-campo) 1}

   RETURN RETURN-VALUE.
END FUNCTION.

PROCEDURE pi-inicia-excel:
   DEFINE INPUT PARAMETER ip-arquivo AS CHARACTER NO-UNDO.

   ASSIGN c-arquivo = ENTRY(1,ip-arquivo,".") + "_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".xlsx".
      
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo NO-ERROR.
  
   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "ESCD0422".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "LISTAGEM DE FORNECEDORES - ESCD0422 -  "  + STRING(TODAY).

   ASSIGN i-cont-linha = 3.

   ASSIGN ch-excel:columns( "A"    ):NumberFormat = "@"                                
          ch-excel:columns( "B"    ):NumberFormat = "@"                                
          ch-excel:columns( "C"    ):NumberFormat = "@"                                
          ch-excel:columns( "D"    ):NumberFormat = "@"                                
          ch-excel:columns( "E"    ):NumberFormat = "@"                                
          ch-excel:columns( "F"    ):NumberFormat = "@"                                
          ch-excel:columns( "G"    ):NumberFormat = "@"                                
          ch-excel:columns( "H"    ):NumberFormat = "@"                                
          ch-excel:columns( "I"    ):NumberFormat = "@"                                
          ch-excel:columns( "J"    ):NumberFormat = "@"                                
          ch-excel:columns( "K"    ):NumberFormat = "@"                                
          ch-excel:columns( "L"    ):NumberFormat = "@"                                
          ch-excel:columns( "M"    ):NumberFormat = "@"                       
          ch-excel:columns( "N"    ):NumberFormat = "@"                       
          ch-excel:columns( "O"    ):NumberFormat = "@"                                
          ch-excel:columns( "P"    ):NumberFormat = "@"                                
          ch-excel:columns( "Q"    ):NumberFormat = "@"                                
          ch-excel:columns( "R"    ):NumberFormat = "@"                                
          ch-excel:columns( "S"    ):NumberFormat = "@"                                
          ch-excel:columns( "T"    ):NumberFormat = "@"                                
          ch-excel:columns( "U"    ):NumberFormat = "@"                     
          ch-excel:columns( "V"    ):NumberFormat = "@"                     
          ch-excel:columns( "W"    ):NumberFormat = "#.##0,00"                                
          ch-excel:columns( "X"    ):NumberFormat = "@"
          ch-excel:columns( "Y"    ):NumberFormat = "@"
          ch-excel:columns( "Z"    ):NumberFormat = "#.##0,00"
          ch-excel:columns( "AA"   ):NumberFormat = "dd/mm/aaaa;@"
          ch-excel:columns( "AB"   ):NumberFormat = "@"
          ch-excel:columns( "AC"   ):NumberFormat = "@"
          ch-excel:columns( "AD"   ):NumberFormat = "@"
          ch-excel:columns( "AE"   ):NumberFormat = "@"
          ch-excel:columns( "AF"   ):NumberFormat = "@"
          ch-excel:columns( "AG"   ):NumberFormat = "@"
          ch-excel:columns( "AH"   ):NumberFormat = "@"
          ch-excel:columns( "AI"   ):NumberFormat = "dd/mm/aaaa;@"
          ch-excel:columns( "AJ"   ):NumberFormat = "dd/mm/aaaa;@"
          ch-excel:columns( "AK"   ):NumberFormat = "@"
          ch-excel:columns( "AL"   ):NumberFormat = "@"
              .
          
          {utp/ut-field.i ems2cadme emitente cod-emitente 1} ch-excel:Range( "A"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE. 
          {utp/ut-field.i ems2cadme emitente nome-abrev   1} ch-excel:Range( "B"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente nome-emit    1} ch-excel:Range( "C"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.
          {utp/ut-field.i ems2cadme emitente cgc          1} ch-excel:Range( "D"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente nome-matriz  1} ch-excel:Range( "E"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente ins-estadual 1} ch-excel:Range( "F"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente cod-gr-forn  1} ch-excel:Range( "G"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente endereco     1} ch-excel:Range( "H"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente bairro       1} ch-excel:Range( "I"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente cidade       1} ch-excel:Range( "J"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente estado       1} ch-excel:Range( "K"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente cep          1} ch-excel:Range( "L"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente caixa-postal 1} ch-excel:Range( "M"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.    
          {utp/ut-field.i ems2cadme emitente telefax      1} ch-excel:Range( "N"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente telefone     1} ch-excel:Range( "O"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.  
          {utp/ut-field.i ems2cadme emitente telef-fac    1} ch-excel:Range( "P"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente telef-modem  1} ch-excel:Range( "Q"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente ramal-fax    1} ch-excel:Range( "R"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente telex        1} ch-excel:Range( "S"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente nat-operacao 1} ch-excel:Range( "T"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente telefone     1} ch-excel:Range( "U"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.  
          {utp/ut-field.i ems2cadme emitente atividade    1} ch-excel:Range( "V"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.    
          {utp/ut-field.i ems2cadme emitente taxa-financ  1} ch-excel:Range( "W"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente cod-cond-pag 1} ch-excel:Range( "X"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente linha-produt 1} ch-excel:Range( "Y"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente bonificacao  1} ch-excel:Range( "Z"   + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente data-taxa    1} ch-excel:Range( "AA"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente emissao-ped  1} ch-excel:Range( "AB"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente conta-corren 1} ch-excel:Range( "AC"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente cod-banco    1} ch-excel:Range( "AD"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.     
          {utp/ut-field.i ems2cadme emitente agencia      1} ch-excel:Range( "AE"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.    
          {utp/ut-field.i ems2cadme emitente pais         1} ch-excel:Range( "AF"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.
          {utp/ut-field.i ems2cadme emitente cod-transp   1} ch-excel:Range( "AG"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE. 

          ch-excel:Range( "AH"  + STRING(i-cont-linha,'99999')):VALUE = "Situa‡Æo".
          ch-excel:Range( "AI"  + STRING(i-cont-linha,'99999')):VALUE = "Vigˆncia Inicial".
          ch-excel:Range( "AJ"  + STRING(i-cont-linha,'99999')):VALUE = "Vigˆncia Final".

   &if '{&bf_mat_versao_ems}' >= '2.05' &then
      {utp/ut-field.i ems2cadme emitente log-controla-val-max-inss  1} ch-excel:Range( "AK"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.ch-excel:Range( "AK"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.
      {utp/ut-field.i ems2cadme emitente cod-inscr-inss             1} ch-excel:Range( "AL"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.ch-excel:Range( "AL"  + STRING(i-cont-linha,'99999')):VALUE = RETURN-VALUE.        
   &endif

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   DEFINE INPUT PARAMETER ip-destino AS INTEGER NO-UNDO.

   DEFINE VARIABLE c-final AS CHARACTER NO-UNDO.

   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   ASSIGN c-final = "AJ".

   &if '{&bf_mat_versao_ems}' >= '2.05' &then
      ASSIGN c-final = "AM".
   &endif

   ch-excel:Range("A3:" + c-final + string(i-cont-linha - 1)):autofilter(,,,).
   ch-excel:Range("A1:" + c-final + string(i-cont-linha - 1)):select.
   ch-excel:Range("A1:" + c-final + "3"):Font:Bold = TRUE.
       
   ch-excel:Columns("A"):ColumnWidth = 12.
   
   ch-excel:workbooks:item(1):SaveAs(c-arquivo,,,,,,).


   case ip-destino:
       when 1 then do:
          ch-excel:worksheets:item(1):select.
          ch-excel:Sheets:PrintOut.
          ch-excel:application:DisplayAlerts = false.
          ch-excel:quit().
       end.
       when 2 then do:
          ch-excel:visible = false.
          ch-excel:ActiveWorkbook:close(yes,c-arquivo).
       end.
       when 3 then do:
          ch-excel:visible = true.
       end.
   end case.

   release object ch-excel no-error.
END PROCEDURE.

PROCEDURE pi-dados-excel:
   ASSIGN ch-excel:Range( "A"   + STRING(i-cont-linha,'99999')):VALUE = emitente.cod-emitente                      
          ch-excel:Range( "B"   + STRING(i-cont-linha,'99999')):VALUE = emitente.nome-abrev                        
          ch-excel:Range( "C"   + STRING(i-cont-linha,'99999')):VALUE = emitente.nome-emit                                
          ch-excel:Range( "D"   + STRING(i-cont-linha,'99999')):VALUE = emitente.cgc                               
          ch-excel:Range( "E"   + STRING(i-cont-linha,'99999')):VALUE = emitente.nome-matriz                       
          ch-excel:Range( "F"   + STRING(i-cont-linha,'99999')):VALUE = emitente.ins-estadual                      
          ch-excel:Range( "G"   + STRING(i-cont-linha,'99999')):VALUE = emitente.cod-gr-forn                       
          ch-excel:Range( "H"   + STRING(i-cont-linha,'99999')):VALUE = emitente.endereco                          
          ch-excel:Range( "I"   + STRING(i-cont-linha,'99999')):VALUE = emitente.bairro                            
          ch-excel:Range( "J"   + STRING(i-cont-linha,'99999')):VALUE = emitente.cidade                            
          ch-excel:Range( "K"   + STRING(i-cont-linha,'99999')):VALUE = emitente.estado                            
          ch-excel:Range( "L"   + STRING(i-cont-linha,'99999')):VALUE = emitente.cep                               
          ch-excel:Range( "M"   + STRING(i-cont-linha,'99999')):VALUE = emitente.caixa-postal                      
          ch-excel:Range( "N"   + STRING(i-cont-linha,'99999')):VALUE = emitente.telefax                           
          ch-excel:Range( "O"   + STRING(i-cont-linha,'99999')):VALUE = emitente.telefone[1]                       
          ch-excel:Range( "P"   + STRING(i-cont-linha,'99999')):VALUE = emitente.telef-fac                         
          ch-excel:Range( "Q"   + STRING(i-cont-linha,'99999')):VALUE = emitente.telef-modem                       
          ch-excel:Range( "R"   + STRING(i-cont-linha,'99999')):VALUE = emitente.ramal-fax                        
          ch-excel:Range( "S"   + STRING(i-cont-linha,'99999')):VALUE = emitente.telex                            
          ch-excel:Range( "T"   + STRING(i-cont-linha,'99999')):VALUE = emitente.nat-operacao                     
          ch-excel:Range( "U"   + STRING(i-cont-linha,'99999')):VALUE = emitente.telefone[2]                      
          ch-excel:Range( "V"   + STRING(i-cont-linha,'99999')):VALUE = emitente.atividade                        
          ch-excel:Range( "W"   + STRING(i-cont-linha,'99999')):VALUE = emitente.taxa-financ                      
          ch-excel:Range( "X"   + STRING(i-cont-linha,'99999')):VALUE = emitente.cod-cond-pag                     
          ch-excel:Range( "Y"   + STRING(i-cont-linha,'99999')):VALUE = emitente.linha-produt                     
          ch-excel:Range( "Z"   + STRING(i-cont-linha,'99999')):VALUE = emitente.bonificacao                      
          ch-excel:Range( "AA"  + STRING(i-cont-linha,'99999')):VALUE = emitente.data-taxa                        
          ch-excel:Range( "AB"  + STRING(i-cont-linha,'99999')):VALUE = emitente.emissao-ped                      
          ch-excel:Range( "AC"  + STRING(i-cont-linha,'99999')):VALUE = emitente.conta-corren                     
          ch-excel:Range( "AD"  + STRING(i-cont-linha,'99999')):VALUE = emitente.cod-banco                        
          ch-excel:Range( "AE"  + STRING(i-cont-linha,'99999')):VALUE = emitente.agencia                          
          ch-excel:Range( "AF"  + STRING(i-cont-linha,'99999')):VALUE = emitente.pais                                    
          ch-excel:Range( "AG"  + STRING(i-cont-linha,'99999')):VALUE = emitente.cod-transp                       
          ch-excel:Range( "AH"  + STRING(i-cont-linha,'99999')):VALUE = c-desc-situacao                                  
          ch-excel:Range( "AI"  + STRING(i-cont-linha,'99999')):VALUE = dt-vig-ini                                
          ch-excel:Range( "AJ"  + STRING(i-cont-linha,'99999')):VALUE = dt-vig-fim.                               

   &if '{&bf_mat_versao_ems}' >= '2.05' &then
      ASSIGN ch-excel:Range( "AK"  + STRING(i-cont-linha,'99999')):VALUE = emitente.log-controla-val-max-inss        
             ch-excel:Range( "AL"  + STRING(i-cont-linha,'99999')):VALUE = emitente.cod-inscr-inss.                   
   &endif                                    

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.



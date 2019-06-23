/*****************************************************************************
**  Programa: esra-load-100.p
**     Autor: 
**      Data: 
** Descricao: Programa Principal de Leitura da Planilha de Carga do RA
** Alteracao: 
******************************************************************************/


    
DEFINE VARIABLE excelApp  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i-linha   AS INTEGER    NO-UNDO.

DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-it-codigo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cod-estabel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-cod-emitente AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-item-do-forn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cfop         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nat-operacao AS CHARACTER   NO-UNDO.


DEFINE INPUT PARAMETER c-estab   AS CHAR.
DEFINE INPUT PARAMETER c-arquivo AS CHAR.

create "excel.application" excelapp.

excelapp:visible = FALSE.
excelapp:workbooks:add(c-arquivo). /*endere‡o da planilha*/
excelapp:worksheets:item(1):select.
    
    
ASSIGN i-linha = 2.


DO WHILE i-linha <= 600:

        ASSIGN i-linha  = i-linha + 1.

        IF excelapp:range("Q" + STRING(i-linha)):VALUE = ? OR
           excelapp:range("Q" + STRING(i-linha)):VALUE = "" THEN LEAVE.

        



    ASSIGN c-it-codigo    = excelapp:range("Q" + STRING(i-linha)):VALUE
           c-cod-estabel  = c-estab
           i-cod-emitente = int(excelapp:range("N" + STRING(i-linha)):VALUE)
           c-item-do-forn = excelapp:range("G" + STRING(i-linha)):VALUE
           c-cfop         = excelapp:range("I" + STRING(i-linha)):VALUE
           c-nat-operacao = excelapp:range("R" + STRING(i-linha)):VALUE.

    FIND FIRST ITEM NO-LOCK
        WHERE ITEM.it-codigo = trim(c-it-codigo) NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.


    /*DISP i-linha
         c-it-codigo    
         c-cod-estabel 
         i-cod-emitente
         c-item-do-forn
         c-cfop        
         c-nat-operacao. */

    IF ITEM.tipo-contr = 4 THEN NEXT.

    ASSIGN i-cont = i-cont + 1.


    
    
    FIND FIRST item-fornec no-lock
        WHERE item-fornec.it-codigo    = c-it-codigo    
        AND   item-fornec.cod-emitente = i-cod-emitente NO-ERROR.
        
    IF NOT AVAIL item-fornec THEN DO:
        
        CREATE item-fornec.
            
        ASSIGN item-fornec.it-codigo    = c-it-codigo   
               item-fornec.cod-emitente = i-cod-emitente.
            
        ASSIGN item-fornec.item-do-forn   = c-item-do-forn                         
               item-fornec.unid-med-for   = ITEM.un                                
               /*item-fornec.fator-conver   = tt-item-fornec.im-if-fator-conver    
               item-fornec.num-casa-dec   = tt-item-fornec.im-if-num-casa-dec */   
               item-fornec.tempo-ressup   = 0                                      
               item-fornec.ativo          = YES                                    
               item-fornec.lote-minimo    = 0                                      
               item-fornec.lote-mul-for   = 0                                      
               item-fornec.perc-compra    = 0                                      
               item-fornec.cot-aut        = NO                                     
               item-fornec.cod-cond-pag   = 1                                      
               item-fornec.classe-repro   = 1                                      
               /*item-fornec.perc-pont-forn = tt-item-fornec.im-perc-pont-forn*/   
               /*item-fornec.ind-pont       = tt-item-fornec.im-ind-pont*/         
               /*item-fornec.perc-dev-forn  = tt-item-fornec.im-perc-dev-forn*/    
               item-fornec.concentracao   = 0                                      
               item-fornec.rendimento     = 0                                      
               /*item-fornec.cod-mensagem   = tt-item-fornec.im-if-cod-msg      */ 
               item-fornec.contr-forn     = NO                                     
               item-fornec.hora-ini       = 0                                      
               item-fornec.hora-fim       = 0                                      
               item-fornec.reaj-tabela    = NO                                     
               item-fornec.conceito       = 0                                      
               item-fornec.observacao     = "Carga Recebimento Automatico RA"      
               item-fornec.serie-nota     = ""                                     
               item-fornec.numero-nota    = ""                                     
               item-fornec.horiz-fixo     = 0                                      
               item-fornec.ped-fornec     = 0                                      
               item-fornec.tp-inspecao    = 2                                      
               item-fornec.criticidade    = 1                                      
               /*item-fornec.qt-max-ordem   = tt-item-fornec.im-if-qt-max-ord   */ 
               item-fornec.niv-qua-ac     = 0                                      
               item-fornec.niv-inspecao   = 0.                                     


        FIND FIRST nfe-emit-cfop-nat no-lock
            WHERE nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel
            AND   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente
            AND   nfe-emit-cfop-nat.cod-cfop     = c-cfop
            AND   nfe-emit-cfop-nat.it-codigo    = c-it-codigo         NO-ERROR.
            
        IF NOT AVAIL nfe-emit-cfop-nat THEN DO:
            
            CREATE nfe-emit-cfop-nat.
                
            ASSIGN nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel 
                   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente
                   nfe-emit-cfop-nat.cod-cfop     = c-cfop        
                   nfe-emit-cfop-nat.nat-operacao = c-nat-operacao
                   nfe-emit-cfop-nat.it-codigo    = c-it-codigo .  
        END.
            
        FIND FIRST nfe-emit-cfop-nat no-lock
            WHERE nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel
            AND   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente                     
            AND   nfe-emit-cfop-nat.cod-cfop     = c-cfop 
            AND   nfe-emit-cfop-nat.it-codigo    = "*"                                       NO-ERROR.
            
        IF NOT AVAIL nfe-emit-cfop-nat THEN DO:
            
            CREATE nfe-emit-cfop-nat.
                
            ASSIGN nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel 
                   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente
                   nfe-emit-cfop-nat.cod-cfop     = c-cfop        
                   nfe-emit-cfop-nat.nat-operacao = c-nat-operacao
                   nfe-emit-cfop-nat.it-codigo    = "*".
        END.








    END.
    ELSE DO:

        FIND FIRST nfe-emit-cfop-nat no-lock
            WHERE nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel
            AND   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente
            AND   nfe-emit-cfop-nat.cod-cfop     = c-cfop
            AND   nfe-emit-cfop-nat.it-codigo    = c-it-codigo         NO-ERROR.
            
        IF NOT AVAIL nfe-emit-cfop-nat THEN DO:
            
            CREATE nfe-emit-cfop-nat.
                
            ASSIGN nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel 
                   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente
                   nfe-emit-cfop-nat.cod-cfop     = c-cfop        
                   nfe-emit-cfop-nat.nat-operacao = c-nat-operacao
                   nfe-emit-cfop-nat.it-codigo    = c-it-codigo .  
        END.
            
        FIND FIRST nfe-emit-cfop-nat no-lock
            WHERE nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel
            AND   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente                     
            AND   nfe-emit-cfop-nat.cod-cfop     = c-cfop 
            AND   nfe-emit-cfop-nat.it-codigo    = "*"                                       NO-ERROR.
            
        IF NOT AVAIL nfe-emit-cfop-nat THEN DO:
            
            CREATE nfe-emit-cfop-nat.
                
            ASSIGN nfe-emit-cfop-nat.cod-estabel  = c-cod-estabel 
                   nfe-emit-cfop-nat.cod-emitente = i-cod-emitente
                   nfe-emit-cfop-nat.cod-cfop     = c-cfop        
                   nfe-emit-cfop-nat.nat-operacao = c-nat-operacao
                   nfe-emit-cfop-nat.it-codigo    = "*".
        END.
    END.






          

  

END.

MESSAGE "FIM"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

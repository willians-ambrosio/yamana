/*
*---------------------------------------------------------*
* Programa: upc-fp14400.p
* objetivo: Avisar duplicidade de CPF
*            
* Autor   : Edson - Amgra 
* Data    : 09/12/2007
*---------------------------------------------------------*
*/

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT PARAM p-ind-object AS CHAR NO-UNDO.
DEF INPUT PARAM p-wgh-object AS handle NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS widget-handle NO-UNDO.
DEF INPUT PARAM p-cod-table  AS CHAR NO-UNDO.
DEF INPUT PARAM p-row-table  AS rowid NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-rg   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-dt   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cpf  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-func AS WIDGET-HANDLE NO-UNDO.

DEF VAR c-cpf    AS CHAR NO-UNDO.
DEF VAR wh-frame AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto AS CHAR NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF BUFFER b-rh_pessoa_fisic FOR rh_pessoa_fisic.

/*pega o nome do programa sem o diretorio e as barras*/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA,"~\"), p-wgh-object:PRIVATE-DATA, "~\").
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(c-objeto,"~/"), c-objeto, "~/").
/*get-link-handle  retorna todos os handles de um determinado tipo de link*/
/* primeiro parametro ‚ o nome da container, o segundo ‚ o tipo e o terceiro
‚ a variavel de retorno dos tipo links)*/

/* para decobrir nome das viewers no programa
depois abra uma tela nova instacie a viewer suspeita para ter certeza*/


IF  p-ind-event = "validate" AND
    c-objeto = "v01py257.w" THEN DO:
    ASSIGN c-cpf = REPLACE(REPLACE(wh-cpf:screen-value,".",""),"-","").

    FIND FIRST b-rh_pessoa_fisic NO-LOCK
         WHERE b-rh_pessoa_fisic.cod_id_feder      = c-cpf
           and b-rh_pessoa_fisic.num_pessoa_fisic <> INT(wh-func:input-value) NO-error.
    IF  AVAIL b-rh_pessoa_fisic THEN DO:
        DEFINE BUTTON db-bt-cancel AUTO-END-KEY 
             LABEL "&Fechar" 
             SIZE 10 BY 1
             BGCOLOR 8.
        
        DEFINE RECTANGLE db-rt-botoes
             EDGE-PIXELS 2 GRAPHIC-EDGE  
             SIZE 58 BY 1.42
             BGCOLOR 7.  
        
        DEFINE VARIABLE c-mensagem AS CHARACTER FORMAT  "X(45)" NO-UNDO.
        
        ASSIGN c-mensagem = 
            "Num.: " + STRING(b-rh_pessoa_fisic.num_pessoa_fisic) + " - " +
            b-rh_pessoa_fisic.nom_pessoa_fisic    + CHR(10) + 
            "CNPF: " + b-rh_pessoa_fisic.cod_id_feder + CHR(10) +
            "RG: " + WH-RG:SCREEN-VALUE +  CHR(10) + 
            "Data Nasc: " + wh-dt:SCREEN-VALUE.
        
        DEFINE RECTANGLE db-rect-1
            EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
            SIZE 59 BY 4.30.
        
        DEFINE FRAME db-frame-1
            c-mensagem NO-LABEL VIEW-AS EDITOR  SIZE 55 BY 3
               at ROW 2.7 col 2 NO-TAB-STOP FONT 0
            
            db-rect-1 AT ROW 1.9 COL 01
        
            db-bt-cancel      AT ROW 7.3 COL 23             
            db-rt-botoes      AT ROW 7.0 COL 1
            SPACE(0.28)
            WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                 THREE-D SCROLLABLE TITLE "* * * C N P F Duplicado! * * *" FONT 1
                 DEFAULT-BUTTON db-bt-cancel CANCEL-BUTTON db-bt-cancel.
        
        DISPLAY c-mensagem WITH FRAME db-frame-1.
        
        ASSIGN c-mensagem:SENSITIVE   = YES
               c-mensagem:read-only   = yes
               c-mensagem:AUTO-RESIZE = yes.
        
        ENABLE db-bt-cancel 
            WITH FRAME db-frame-1. 
        
        WAIT-FOR "GO":U OF FRAME db-frame-1.
        /* Fim do Dialog-box com Mensagem Final */
    END.
END.

IF  p-ind-event = "initialize" THEN DO:
/*       
      MESSAGE 
    p-ind-event  
    p-ind-object 
    p-wgh-object 
    p-wgh-frame:NAME  
    p-cod-table  
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    ASSIGN wh-frame = p-wgh-frame:FIRST-CHILD
            wh-frame = wh-frame:FIRST-CHILD.

    DO WHILE wh-frame <> ?:
        IF  wh-frame:TYPE <> "field-group" THEN DO:
            IF  wh-frame:NAME = "num_pessoa_fisic" THEN
                ASSIGN wh-func = wh-frame.

            IF  wh-frame:NAME = "dat_nascimento" THEN
                ASSIGN wh-dt = wh-frame.
        
            IF  wh-frame:NAME="cod_id_feder" THEN
                ASSIGN wh-cpf = wh-frame.

            IF  wh-frame:NAME = "cod_id_estad_fisic" THEN DO:
                ASSIGN wh-rg = wh-frame.
/*                 LEAVE. */
            END.
            ASSIGN wh-frame=wh-frame:NEXT-SIBLING.
        END.
        ELSE 
            ASSIGN wh-frame = wh-frame:FIRST-CHILD.
    END.
END.

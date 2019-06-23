/******************************************************************************************************************************************
** Programa: esp/esap3001rp.p
** Data    : 16-05-2015
** Objetivo: Relatorio Cria AVMN
********************************************************************************************************************************************/
DISABLE TRIGGERS FOR LOAD OF item-uni-estab.
/* include de controle de versÊo */
{include/i-prgvrs.i esap3001 "TOTVS-12"}

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.

DEFINE VARIABLE empresa   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE it-codigo AS CHARACTER   NO-UNDO.

DEFINE VARIABLE l-continua   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-linha-cont AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-nula AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-erros      AS INTEGER     NO-UNDO.
DEFINE VARIABLE h-acomp      AS HANDLE      NO-UNDO.

DEFINE VARIABLE c-itens AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i-cont-saldo       LIKE saldo-terc.quantidade.

DEF BUFFER b-item FOR ITEM.
DEF BUFFER b-item-uni-estab FOR item-uni-estab.

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD l-obsoletar      AS LOG.
    
DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

DEFINE TEMP-TABLE tt-import NO-UNDO
    FIELD empresa    AS CHAR
    FIELD it-codigo  AS CHAR .


DEFINE TEMP-TABLE tt-saida NO-UNDO
    FIELD empresa      AS CHAR
    FIELD it-codigo    AS CHAR 
    FIELD cod-obsoleto AS INT.

DEFINE TEMP-TABLE tt-error NO-UNDO
    FIELD descricao AS CHAR.

/* recebimento de parümetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*Carregando a tt-digita*/
create tt-param.
raw-transfer raw-param to tt-param.


ASSIGN c-itens = "Servi‡o - MCN,Item Contrato,Contratos,Contrato,SV000956,99300001".

RUN pi-importa-excel.
RUN pi-logica.


PROCEDURE pi-logica:
    FOR EACH ITEM NO-LOCK
       WHERE ITEM.it-codigo <> ""
         AND ITEM.cod-obsoleto = 1:


        /* exce‡äes */
        FIND FIRST tt-import NO-LOCK
             WHERE tt-import.empresa   = i-ep-codigo-usuario
               AND tt-import.it-codigo = ITEM.it-codigo NO-ERROR.
        IF AVAIL(tt-import) THEN 
            NEXT.
        
        IF LOOKUP(ITEM.it-codigo,c-itens) > 0 THEN NEXT.

        IF  SUBSTRING(ITEM.it-codigo,1,2) >= "AA" 
        AND SUBSTRING(ITEM.it-codigo,1,2) <= "ZZ" 
        AND SUBSTRING(ITEM.it-codigo,2,1) >= "A" 
        AND SUBSTRING(ITEM.it-codigo,2,1) <= "Z" THEN NEXT.

         /* exce‡äes */

        ASSIGN i-cont-saldo = 0.

        FOR EACH saldo-terc NO-LOCK
           WHERE saldo-terc.it-codigo  = ITEM.it-codigo
             AND saldo-terc.quantidade > 0 :
            
            ASSIGN i-cont-saldo = i-cont-saldo + saldo-terc.quantidade.
        END.



        IF i-cont-saldo > 0 THEN DO:
            FIND FIRST saldo-estoq no-lock
               WHERE /*saldo-estoq.cod-estabel = item.cod-estabel 
                 AND saldo-estoq.cod-localiz = item.cod-localiz              
                 AND*/ saldo-estoq.it-codigo   = item.it-codigo 
                 AND saldo-estoq.cod-refer   = item.cod-refer  no-error.
              /*IF AVAIL(saldo-estoq) THEN DO:*/
                
                    IF (AVAIL(saldo-estoq) AND saldo-estoq.qtidade-atu = 0) OR NOT AVAIL(saldo-estoq) THEN DO:

                        FIND FIRST b-item 
                             WHERE b-item.it-codigo = ITEM.it-codigo NO-ERROR.
                        IF AVAIL(b-item) THEN DO: 
                            IF tt-param.l-obsoletar = TRUE THEN DO:
                                ASSIGN b-item.cod-obsoleto = 3.
                           
                                FOR EACH b-item-uni-estab 
                                   WHERE b-item-uni-estab.it-codigo = ITEM.it-codigo:
    
                                   
                                        ASSIGN b-item-uni-estab.cod-obsoleto = 3.
                                    
    
                                END.
                            END.
                            RUN pi-cria-saida(INPUT ITEM.it-codigo,
                                              INPUT 3).
                        END.
                    END. /*saldo estoq*/
             /* END.  /*IF AVAIL(saldo-estoq)*/  */
        END.
        ELSE DO:

            IF ITEM.tipo-contr = 2 THEN DO:
    
    
                FIND FIRST saldo-estoq no-lock
                   WHERE /*saldo-estoq.cod-estabel = item.cod-estabel 
                     AND saldo-estoq.cod-localiz = item.cod-localiz              
                     AND*/ saldo-estoq.it-codigo   = item.it-codigo 
                     AND saldo-estoq.cod-refer   = item.cod-refer  no-error.
                  
                /*IF AVAIL(saldo-estoq) THEN DO:*/
                    
                        IF (AVAIL(saldo-estoq) AND saldo-estoq.qtidade-atu > 0) THEN DO:
    
                            FIND FIRST b-item 
                                 WHERE b-item.it-codigo = ITEM.it-codigo NO-ERROR.
                            IF AVAIL(b-item) THEN DO: 
                                IF tt-param.l-obsoletar = TRUE THEN DO:
                                    ASSIGN b-item.cod-obsoleto = 3.
                                

                                    FOR EACH b-item-uni-estab 
                                       WHERE b-item-uni-estab.it-codigo = ITEM.it-codigo:
            
                                        
                                            ASSIGN b-item-uni-estab.cod-obsoleto = 3.
                                        
            
                                    END.
                                END.

                                RUN pi-cria-saida(INPUT ITEM.it-codigo,
                                                  INPUT 3).
                            END.
                        END. /*saldo estoq*/
                        ELSE DO:
    
    
                            FIND FIRST b-item 
                                 WHERE b-item.it-codigo = ITEM.it-codigo NO-ERROR.
                            IF AVAIL(b-item) THEN DO:
                                IF tt-param.l-obsoletar = TRUE THEN DO:
                                    ASSIGN b-item.cod-obsoleto = 4.
    
                                    FOR EACH b-item-uni-estab 
                                       WHERE b-item-uni-estab.it-codigo = ITEM.it-codigo:
            
                                        
                                            ASSIGN b-item-uni-estab.cod-obsoleto = 4.
                                       
            
                                    END.

                                END.
    
                                RUN pi-cria-saida(INPUT ITEM.it-codigo,
                                                  INPUT 4).
    
                            END.
    
                                
    
    
                            
    
    
                        END.
                    
                /*END.  /*IF AVAIL(saldo-estoq)*/ */
            END.
            ELSE DO:
                DEFINE VARIABLE l-ordem AS LOGICAL     NO-UNDO.
    
                ASSIGN l-ordem = FALSE.
    
                FOR EACH ordem-compra no-lock
                    WHERE ordem-compra.data-emissao >= DATE("01/01/2010")
                      AND ordem-compra.it-codigo     = ITEM.it-codigo,
                     EACH prazo-compra no-lock
                    WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem
                      AND prazo-compra.situacao     <> 4
                      AND prazo-compra.situacao     <> 6
                      AND prazo-compra.quantidade > 0:
    
                    ASSIGN l-ordem = TRUE.
    
                        FIND FIRST b-item 
                             WHERE b-item.it-codigo = ITEM.it-codigo NO-ERROR.
                        IF AVAIL(b-item) THEN DO:
                             IF tt-param.l-obsoletar = TRUE THEN DO:
                                 ASSIGN b-item.cod-obsoleto = 3.

                                 FOR EACH b-item-uni-estab 
                                   WHERE b-item-uni-estab.it-codigo = ITEM.it-codigo:
            
                                        ASSIGN b-item-uni-estab.cod-obsoleto = 3.
            
                                END.
    
                             END.
    
                             RUN pi-cria-saida(INPUT ITEM.it-codigo,
                                               INPUT 3).
                        END.
                            
    
    
                        
                    /*LEAVE.*/
                END.
    
    
                IF l-ordem = FALSE THEN DO:
    
                    FIND FIRST b-item 
                         WHERE b-item.it-codigo = ITEM.it-codigo NO-ERROR.
                    IF AVAIL(b-item) THEN DO:
                        IF tt-param.l-obsoletar = TRUE THEN DO:
                            ASSIGN b-item.cod-obsoleto = 4.
                                FOR EACH b-item-uni-estab 
                                   WHERE b-item-uni-estab.it-codigo = ITEM.it-codigo:
                
                                        ASSIGN b-item-uni-estab.cod-obsoleto = 4.
                
                                END.
                        END.
    
    
                        RUN pi-cria-saida(INPUT ITEM.it-codigo,
                                          INPUT 4).
    
                    END. /*IF AVAIL(b-item)*/
    
                        
    
    
                    
                
                    
    
                END. /*IF l-ordem = FALSE*/
            END.
        END.
    END.

END PROCEDURE.

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "ymcd4001.csv").

PUT "Empresa;"
    "ITEM;"
    "Obsoletar;" SKIP.


FOR EACH tt-saida:
    EXPORT DELIMITER ';' tt-saida.
END.
OUTPUT CLOSE.

MESSAGE "Foi criado um arquivo csv em: " + SESSION:TEMP-DIRECTORY + "ymcd4001.csv, com os itens que irÆo/foram ser obsoletados."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.




PROCEDURE pi-importa-excel:

     ASSIGN empresa    = ""
            it-codigo  = "".

        RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
        {utp/ut-liter.i Verificando Excel *}
    
        RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    
        CREATE "Excel.Application"  chExcelApplication.
        chWorkbook  = chExcelApplication:workBooks:ADD(tt-param.arq-entrada).
    
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
    
           ASSIGN empresa    = STRING(INT(chWorkSheet:Range('A' + STRING(i-linha-cont)):VALUE))            
                  it-codigo  = chWorkSheet:Range('B' + STRING(i-linha-cont)):VALUE.
            
           run pi-acompanhar in h-acomp (input "Empresa: " + UPPER(string(Empresa)) + " Item:" + upper(it-codigo)).
    
           CREATE tt-import.
           ASSIGN tt-import.empresa    = empresa   
                  tt-import.it-codigo  = it-codigo . 
    
        END.
   
    RUN pi-finalizar IN h-acomp.
    
    IF VALID-HANDLE(chexcelApplication) THEN
        RELEASE OBJECT chexcelApplication.
        
    IF VALID-HANDLE(chWorkbook) THEN
        RELEASE OBJECT chWorkbook.
    
    IF VALID-HANDLE(chWorkSheet) THEN
        RELEASE OBJECT chWorksheet.

END PROCEDURE.

PROCEDURE pi-cria-saida:
    
    DEFINE INPUT PARAMETER it-codigo LIKE ITEM.it-codigo.
    DEFINE INPUT PARAMETER cod-obsoleto AS INT.


    CREATE tt-saida.
    ASSIGN tt-saida.empresa      = i-ep-codigo-usuario
           tt-saida.it-codigo    = it-codigo /*ITEM.it-codigo*/
           tt-saida.cod-obsoleto = cod-obsoleto.

END PROCEDURE.

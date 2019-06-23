/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i RE1001-EPC 2.00.00.000}  /*** 010010 ***/
/***********************************************************************************
**  Programa..: UPC-RE1001.P
**  Finalidade: Bot∆o Gerar por Nota
**  Versío....: 2.00.000 - 
***********************************************************************************/
DEF INPUT PARAM p-ind-event   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object  AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame   AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table   AS ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-upc-re1001                  AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-frame1-re1001              AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-gera-por-item-re1001    AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-gera-por-nota-re1001    AS HANDLE  NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-emitente-re1001        AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-serie-re1001           AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nat-operacao-re1001    AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nro-docto-re1001       AS HANDLE  NO-UNDO.

DEF NEW GLOBAL SHARED VAR P-emitente-re1001       LIKE DOCUM-EST.COD-EMITENTE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR P-serie-re1001          LIKE DOCUM-EST.serie         NO-UNDO.
DEF NEW GLOBAL SHARED VAR P-nat-operacao-re1001   LIKE DOCUM-EST.nat-operacao  NO-UNDO.
DEF NEW GLOBAL SHARED VAR P-nro-docto-re1001      LIKE DOCUM-EST.nro-docto     NO-UNDO.


DEF VAR wgh-grupo     AS WIDGET-HANDLE NO-UNDO.
DEF VAR wgh-child     AS WIDGET-HANDLE NO-UNDO.
DEF VAR wgh-objeto    AS WIDGET-HANDLE NO-UNDO.


if  p-ind-event   = "AFTER-INITIALIZE" and
    p-ind-object  = "CONTAINER"  then do:

    RUN epc/re1001-epc.p PERSISTENT SET h-upc-re1001 (INPUT "",            
                                                      INPUT "",            
                                                      INPUT p-wgh-object,  
                                                      INPUT p-wgh-frame,   
                                                      INPUT "",            
                                                      INPUT p-row-table).

    ASSIGN wgh-grupo = p-wgh-frame:FIRST-CHILD.

    DO  WHILE VALID-HANDLE(wgh-grupo):
        ASSIGN wgh-child = wgh-grupo:FIRST-CHILD.

        DO  WHILE VALID-HANDLE(wgh-child):
            ASSIGN wgh-objeto = wgh-child:HANDLE.

             CASE wgh-child:TYPE:
                WHEN "FRAME" THEN DO: 
                    IF  wgh-child:NAME = "fPage1" THEN 
                        ASSIGN wh-frame1-re1001 = wgh-child:HANDLE.
                END.
                WHEN "fill-in" THEN DO: 
                    IF  wgh-child:NAME = "cod-emitente" THEN 
                        ASSIGN wh-emitente-re1001 = wgh-child:HANDLE.
                    IF  wgh-child:NAME = "nro-docto" THEN 
                        ASSIGN wh-nro-docto-re1001 = wgh-child:HANDLE.
                    IF  wgh-child:NAME = "serie-docto" THEN 
                        ASSIGN wh-serie-re1001 = wgh-child:HANDLE.
                    IF  wgh-child:NAME = "nat-operacao" THEN 
                        ASSIGN wh-nat-operacao-re1001 = wgh-child:HANDLE.
                END.
            END.
            ASSIGN wgh-child = wgh-child:NEXT-SIBLING NO-ERROR.
        END.
        LEAVE.
    END.

    
    ASSIGN wgh-grupo = wh-frame1-re1001:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wgh-grupo):
        ASSIGN wgh-child = wgh-grupo:FIRST-CHILD. 

        DO  WHILE VALID-HANDLE(wgh-child):
            ASSIGN wgh-objeto = wgh-child:HANDLE.
             CASE wgh-child:TYPE:
                WHEN "button" THEN DO:
                    IF  wgh-child:NAME = "btGeraPorItem" THEN 
                        ASSIGN wh-bt-gera-por-item-re1001 = wgh-child:HANDLE. /*handle do campo numero ordem*/
                END.
            END.
            ASSIGN wgh-child = wgh-child:NEXT-SIBLING NO-ERROR.
        END.
        LEAVE.
    END.

    CREATE BUTTON wh-bt-gera-por-nota-re1001 
    ASSIGN FRAME     = wh-bt-gera-por-item-re1001:FRAME
           WIDTH     = wh-bt-gera-por-item-re1001:WIDTH
           HEIGHT    = wh-bt-gera-por-item-re1001:HEIGHT
           LABEL     = "Gerar por Nota"
           ROW       = wh-bt-gera-por-item-re1001:ROW
           COL       = wh-bt-gera-por-item-re1001:COL - (wh-bt-gera-por-item-re1001:WIDTH) - 15
           FONT      = 1
           SENSITIVE = NO
           VISIBLE   = YES
           TRIGGERS:
               ON CHOOSE persistent run pi-gerar-por-nota in h-upc-re1001.
           END TRIGGERS.

    IF VALID-HANDLE(wh-bt-gera-por-nota-re1001) AND
       VALID-HANDLE(wh-bt-gera-por-item-re1001) THEN DO:
       ASSIGN wh-bt-gera-por-nota-re1001:SENSITIVE = wh-bt-gera-por-item-re1001:SENSITIVE.
       wh-bt-gera-por-nota-re1001:MOVE-BEFORE-TAB-ITEM(wh-bt-gera-por-item-re1001:handle). 
    END.

     IF VALID-HANDLE(wh-emitente-re1001)   AND 
        VALID-HANDLE(wh-serie-re1001)        AND
        VALID-HANDLE(wh-nat-operacao-re1001) AND
        VALID-HANDLE(wh-nro-docto-re1001) THEN
    ASSIGN P-emitente-re1001      = int(wh-emitente-re1001:SCREEN-VALUE)    
           P-serie-re1001         = wh-serie-re1001:SCREEN-VALUE       
           P-nat-operacao-re1001  = wh-nat-operacao-re1001:SCREEN-VALUE
           P-nro-docto-re1001     = wh-nro-docto-re1001:SCREEN-VALUE.

END.   

if  p-ind-event   = "AFTER-DISPLAY" and
    p-ind-object  = "CONTAINER"  then do:

    IF VALID-HANDLE(wh-bt-gera-por-nota-re1001) AND
       VALID-HANDLE(wh-bt-gera-por-item-re1001) THEN
       ASSIGN wh-bt-gera-por-nota-re1001:SENSITIVE = wh-bt-gera-por-item-re1001:SENSITIVE.

     IF VALID-HANDLE(wh-emitente-re1001)   AND 
        VALID-HANDLE(wh-serie-re1001)        AND
        VALID-HANDLE(wh-nat-operacao-re1001) AND
        VALID-HANDLE(wh-nro-docto-re1001) THEN
    ASSIGN P-emitente-re1001      = int(wh-emitente-re1001:SCREEN-VALUE)    
           P-serie-re1001         = wh-serie-re1001:SCREEN-VALUE       
           P-nat-operacao-re1001  = wh-nat-operacao-re1001:SCREEN-VALUE
           P-nro-docto-re1001     = wh-nro-docto-re1001:SCREEN-VALUE.

END.
     
PROCEDURE pi-gerar-por-nota:

    RUN epc/re1001j-epc.w.
    RUN openQueriesSon IN p-wgh-object.

END PROCEDURE.

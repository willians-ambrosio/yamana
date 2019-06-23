 /*-----------------------------------------------------------------------
    File        : esre1001a-u01
    Purpose     : Inluir FILL IN de tipo de documento
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esre1001a-u01.p 2.06.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}       

/* ###########           Parametros obrigatorios para UPC.        ########## */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

/* ###########           Definicao das Variavel Globais.         ########## */
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-btok            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-btok-f          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-btsave            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-btsave-f          AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-nat-operacao    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-cod-emitente    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-nro-docto       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-serie-docto     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-tipo-docto      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esre1001a-cod-tax         AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE i-esre1001a-cod-emitente    AS INTE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-nro-docto       AS CHAR FORMAT "x(8)" NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE i-esre1001a-nro-docto       AS INT FORMAT  "99999999" NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-serie-docto     AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-nat-operacao    AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-tipo-docto      AS CHAR FORMAT "x(5)"  NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-cod-tax         AS CHAR FORMAT "x(5)"  NO-UNDO.


DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-nro-docto-2       AS CHAR FORMAT "x(7)" NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE i-esre1001a-nro-docto-2       AS INT FORMAT  "9999999" NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-cod-estabel    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE c-esre1001a-dt-emissao     AS WIDGET-HANDLE NO-UNDO.

DEF VAR upc-cgc         AS CHAR NO-UNDO.
DEF VAR c-tipo-tax AS CHAR NO-UNDO.
DEF VAR i-qt-tipo    AS INT NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.

/* ###########           Mapeador de Endere¯os de Objeos         ########## */


/*##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####*/
/*      MESSAGE "p-ind-event..:" p-ind-event                  SKIP  */
/*              "p-ind-object.:" p-ind-object                 SKIP  */
/*              "p-cod-table..:" STRING(p-cod-table)          SKIP  */
/*              "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP  */
/*              "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP  */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */
/* /* ????????????????????????????????????????????????????????????????*/ */



IF  p-ind-object = "CONTAINER"
AND p-ind-event = "BEFORE-INITIALIZE" THEN
DO:

    ASSIGN c-handle-obj = fc-handle-obj("cod-estabel,dt-emissao,btok,btsave",p-wgh-frame).
    ASSIGN c-esre1001a-cod-estabel     = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           c-esre1001a-dt-emissao      = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
           wh-esre1001a-btok           = WIDGET-HANDLE(ENTRY(3,c-handle-obj))         
           wh-esre1001a-btsave         = WIDGET-HANDLE(ENTRY(4,c-handle-obj)).
           wh-esre1001a-btok-f         = fc-falso(wh-esre1001a-btok, wh-esre1001a-btok  :FRAME, ""). 
           wh-esre1001a-btok-f:LOAD-IMAGE-UP(wh-esre1001a-btok:IMAGE-UP)    .                            
           wh-esre1001a-btok-f:MOVE-TO-TOP()  .                                                    
           wh-esre1001a-btok:VISIBLE   = NO   .                                            
           wh-esre1001a-btsave-f       = fc-falso(wh-esre1001a-btsave, wh-esre1001a-btsave  :FRAME, ""). 
           wh-esre1001a-btsave-f:LOAD-IMAGE-UP(wh-esre1001a-btsave:IMAGE-UP).                          
           wh-esre1001a-btsave-f:MOVE-TO-TOP().                                                   
           wh-esre1001a-btsave:VISIBLE = NO   .                                           
    
           ON 'CHOOSE':U OF wh-esre1001a-btok-f PERSISTENT RUN upc\esre1001a-u04.p
               ("choose",
                                          "Botao Ok",
                                           p-wgh-object,
                                           p-wgh-frame,
                                           p-cod-table,
                                           p-row-table).
    
    
           ON 'CHOOSE':U OF wh-esre1001a-btsave-f PERSISTENT RUN upc\esre1001a-u04.p
               ("choose",
                                          "Botao Save",
                                           p-wgh-object,
                                           p-wgh-frame,
                                           p-cod-table,
                                           p-row-table).
           
        
END.


IF p-ind-event = "choose" AND
   p-ind-object = "Botao Ok"  THEN 

DO:


    ASSIGN c-handle-obj = fc-handle-obj("serie-docto",p-wgh-frame).
    ASSIGN wh-esre1001a-serie-docto   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) .


    IF   wh-esre1001a-tipo-docto:SENSITIVE = YES 
     AND wh-esre1001a-tipo-docto:SCREEN-VALUE = ? THEN DO:

        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Tipo de Nota Em Branco~~" +
                                 "Essa natureza de opera‡Æo gera DES, favor informar um tipo de Documento.").
        
        RETURN NO-APPLY.

    END.
    ELSE DO:

        ASSIGN c-esre1001a-tipo-docto = wh-esre1001a-tipo-docto:SCREEN-VALUE.


        IF(c-esre1001a-tipo-docto <> "CF"    AND
           c-esre1001a-tipo-docto <> "NFFS"  AND
           c-esre1001a-tipo-docto <> "R"     AND
           c-esre1001a-tipo-docto <> "RPA"   AND
           c-esre1001a-tipo-docto <> "EXTR"  AND
           c-esre1001a-tipo-docto <> "FAT"   AND
           c-esre1001a-tipo-docto <> "FORA"  AND
           c-esre1001a-tipo-docto <> "OUTRO" AND
           c-esre1001a-tipo-docto <> "NADA"  AND
           c-esre1001a-tipo-docto <> ?) AND
           wh-esre1001a-serie-docto:SCREEN-VALUE = ""  THEN DO:


            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 17006,
                               INPUT "Serie Em Branco~~" +
                                     "Esse tipo de documento necessita de serie, favor informar uma Serie.").

            RETURN NO-APPLY.

        END.

    END.


    APPLY "choose" TO wh-esre1001a-btok.
 
END.


IF p-ind-event = "choose" AND
   p-ind-object = "Botao Save"  THEN 

DO:

    ASSIGN c-handle-obj = fc-handle-obj("serie-docto",p-wgh-frame).
    ASSIGN wh-esre1001a-serie-docto   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) .


    IF   wh-esre1001a-tipo-docto:SENSITIVE = YES 
     AND wh-esre1001a-tipo-docto:SCREEN-VALUE = ? THEN DO:

        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Tipo de Nota Em Branco~~" +
                                 "Essa natureza de opera‡Æo gera DES, favor informar um tipo de Documento.").
        
        RETURN NO-APPLY.

    END.
    ELSE DO:

        ASSIGN c-esre1001a-tipo-docto = wh-esre1001a-tipo-docto:SCREEN-VALUE.

    END.

    IF(c-esre1001a-tipo-docto <> "CF"    AND
       c-esre1001a-tipo-docto <> "NFFS"  AND
       c-esre1001a-tipo-docto <> "R"     AND
       c-esre1001a-tipo-docto <> "RPA"   AND
       c-esre1001a-tipo-docto <> "EXTR"  AND
       c-esre1001a-tipo-docto <> "FAT"   AND
       c-esre1001a-tipo-docto <> "FORA"  AND
       c-esre1001a-tipo-docto <> "OUTRO" AND
       c-esre1001a-tipo-docto <> "NADA") AND
       wh-esre1001a-serie-docto:SCREEN-VALUE = ""  THEN DO:


        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Serie Em Branco~~" +
                                 "Esse tipo de documento necessita de serie, favor informar uma Serie.").
    
        RETURN NO-APPLY.

    END.

    APPLY "choose" TO wh-esre1001a-btsave.
 
END.




IF  p-ind-event = "BEFORE-SAVE-FIELDS"
AND p-ind-object = "CONTAINER" THEN
DO:

    ASSIGN c-handle-obj = fc-handle-obj("nat-operacao,cod-emitente,serie-docto,i-nro-docto",p-wgh-frame).
    ASSIGN wh-esre1001a-nat-operacao   = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-esre1001a-cod-emitente   = WIDGET-HANDLE(ENTRY(2,c-handle-obj))  
           wh-esre1001a-serie-docto    = WIDGET-HANDLE(ENTRY(3,c-handle-obj))
           wh-esre1001a-nro-docto      = WIDGET-HANDLE(ENTRY(4,c-handle-obj)) .

    ASSIGN i-esre1001a-cod-emitente  = int(wh-esre1001a-cod-emitente:SCREEN-VALUE)
           c-esre1001a-nro-docto     = wh-esre1001a-nro-docto:SCREEN-VALUE
           c-esre1001a-serie-docto   = wh-esre1001a-serie-docto:SCREEN-VALUE  
           c-esre1001a-nat-operacao  = wh-esre1001a-nat-operacao:SCREEN-VALUE  .  
     
    ASSIGN i-esre1001a-nro-docto     = int(c-esre1001a-nro-docto)
           c-esre1001a-nro-docto     = STRING(i-esre1001a-nro-docto,"99999999").


    ASSIGN i-esre1001a-nro-docto-2     = int(c-esre1001a-nro-docto)
           c-esre1001a-nro-docto-2     = STRING(i-esre1001a-nro-docto,"9999999").






END.


IF p-ind-event  = "after-assign" AND 
   p-ind-object = "container" THEN DO:

    IF wh-esre1001a-tipo-docto:SENSITIVE = YES THEN DO:
    
        FIND FIRST es-gera-des EXCLUSIVE-LOCK
             WHERE es-gera-des.cod-emitente = i-esre1001a-cod-emitente
               AND es-gera-des.nro-docto    = c-esre1001a-nro-docto
               AND es-gera-des.serie        = c-esre1001a-serie-docto  
               AND es-gera-des.nat-operacao = c-esre1001a-nat-operacao  NO-ERROR.
        
        IF NOT AVAIL es-gera-des THEN DO:

            FIND FIRST estabelec NO-LOCK 
                 WHERE estabelec.cod-estabel = c-esre1001a-cod-estabel:SCREEN-VALUE NO-ERROR.

            IF NOT AVAIL estabelec THEN NEXT.

            FIND FIRST emitente NO-LOCK 
                 WHERE emitente.cod-emitente = i-esre1001a-cod-emitente NO-ERROR. 
        
            IF NOT AVAIL emitente THEN NEXT.

            CREATE es-gera-des.
            ASSIGN es-gera-des.cgc            = estabelec.cgc
                   es-gera-des.tipo-docto     = string(c-esre1001a-tipo-docto)
                   es-gera-des.serie          = c-esre1001a-serie-docto
                   es-gera-des.nro-docto      = c-esre1001a-nro-docto
                   es-gera-des.dt-trans       = date(c-esre1001a-dt-emissao:SCREEN-VALUE)
                   es-gera-des.cgc-fornec     = emitente.cgc
                   es-gera-des.cod-emitente   = emitente.cod-emitente
                   es-gera-des.cod-estabel    = c-esre1001a-cod-estabel:SCREEN-VALUE 
                   es-gera-des.nat-operacao   = c-esre1001a-nat-operacao
                   es-gera-des.cidade         = emitente.cidade.

        END.
        ELSE DO:

            FIND FIRST estabelec NO-LOCK 
                 WHERE estabelec.cod-estabel = c-esre1001a-cod-estabel:SCREEN-VALUE NO-ERROR.

            IF NOT AVAIL estabelec THEN NEXT.

            FIND FIRST emitente NO-LOCK 
                 WHERE emitente.cod-emitente = i-esre1001a-cod-emitente NO-ERROR. 
        
            IF NOT AVAIL emitente THEN NEXT.

            ASSIGN es-gera-des.cgc            = estabelec.cgc
                   es-gera-des.tipo-docto     = string(c-esre1001a-tipo-docto)
                   es-gera-des.serie          = c-esre1001a-serie-docto
                   es-gera-des.nro-docto      = c-esre1001a-nro-docto
                   es-gera-des.dt-trans       = date(c-esre1001a-dt-emissao:SCREEN-VALUE)
                   es-gera-des.cgc-fornec     = emitente.cgc
                   es-gera-des.cod-emitente   = emitente.cod-emitente
                   es-gera-des.cod-estabel    = c-esre1001a-cod-estabel:SCREEN-VALUE 
                   es-gera-des.nat-operacao   = c-esre1001a-nat-operacao.
        END.    
    END.    
END.
                          

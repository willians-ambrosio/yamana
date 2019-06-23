 /*-----------------------------------------------------------------------
    File        : esap0501ir-u03
    Purpose     : Inluir Botoes Falsos para tabela Gera DES
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esap0501ir-u03.p 2.06.00.000}
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

DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-bt-ok       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-bt-ok-f     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-bt-save     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-bt-save-f   AS WIDGET-HANDLE NO-UNDO.


DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-cod-fornec   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-c-nr-docto   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-cod-estabel  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-cod-esp      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-parcela      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-serie        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-da-vencimen  AS WIDGET-HANDLE NO-UNDO.


DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-cod-esp   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-tot-valor-base  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501j-cod-tax         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-esap0501ir-tipo-docto     AS WIDGET-HANDLE NO-UNDO.

/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */


DEF VARIABLE c-esap0501ir-c-nr-docto       AS CHAR FORMAT "x(8)"     NO-UNDO.
DEF VARIABLE i-esap0501ir-c-nr-docto       AS INT FORMAT  "99999999" NO-UNDO.

DEFINE VARIABLE c-esap0501ir-tipo-docto  AS CHAR NO-UNDO.

DEF VARIABLE c-esap0501j-cod-tax           AS CHAR FORMAT "x(5)" NO-UNDO.
DEF VARIABLE i-esap0501j-cod-tax           AS INT FORMAT  "99999" NO-UNDO.

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.
DEFINE VARIABLE c-tipo-tax   AS CHAR NO-UNDO.
DEFINE VARIABLE i-qt-tipo    AS INT  NO-UNDO.
/* ###########           Mapeador de Endere¯os de Objeos         ########## */

/*##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####*/
/*      MESSAGE "p-ind-event..:" p-ind-event                  SKIP  */
/*              "p-ind-object.:" p-ind-object                 SKIP  */
/*              "p-cod-table..:" STRING(p-cod-table)          SKIP  */
/*              "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP  */
/*              "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP  */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */
/* ????????????????????????????????????????????????????????????????*/
/*8377*/


IF  p-ind-object = "CONTAINER"
AND p-ind-event = "BEFORE-INITIALIZE" THEN
DO:

    ASSIGN c-handle-obj = fc-handle-obj("bt-ok,bt-salvar,de-vl-tributavel,i-cod-imposto,i-cod-fornec,c-nr-docto,c-serie,c-parcela,c-cod-estabel,da-vencimen",p-wgh-frame).
    ASSIGN wh-esap0501ir-bt-ok           = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-esap0501ir-bt-save         = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
           wh-esap0501j-tot-valor-base   = WIDGET-HANDLE(ENTRY(3,c-handle-obj))    
           wh-esap0501j-cod-tax          = WIDGET-HANDLE(ENTRY(4,c-handle-obj))
           wh-esap0501ir-cod-fornec      = WIDGET-HANDLE(ENTRY(5,c-handle-obj))
           wh-esap0501ir-c-nr-docto      = WIDGET-HANDLE(ENTRY(6,c-handle-obj)) 
           wh-esap0501ir-serie           = WIDGET-HANDLE(ENTRY(7,c-handle-obj))
           wh-esap0501ir-parcela         = WIDGET-HANDLE(ENTRY(8,c-handle-obj))   
           wh-esap0501ir-cod-estabel     = WIDGET-HANDLE(ENTRY(9,c-handle-obj)) 
           wh-esap0501ir-da-vencimen     = WIDGET-HANDLE(ENTRY(10,c-handle-obj))  .
           wh-esap0501ir-bt-ok-f         = fc-falso(wh-esap0501ir-bt-ok, wh-esap0501ir-bt-ok  :FRAME, ""). 
           wh-esap0501ir-bt-ok-f:LOAD-IMAGE-UP(wh-esap0501ir-bt-ok:IMAGE-UP)    .                            
           wh-esap0501ir-bt-ok-f:MOVE-TO-TOP()  .                                                    
           wh-esap0501ir-bt-ok:VISIBLE   = NO   .                                            
           wh-esap0501ir-bt-save-f       = fc-falso(wh-esap0501ir-bt-save, wh-esap0501ir-bt-save  :FRAME, ""). 
           wh-esap0501ir-bt-save-f:LOAD-IMAGE-UP(wh-esap0501ir-bt-save:IMAGE-UP).                          
           wh-esap0501ir-bt-save-f:MOVE-TO-TOP().                                                   
           wh-esap0501ir-bt-save:VISIBLE = NO   .                                           
          
           ON 'CHOOSE':U OF wh-esap0501ir-bt-ok-f PERSISTENT RUN upc\esap0501ir-u03.p
               ("choose",
                                          "Botao Ok",
                                           p-wgh-object,
                                           p-wgh-frame,
                                           p-cod-table,
                                           p-row-table).


           ON 'CHOOSE':U OF wh-esap0501ir-bt-save-f PERSISTENT RUN upc\esap0501ir-u03.p
               ("choose",
                                          "Botao Save",
                                           p-wgh-object,
                                           p-wgh-frame,
                                           p-cod-table,
                                           p-row-table).



END.
  
   

IF p-ind-event  = "CHOOSE" AND 
   p-ind-object = "Botao Save" THEN DO:
        
    IF   wh-esap0501ir-tipo-docto:SENSITIVE = YES 
     AND wh-esap0501ir-tipo-docto:SCREEN-VALUE = ? THEN DO:

        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Tipo de Nota Em Branco~~" +
                                 "Esse tipo de Imposto gera DES, favor informar um tipo de Documento.").
        
        RETURN NO-APPLY.

    END.
    
    ASSIGN c-esap0501ir-tipo-docto = string(wh-esap0501ir-tipo-docto:SCREEN-VALUE).
   

    ASSIGN c-esap0501j-cod-tax     = wh-esap0501j-cod-tax:SCREEN-VALUE
           i-esap0501j-cod-tax     = int(c-esap0501j-cod-tax)
           c-esap0501j-cod-tax     = STRING(i-esap0501j-cod-tax,"99999").

    ASSIGN c-esap0501ir-c-nr-docto     = wh-esap0501ir-c-nr-docto:SCREEN-VALUE  
           i-esap0501ir-c-nr-docto     = int(c-esap0501ir-c-nr-docto)
           c-esap0501ir-c-nr-docto     = STRING(i-esap0501ir-c-nr-docto,"99999999").
    
    APPLY "choose" TO wh-esap0501ir-bt-save.

    IF wh-esap0501ir-tipo-docto:SENSITIVE = YES THEN DO:

        FIND FIRST es-gera-des 
             WHERE es-gera-des.cod-emitente = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE)
               AND es-gera-des.nro-docto    = c-esap0501ir-c-nr-docto        
               AND es-gera-des.serie        = wh-esap0501ir-serie:SCREEN-VALUE   
               AND es-gera-des.parcela      = wh-esap0501ir-parcela:SCREEN-VALUE 
               AND es-gera-des.cod-esp      = wh-esap0501j-cod-esp:SCREEN-VALUE   NO-ERROR.
    
        IF NOT AVAIL es-gera-des THEN DO:

            FIND FIRST lin-i-ap NO-LOCK
                 WHERE lin-i-ap.cod-estabel = wh-esap0501ir-cod-estabel:SCREEN-VALUE
                   AND lin-i-ap.cod-fornec  = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE)
                   AND lin-i-ap.cod-esp     = wh-esap0501j-cod-esp:SCREEN-VALUE
                   AND lin-i-ap.serie       = wh-esap0501ir-serie:SCREEN-VALUE
                   AND lin-i-ap.nr-docto    = wh-esap0501ir-c-nr-docto:SCREEN-VALUE
                   AND lin-i-ap.parcela     = wh-esap0501ir-parcela:SCREEN-VALUE NO-ERROR.

            FIND FIRST estabelec NO-LOCK 
                 WHERE estabelec.cod-estabel = wh-esap0501ir-cod-estabel:SCREEN-VALUE NO-ERROR.

            IF NOT AVAIL estabelec THEN NEXT.

            FIND FIRST emitente NO-LOCK 
                 WHERE emitente.cod-emitente = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE) NO-ERROR. 

            IF NOT AVAIL emitente THEN NEXT.
            
            CREATE es-gera-des.

            ASSIGN es-gera-des.cgc          = estabelec.cgc
                   es-gera-des.cod-emitente = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE)
                   es-gera-des.cgc-fornec   = emitente.cgc
                   es-gera-des.nro-docto    = c-esap0501ir-c-nr-docto
                   es-gera-des.cod-estabel  = wh-esap0501ir-cod-estabel:SCREEN-VALUE
                   es-gera-des.serie        = wh-esap0501ir-serie:SCREEN-VALUE
                   es-gera-des.tot-valor    = lin-i-ap.vl-orig-me
                   es-gera-des.dt-trans     = date(wh-esap0501ir-da-vencimen:SCREEN-VALUE)
                   es-gera-des.cod-esp      = wh-esap0501j-cod-esp:SCREEN-VALUE
                   es-gera-des.parcela      = wh-esap0501ir-parcela:SCREEN-VALUE 
                   es-gera-de.cidade        = emitente.cidade.

            ASSIGN es-gera-des.tipo-docto     = c-esap0501ir-tipo-docto
                   es-gera-des.cod-tax        = c-esap0501j-cod-tax
                   es-gera-des.tot-valor-base = dec(wh-esap0501j-tot-valor-base:SCREEN-VALUE).

        END.
        ELSE DO:


            ASSIGN es-gera-des.tipo-docto     = c-esap0501ir-tipo-docto
                   es-gera-des.cod-tax        = c-esap0501j-cod-tax
                   es-gera-des.tot-valor-base = dec(wh-esap0501j-tot-valor-base:SCREEN-VALUE).
        END.
    END.
       
END.


IF p-ind-event  = "CHOOSE" AND 
   p-ind-object = "Botao Ok" THEN DO:

    IF   wh-esap0501ir-tipo-docto:SENSITIVE = YES 
     AND wh-esap0501ir-tipo-docto:SCREEN-VALUE = ? THEN DO:

        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Tipo de Nota Em Branco~~" +
                                 "Esse tipo de Imposto gera DES, favor informar um tipo de Documento.").
        
        RETURN NO-APPLY.

    END.
    
    ASSIGN c-esap0501ir-tipo-docto = string(wh-esap0501ir-tipo-docto:SCREEN-VALUE).
   

    ASSIGN c-esap0501j-cod-tax     = wh-esap0501j-cod-tax:SCREEN-VALUE
           i-esap0501j-cod-tax     = int(c-esap0501j-cod-tax)
           c-esap0501j-cod-tax     = STRING(i-esap0501j-cod-tax,"99999").

    ASSIGN c-esap0501ir-c-nr-docto     = wh-esap0501ir-c-nr-docto:SCREEN-VALUE  
           i-esap0501ir-c-nr-docto     = int(c-esap0501ir-c-nr-docto)
           c-esap0501ir-c-nr-docto     = STRING(i-esap0501ir-c-nr-docto,"99999999").
    
    APPLY "choose" TO wh-esap0501ir-bt-ok.

    IF wh-esap0501ir-tipo-docto:SENSITIVE = YES THEN DO:

        FIND FIRST es-gera-des 
             WHERE es-gera-des.cod-emitente = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE)
               AND es-gera-des.nro-docto    = c-esap0501ir-c-nr-docto        
               AND es-gera-des.serie        = wh-esap0501ir-serie:SCREEN-VALUE   
               AND es-gera-des.parcela      = wh-esap0501ir-parcela:SCREEN-VALUE 
               AND es-gera-des.cod-esp      = wh-esap0501j-cod-esp:SCREEN-VALUE   NO-ERROR.
    
        IF NOT AVAIL es-gera-des THEN DO:

            FIND FIRST lin-i-ap NO-LOCK
                 WHERE lin-i-ap.cod-estabel = wh-esap0501ir-cod-estabel:SCREEN-VALUE
                   AND lin-i-ap.cod-fornec  = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE)
                   AND lin-i-ap.cod-esp     = wh-esap0501j-cod-esp:SCREEN-VALUE
                   AND lin-i-ap.serie       = wh-esap0501ir-serie:SCREEN-VALUE
                   AND lin-i-ap.nr-docto    = wh-esap0501ir-c-nr-docto:SCREEN-VALUE
                   AND lin-i-ap.parcela     = wh-esap0501ir-parcela:SCREEN-VALUE NO-ERROR.

            FIND FIRST estabelec NO-LOCK 
                 WHERE estabelec.cod-estabel = wh-esap0501ir-cod-estabel:SCREEN-VALUE NO-ERROR.

            IF NOT AVAIL estabelec THEN NEXT.

            FIND FIRST emitente NO-LOCK 
                 WHERE emitente.cod-emitente = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE) NO-ERROR. 

            IF NOT AVAIL emitente THEN NEXT.
            
            CREATE es-gera-des.

            ASSIGN es-gera-des.cgc          = estabelec.cgc
                   es-gera-des.cod-emitente = int(wh-esap0501ir-cod-fornec:SCREEN-VALUE)
                   es-gera-des.cgc-fornec   = emitente.cgc
                   es-gera-des.nro-docto    = c-esap0501ir-c-nr-docto
                   es-gera-des.cod-estabel  = wh-esap0501ir-cod-estabel:SCREEN-VALUE
                   es-gera-des.serie        = wh-esap0501ir-serie:SCREEN-VALUE
                   es-gera-des.tot-valor    = lin-i-ap.vl-orig-me
                   es-gera-des.dt-trans     = date(wh-esap0501ir-da-vencimen:SCREEN-VALUE)
                   es-gera-des.cod-esp      = wh-esap0501j-cod-esp:SCREEN-VALUE
                   es-gera-des.parcela      = wh-esap0501ir-parcela:SCREEN-VALUE .

            ASSIGN es-gera-des.tipo-docto     = c-esap0501ir-tipo-docto
                   es-gera-des.cod-tax        = c-esap0501j-cod-tax
                   es-gera-des.tot-valor-base = dec(wh-esap0501j-tot-valor-base:SCREEN-VALUE).

        END.
        ELSE DO:


            ASSIGN es-gera-des.tipo-docto     = c-esap0501ir-tipo-docto
                   es-gera-des.cod-tax        = c-esap0501j-cod-tax
                   es-gera-des.tot-valor-base = dec(wh-esap0501j-tot-valor-base:SCREEN-VALUE).
        END.
    END.

END.

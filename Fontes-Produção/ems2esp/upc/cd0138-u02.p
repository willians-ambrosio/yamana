/* =========================================================================== 
   PROGRAMA    : CD0204-U05.p
   DATA        :  05/05/2015
   DESENVOLVIDO: Carlos Souza - DSC
   VERSAO      : 001
   OBJETIVO    : UPC no programa manut familias materiais. 
   =========================================================================== */
{include/i-prgvrs.i  cd0204-u05.P 2.06.00.000}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}


DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

DEF VAR h-objeto                 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-cod-obsoleto   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-fm-codigo      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-ge-codigo      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE c-handle-obj  AS CHARACTER      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-it-codigo     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-tipo-controle AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-depto         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-depto-txt     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-desc-depto    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-cd0204-upc02-i-ini          AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-138-cb-tipo-contr  AS WIDGET-HANDLE NO-UNDO.



DEFINE VARIABLE rw-ext-nf-e   AS ROWID          NO-UNDO.
DEFINE VARIABLE wh-pesquisa   AS WIDGET-HANDLE  NO-UNDO.
DEFINE VARIABLE l-implanta    AS LOGICAL        NO-UNDO.   
DEFINE VARIABLE c-tipo-tax    AS CHAR           NO-UNDO.

DEFINE VARIABLE i-qt-tipo     AS INT            NO-UNDO.
DEFINE VARIABLE c-cod-tax     AS CHAR           NO-UNDO.
DEFINE VARIABLE i-qt-tax      AS INT            NO-UNDO.
DEFINE VARIABLE i-itens       AS INTEGER        NO-UNDO.
DEFINE VARIABLE l-achou       AS LOGICAL     NO-UNDO.
define new global shared variable c-seg-usuario                 as char format 'x(12)' no-undo.

DEFINE NEW GLOBAL SHARED VARIABLE listagem AS CHAR NO-UNDO.


/*    RUN pi-msg. */

 run tela-upc(input p-wgh-frame,      
                 input p-ind-event,      
                 input p-wgh-object:type,
                 input p-wgh-object:name,
                 input no,             
                 output p-wgh-object).



IF VALID-HANDLE(wh-cd0138-cod-obsoleto) THEN DO:
   
   wh-cd0138-cod-obsoleto:SENSITIVE = NO.
   wh-cd0138-fm-codigo:SENSITIVE    = NO.
   wh-cd0138-ge-codigo:SENSITIVE    = NO.
   wh-cd0204-upc02-138-cb-tipo-contr:SENSITIVE    = NO.

END.


IF p-ind-event               = "BEFORE-INITIALIZE"   THEN  DO:
 

  c-handle-obj = fc-handle-obj("it-codigo",p-wgh-frame).
  wh-cd0204-upc02-it-codigo      = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  c-handle-obj = fc-handle-obj("cod-localiz",p-wgh-frame).
   wh-cd0204-upc02-tipo-controle  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  /* Carregando Deptos cadastrados   */
  i-itens = 0.
  FOR EACH es-depto no-lock:
      ASSIGN i-itens = i-itens + 1
             c-tipo-tax = c-tipo-tax + TRIM(string(es-depto.codigo,"999")) + " - " + TRIM(string(es-depto.descricao,"x(30)")) + "," + TRIM(STRING(i-itens,">9")) + ","  
             i-qt-tipo = i-qt-tipo + 1 .
  END.

  /* Criando campo de Departamento do Item */
  CREATE COMBO-BOX wh-cd0204-upc02-depto
  ASSIGN FRAME     = wh-cd0204-upc02-tipo-controle:FRAME
       DATA-TYPE    = "INTEGER"
       FORMAT       = ">9"
       WIDTH        = 30
       NAME         = "Depto"
       ROW          = wh-cd0204-upc02-tipo-controle:ROW + 1
       COL          = wh-cd0204-upc02-tipo-controle:col + wh-cd0204-upc02-tipo-controle:width + 11
       VISIBLE      = YES
       SENSITIVE    = NO
       INNER-LINES  = i-qt-tipo.
    ASSIGN c-tipo-tax = SUBSTRING(c-tipo-tax,1,LENGTH(c-tipo-tax) - 1) /* RETIRA A ULTIMA LINHA EM BRANCO */
           wh-cd0204-upc02-depto:LIST-ITEM-PAIRS  = c-tipo-tax.
    CREATE FILL-IN wh-cd0204-upc02-desc-depto
    ASSIGN
        NAME      = "desc-depto"
        FRAME     = wh-cd0204-upc02-tipo-controle:frame
        COL       = wh-cd0204-upc02-tipo-controle:col + wh-cd0204-upc02-tipo-controle:width + 9
        ROW       = wh-cd0204-upc02-tipo-controle:row
        FORMAT    = "x(30)"
        WIDTH     = 30
        HEIGHT    = 0.88
        VISIBLE   = NO
        SENSITIVE = YES
        TRIGGERS:
        END TRIGGERS.

    CREATE TEXT wh-cd0204-upc02-depto-txt
    ASSIGN
        FRAME        = wh-cd0204-upc02-depto:FRAME
        FORMAT       = "x(20)"
        WIDTH        = 5
        HEIGHT-CHARS = 0.88
        SCREEN-VALUE = "Depto:"
        ROW          = wh-cd0204-upc02-depto:ROW
        COL          = wh-cd0204-upc02-depto:COL - 5
        VISIBLE      = YES.
END.

IF VALID-HANDLE(wh-cd0204-upc02-depto-txt) THEN
  ASSIGN wh-cd0204-upc02-depto-txt:SCREEN-VALUE = "Depto:".


/* ---> NAVEGA€ÇO <--- */
if p-ind-event               = "AFTER-DISPLAY"        
             
   /*p-wgh-object:private-data = "invwr/v36in172.w"  */ then do:

    ASSIGN listagem = "".

    ASSIGN i-itens = 0.
    
    FOR EACH es-depto no-lock:
        ASSIGN i-itens = i-itens + 1
               listagem = listagem + TRIM(string(es-depto.codigo,"999")) + " - " + TRIM(string(es-depto.descricao,"x(30)")) + ",".
               
    END.

  find first item no-lock
    where rowid(item) = p-row-table no-error.
  if avail item then do:

    /* Carregando Deptos cadastrados   */
    ASSIGN i-cd0204-upc02-i-ini = i-cd0204-upc02-i-ini + 1.

    i-itens = 0.
    FOR EACH es-depto no-lock:

      ASSIGN i-itens = i-itens + 1
             c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " , " + string(es-depto.descricao,"x(30)") + ",".
             /** c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " - " + string(es-depto.descricao,"x(30)") + ",". **/
 
        find es-it-depto
            where es-it-depto.it-codigo = ITEM.it-codigo
              and es-it-depto.cod-depto = i-itens
              and es-it-depto.cod-depto <> 0 no-lock no-error.
        if avail es-it-depto then do:

            if valid-handle(wh-cd0204-upc02-desc-depto) then
                assign wh-cd0204-upc02-desc-depto:visible = no.

            if valid-handle(wh-cd0204-upc02-depto) THEN DO:
/*
                IF i-cd0204-upc02-i-ini > 1 THEN DO:
                    IF i-itens MOD 2 <> 0 THEN
                        ASSIGN i-itens = i-itens * 2.
                    ELSE
                        ASSIGN i-itens = i-itens * 2.

                    assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEM-PAIRS, ",").
                END.
                ELSE
                    
*/                  
                    assign wh-cd0204-upc02-depto:screen-value = STRING(i-itens) NO-ERROR .
                    /*assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEMS, ",").*/

            END.
                /** assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:list-items). **/
        end.
    end.
    if not can-find(first es-it-depto
                    where es-it-depto.it-codigo = item.it-codigo) then do:
        if valid-handle(wh-cd0204-upc02-desc-depto) then
            assign wh-cd0204-upc02-desc-depto:visible = NO.
    end.
    else do:
        find first es-it-depto
             where es-it-depto.it-codigo = item.it-codigo no-lock no-error.
        if avail es-it-depto and es-it-depto.cod-depto = 0 then do:
/*             if valid-handle(wh-cd0204-upc02-desc-depto) then     */
/*                 assign wh-cd0204-upc02-desc-depto:visible = yes. */
        end.
    end.
  end.
end.

if p-ind-event = "AFTER-ENABLE" then do:

    /* Carregando Deptos cadastrados   */
    find first item no-lock
      where rowid(item) = p-row-table no-error.
    if avail item then do:

      /* Carregando Deptos cadastrados   */
      assign i-itens = 0
             l-achou = no.
      FOR EACH es-depto no-lock:

        ASSIGN i-itens = i-itens + 1
               c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " , " + string(es-depto.descricao,"x(30)") + ",".
               /** c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " - " + string(es-depto.descricao,"x(30)") + ",". **/

          find es-it-depto
              where es-it-depto.it-codigo = ITEM.it-codigo
                and es-it-depto.cod-depto = i-itens
                and es-it-depto.cod-depto <> 0 no-lock no-error.
          if avail es-it-depto then do:
              assign l-achou = yes.

              if valid-handle(wh-cd0204-upc02-desc-depto) then
                  assign wh-cd0204-upc02-desc-depto:visible = no.

              if valid-handle(wh-cd0204-upc02-depto) THEN DO:

                  IF i-itens MOD 2 <> 0 THEN
                      ASSIGN i-itens = i-itens * 2.
                  ELSE
                      ASSIGN i-itens = i-itens * 2.

                  /* assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEM-PAIRS, ","). */
                  /*
                  MESSAGE "-------> "  es-it-depto.cod-depto
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  */    
                  assign wh-cd0204-upc02-depto:screen-value = STRING(es-it-depto.cod-depto) NO-ERROR /* STRING(i-itens) */ /*entry(i-itens,listagem)*/.

                  /*assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEMS, ",").*/

                  /** assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEMS). **/
              END.

          end.
      end.
      if l-achou = no  then do:
          if valid-handle(wh-cd0204-upc02-depto) then
              assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:LIST-ITEM-PAIRS) NO-ERROR.
      end.
    end.

    if valid-handle(wh-cd0204-upc02-desc-depto) then
        assign wh-cd0204-upc02-desc-depto:visible = NO.

    if valid-handle(wh-cd0204-upc02-depto) then
        assign wh-cd0204-upc02-depto:sensitive = yes.
end.

/* --->  ENABLE  <--- */

/* --->  ADD ITEM  <--- */
if p-ind-event               = "ADD"    
   then do:

    /* Carregando Deptos cadastrados   */
    i-itens = 0.
    FOR EACH es-depto no-lock:
      ASSIGN i-itens = i-itens + 1
             c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " , " + string(es-depto.descricao,"x(30)") + ","
             i-qt-tipo = i-qt-tipo + 1 .
             /** c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " - " + string(es-depto.descricao,"x(30)") + "," **/

    END.

    if valid-handle(wh-cd0204-upc02-depto) then
        assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:LIST-ITEM-PAIRS).
/*         assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:LIST-ITEM-PAIRS). */
        /** assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:list-items). **/
end.


/* --->  DISABLE  <--- */
if p-ind-event               = "AFTER-DISABLE"    then do:

    assign wh-cd0204-upc02-depto:sensitive = no.

    if valid-handle(wh-cd0204-upc02-desc-depto) then
      assign wh-cd0204-upc02-desc-depto:visible = no.

end.


/* ---> GRAVANDO  <--- */
if p-ind-event  = "after-assign"  then do:

    find first item no-lock
         where rowid(item) = p-row-table no-error.
    if avail item then do:

        find first usuar_mestre where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.

        /* Gravando o codigo do item na tabela de deptos */
        find first es-it-depto
             where es-it-depto.it-codigo = item.it-codigo exclusive-lock no-error.
        if not avail es-it-depto then do:

               create es-it-depto.
               assign es-it-depto.it-codigo = item.it-codigo
                      es-it-depto.cod-depto = int(substring(wh-cd0204-upc02-depto:screen-value,1,3))
                      es-it-depto.data      = today
                      es-it-depto.hora      = string(time,"hh:mm:ss")
                      es-it-depto.origem    = "CD0204"
                      es-it-depto.usuario   = if avail usuar_mestre then usuar_mestre.cod_usuario else "".
        end.
        else do:
            assign es-it-depto.cod-depto    = int(substring(wh-cd0204-upc02-depto:screen-value,1,3))
                   es-it-depto.usuar-altera = if avail usuar_mestre then usuar_mestre.cod_usuario else ""
                   es-it-depto.data-altera  = today
                   es-it-depto.hr-altera    = string(time,"hh:mm:ss").
        end.
    end.
end.

/* ---> VALIDACAO <--- */
if p-ind-event               = "validate"         and
   p-ind-object              = "VIEWER"           and
   p-wgh-object:private-data = "invwr/v36in172.w" then do:

  find first es-depto no-lock
       where es-depto.codigo = int(substring(wh-cd0204-upc02-depto:screen-value,1,3)) no-error.
  if not avail es-depto then do:
    run utp/ut-msgs.p(input 'show',
                      input 2,
                      input "Departamento").
    return 'NOK':U.
  end.
end.

/* --->  DESTROY  <--- */
if p-ind-event               = "DESTROY"    then
    assign i-cd0204-upc02-i-ini = 0.






IF RETURN-VALUE = "NOK" THEN 
  RETURN "NOK":U.
                                                
RETURN RETURN-VALUE.



PROCEDURE tela-upc:
    DEFINE INPUT  PARAMETER  pWghFrame    AS WIDGET-HANDLE NO-UNDO.  
    DEFINE INPUT  PARAMETER  pIndEvent    AS CHARACTER     NO-UNDO.      
    DEFINE INPUT  PARAMETER  pObjType     AS CHARACTER     NO-UNDO.  
    DEFINE INPUT  PARAMETER  pObjName     AS CHARACTER     NO-UNDO.   
    DEFINE INPUT  PARAMETER  pApresMsg    AS LOGICAL       NO-UNDO.  
    DEFINE OUTPUT PARAMETER  phObj        AS HANDLE        NO-UNDO.  
    
    DEFINE VARIABLE wgh-obj AS WIDGET-HANDLE NO-UNDO.

    ASSIGN wgh-obj = pWghFrame:FIRST-CHILD.

    DO  WHILE VALID-HANDLE(wgh-obj):


      
       IF wgh-obj:NAME = "cb-cod-obsoleto" THEN wh-cd0138-cod-obsoleto = wgh-obj.
       IF wgh-obj:NAME = "fm-codigo"       THEN wh-cd0138-fm-codigo = wgh-obj.
       IF wgh-obj:NAME = "ge-codigo"       THEN wh-cd0138-ge-codigo = wgh-obj.
       IF wgh-obj:NAME = "cb-tipo-contr"   THEN wh-cd0204-upc02-138-cb-tipo-contr = wgh-obj. 
        
       
        IF wgh-obj:TYPE = pObjType AND
           wgh-obj:NAME = pObjName THEN DO:
            ASSIGN phObj = wgh-obj:HANDLE.
            LEAVE.
        END.
        IF wgh-obj:TYPE = "field-group" THEN
            ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
        ELSE
            if wgh-obj:TYPE = "frame" and
               wgh-obj:name = "fpage1" then 
                ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
            else
                ASSIGN wgh-obj = wgh-obj:NEXT-SIBLING.

    END.

END PROCEDURE. 

procedure pi-msg:
message "p-ind-event..:" p-ind-event                  skip
        "p-ind-object.:" p-ind-object                 skip
        "p-cod-table..:" STRING(p-cod-table)          skip
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip
        "p-wgh-frame..:" STRING(p-wgh-frame)          skip
        "p-row-table..:" string(p-row-table)          skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

















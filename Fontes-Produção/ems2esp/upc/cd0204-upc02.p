/*-----------------------------------------------------------------------------------
    PROGRAMA : cd0204-upc02.p
    OBJETIVO : Criacao do campo para indica‡Æo do departamento, vinculando-o ao item
               (Depto Descricao), no programa cd0204
    AUTOR    : LUIZ CRUZ
    DATA     : 12/mai/2009
    EMPRESA  : DSC CONSULTORIA
    DESCRICAO:
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
    AUTERA€ÇO: THIAGO COUTINHO
    DATA     : 28/MAR/2012
    EMPRESA  : CSX SOLUTION
    DESCRICAO: MIGRA€ÇO 2.06 - ALTERA€åES PARA LIST-ITEM-PAIRS.
-----------------------------------------------------------------------------------*/
/*************************************************************************************
                                      INCLUDES
*************************************************************************************/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS GLOBAIS
*************************************************************************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-it-codigo     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-tipo-controle AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-depto         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-depto-txt     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0204-upc02-desc-depto    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-cd0204-upc02-i-ini          AS INTEGER       NO-UNDO.

define new global shared variable c-seg-usuario                 as char format 'x(12)' no-undo.

/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/
DEFINE VARIABLE c-handle-obj  AS CHARACTER      NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID          NO-UNDO.
DEFINE VARIABLE wh-pesquisa   AS WIDGET-HANDLE  NO-UNDO.
DEFINE VARIABLE l-implanta    AS LOGICAL        NO-UNDO.
DEFINE VARIABLE c-tipo-tax    AS CHAR           NO-UNDO.
DEFINE VARIABLE i-qt-tipo     AS INT            NO-UNDO.
DEFINE VARIABLE c-cod-tax     AS CHAR           NO-UNDO.
DEFINE VARIABLE i-qt-tax      AS INT            NO-UNDO.
DEFINE VARIABLE i-itens       AS INTEGER        NO-UNDO.
DEFINE VARIABLE l-achou       AS LOGICAL     NO-UNDO.

/* RUN pi-msg. */

/* ---> INSTANCIA OBJETOS <--- */
IF p-ind-event               = "BEFORE-INITIALIZE"    AND
   p-ind-object              = "VIEWER"               AND
   p-wgh-object:PRIVATE-DATA = "invwr/v34in172.w"      THEN DO:

  c-handle-obj = fc-handle-obj("it-codigo",p-wgh-frame).
  wh-cd0204-upc02-it-codigo      = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
END.

IF p-ind-event               = "BEFORE-INITIALIZE"    AND
   p-ind-object              = "VIEWER"               AND
   p-wgh-object:PRIVATE-DATA = "invwr/v36in172.w"     THEN DO:

   c-handle-obj = fc-handle-obj("lote-economi",p-wgh-frame).
   wh-cd0204-upc02-tipo-controle  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

  /* Carregando Deptos cadastrados   */
  i-itens = 0.
  FOR EACH es-depto no-lock:
      ASSIGN i-itens = i-itens + 1
             c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " - " + string(es-depto.descricao,"x(30)") + ","
             i-qt-tipo = i-qt-tipo + 1 .
  END.

  /* Criando campo de Departamento do Item */
  CREATE COMBO-BOX wh-cd0204-upc02-depto
  ASSIGN FRAME     = wh-cd0204-upc02-tipo-controle:FRAME
       DATA-TYPE    = "CHARACTER"
       FORMAT       = "x(30)"
       WIDTH        = 30
       NAME         = "Depto"
       ROW          = wh-cd0204-upc02-tipo-controle:row
       COL          = wh-cd0204-upc02-tipo-controle:col + wh-cd0204-upc02-tipo-controle:width + 9
       VISIBLE      = YES
       SENSITIVE    = no
       INNER-LINES  = i-qt-tipo.

    ASSIGN c-tipo-tax = SUBSTRING(c-tipo-tax,1,LENGTH(c-tipo-tax) - 1) /* RETIRA A ULTIMA LINHA EM BRANCO */
           wh-cd0204-upc02-depto:LIST-ITEMS = c-tipo-tax.

    CREATE FILL-IN wh-cd0204-upc02-desc-depto
    ASSIGN
        NAME      = "desc-depto"
        FRAME     = wh-cd0204-upc02-tipo-controle:frame
        COL       = wh-cd0204-upc02-tipo-controle:col + wh-cd0204-upc02-tipo-controle:width + 6
        ROW       = wh-cd0204-upc02-tipo-controle:row
        FORMAT    = "x(30)"
        WIDTH     = 30
        HEIGHT    = 0.88
        VISIBLE   = no
        SENSITIVE = no
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
if p-ind-event               = "DISPLAY"          and
   p-ind-object              = "VIEWER"           and
   p-wgh-object:private-data = "invwr/v36in172.w" then do:

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
                    assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEMS, ",").

            END.
                /** assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:list-items). **/
        end.
    end.
    if not can-find(first es-it-depto
                    where es-it-depto.it-codigo = item.it-codigo) then do:
        if valid-handle(wh-cd0204-upc02-desc-depto) then
            assign wh-cd0204-upc02-desc-depto:visible = yes.
    end.
    else do:
        find first es-it-depto
             where es-it-depto.it-codigo = item.it-codigo no-lock no-error.
        if avail es-it-depto and es-it-depto.cod-depto = 0 then do:
            if valid-handle(wh-cd0204-upc02-desc-depto) then
                assign wh-cd0204-upc02-desc-depto:visible = yes.
        end.
    end.
  end.
end.

/* --->  ENABLE  <--- */
if p-ind-event               = "AFTER-ENABLE"     and
   p-ind-object              = 'VIEWER'           and
   p-wgh-object:private-data = "invwr/v36in172.w" then do:

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
/*
                  IF i-itens MOD 2 <> 0 THEN
                      ASSIGN i-itens = i-itens * 2.
                  ELSE
                      ASSIGN i-itens = i-itens * 2.

                  assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEM-PAIRS, ",").
*/
                  assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEMS, ",").
                  /** assign wh-cd0204-upc02-depto:screen-value = entry(i-itens,wh-cd0204-upc02-depto:LIST-ITEMS). **/
              END.

          end.
      end.
      if l-achou = no  then do:
          if valid-handle(wh-cd0204-upc02-depto) then
              assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:list-items).
      end.
    end.

    if valid-handle(wh-cd0204-upc02-desc-depto) then
        assign wh-cd0204-upc02-desc-depto:visible = no.

    if valid-handle(wh-cd0204-upc02-depto) then
        assign wh-cd0204-upc02-depto:sensitive = yes.
end.

/* --->  ADD ITEM  <--- */
if p-ind-event               = "ADD"    and
   p-ind-object              = 'VIEWER' then do:
    /* Carregando Deptos cadastrados   */
    i-itens = 0.
    FOR EACH es-depto no-lock:
      ASSIGN i-itens = i-itens + 1
             c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " , " + string(es-depto.descricao,"x(30)") + ","
             i-qt-tipo = i-qt-tipo + 1 .
             /** c-tipo-tax = c-tipo-tax + string(es-depto.codigo,"999") + " - " + string(es-depto.descricao,"x(30)") + "," **/

    END.
    if valid-handle(wh-cd0204-upc02-depto) then
        assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:LIST-ITEMS).
/*         assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:LIST-ITEM-PAIRS). */
        /** assign wh-cd0204-upc02-depto:screen-value = entry(1,wh-cd0204-upc02-depto:list-items). **/
end.


/* --->  DISABLE  <--- */
if p-ind-event               = "AFTER-DISABLE"    and
   p-ind-object              = 'VIEWER'           and
   p-wgh-object:private-data = "invwr/v36in172.w" then do:

    assign wh-cd0204-upc02-depto:sensitive = no.

    if valid-handle(wh-cd0204-upc02-desc-depto) then
      assign wh-cd0204-upc02-desc-depto:visible = no.

end.

/* ---> GRAVANDO  <--- */
if p-ind-event  = "assign"    and
   p-ind-object = 'VIEWER'    and
   p-wgh-object:private-data = "invwr/v36in172.w" then do:

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


procedure pi-msg:
message "p-ind-event..:" p-ind-event                  skip
        "p-ind-object.:" p-ind-object                 skip
        "p-cod-table..:" STRING(p-cod-table)          skip
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip
        "p-wgh-frame..:" STRING(p-wgh-frame)          skip
        "p-row-table..:" string(p-row-table)          skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

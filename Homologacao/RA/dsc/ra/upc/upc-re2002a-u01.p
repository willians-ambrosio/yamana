/*****************************************************************
**
**    Programa: upc-re2002a-u01
**
**    Objetivo: Atualizacao de Contagem no Recebimento Fisico
**
**       Autor: Renato Oliveira
**
** Atualizacao: Dezembro / 2018
**
**      Versao: 2.12.00.000 - Desenvolvimento Inicial
**
*****************************************************************/
{include/i-prgvrs.i upc-re2002a-u01 2.12.00.000}

/* Definicao de Funcoes */
{tools/fc-handle-obj.i}
{tools/fc-falso-4.i}

/* Definicao de Parametros */
DEF INPUT PARAMETER p-ind-event  AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/* Definicao de Temp-Table */
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-cod-depos        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-quantidade       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-cod-depos-txt    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-cod-depos        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-cod-localiz-txt  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-cod-localiz      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-lote-txt         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-lote             AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-dt-vali-lote-txt AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-dt-vali-lote     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-bt-ok            AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-bt-ok-f          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-p-wgh-object     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-i-sequencia      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-c-it-codigo      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-c-un             AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002a-u01-rect-1           AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario                   AS CHAR FORMAT "x(12)" NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-doc-fisico                   AS ROWID               NO-UNDO.

/* Definicao de Variaveis Locais */
DEF VAR c-handle-obj AS CHAR NO-UNDO.

/* Definicao de Buffer */

/* MESSAGE "p-ind-event..:" p-ind-event            SKIP */
/*         "p-ind-object.:" p-ind-object           SKIP */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP */
/*         "p-wgh-frame..:" p-wgh-frame:NAME       SKIP */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP */
/*         "p-row-table..:" STRING(p-row-table)    SKIP */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.               */

/* Logica de Programacao */
IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "CONTAINER" THEN DO:

    ASSIGN wh-re2002a-u01-p-wgh-object = p-wgh-object.
           c-handle-obj = fc-handle-obj("bt-Ok",p-wgh-frame).
           wh-re2002a-u01-bt-Ok = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

    ASSIGN wh-re2002a-u01-bt-ok-f = fc-falso(wh-re2002a-u01-bt-ok,wh-re2002a-u01-bt-ok:FRAME,"").
           wh-re2002a-u01-bt-ok-f:LOAD-IMAGE-UP(wh-re2002a-u01-bt-ok:IMAGE-UP ).
           wh-re2002a-u01-bt-ok-f:LOAD-IMAGE-DOWN(wh-re2002a-u01-bt-ok:IMAGE-DOWN ).
           wh-re2002a-u01-bt-ok-f:SENSITIVE = YES.
           wh-re2002a-u01-bt-ok-f:MOVE-TO-TOP().
           wh-re2002a-u01-bt-ok:SENSITIVE   = NO.
    ON "choose":U OF wh-re2002a-u01-bt-ok-f PERSISTENT RUN upc/upc-re2002a-u01.p(INPUT "choose",
                                                                                 INPUT "wh-re2002a-u01-bt-ok-f",
                                                                                 INPUT p-wgh-object,
                                                                                 INPUT p-wgh-frame,
                                                                                 INPUT "",
                                                                                 INPUT  ?).

END.

IF  p-ind-event  = "choose"
AND p-ind-object = "wh-re2002a-u01-bt-ok-f" THEN DO:

    IF DEC(wh-re2002a-u01-quantidade:SCREEN-VALUE) = 0 THEN DO:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Quantidade n∆o pode ser Zero" + "~~" + "Quantidade n∆o pode ser Zero").
        APPLY "ENTRY" TO wh-re2002a-u01-quantidade.
        RETURN NO-APPLY.
    END.

     FIND FIRST doc-fisico NO-LOCK
          WHERE ROWID(doc-fisico) = gr-doc-fisico NO-ERROR.

     FIND FIRST deposito NO-LOCK
          WHERE deposito.cod-depos = wh-re2002a-u01-cod-depos:SCREEN-VALUE NO-ERROR.
     IF NOT AVAIL deposito THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                           INPUT 17006,
                           INPUT "Dep¢sito n∆o Cadastrado" + "~~" + "Dep¢sito n∆o Cadastrado").
         APPLY "ENTRY" TO wh-re2002a-u01-cod-depos.
         RETURN NO-APPLY.
     END.

     FIND FIRST ems2cademp.localizacao NO-LOCK
          WHERE localizacao.cod-estabel = doc-fisico.cod-estabel 
            AND localizacao.cod-depos   = deposito.cod-depos
            AND localizacao.cod-localiz = wh-re2002a-u01-cod-localiz:SCREEN-VALUE NO-ERROR.
     IF NOT AVAIL localizacao THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                           INPUT 17006,
                           INPUT "Localizaá∆o n∆o Cadastrada" + "~~" + "Localizaá∆o n∆o Cadastrada").
         APPLY "ENTRY" TO wh-re2002a-u01-cod-localiz.
         RETURN NO-APPLY.
     END.

     IF  wh-re2002a-u01-lote:SENSITIVE = YES
     AND wh-re2002a-u01-lote:SCREEN-VALUE = "" THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                           INPUT 17006,
                           INPUT "Lote em Branco" + "~~" + "Lote em Branco").
         APPLY "ENTRY" TO wh-re2002a-u01-lote.
         RETURN NO-APPLY.
     END.

     IF wh-re2002a-u01-dt-vali-lote:SENSITIVE = YES
     AND DATE(wh-re2002a-u01-dt-vali-lote:SCREEN-VALUE) = ? THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                           INPUT 17006,
                           INPUT "Validade do Lote em Branco" + "~~" + "Validade do Lote em Branco").
         APPLY "ENTRY" TO wh-re2002a-u01-dt-vali-lote.
         RETURN NO-APPLY.
     END.

     FIND FIRST it-doc-fisico NO-LOCK 
          WHERE it-doc-fisico.serie-docto  = doc-fisico.serie-docto
            AND it-doc-fisico.nro-docto    = doc-fisico.nro-docto
            AND it-doc-fisico.cod-emitente = doc-fisico.cod-emitente
            AND it-doc-fisico.tipo-nota    = doc-fisico.tipo-nota
            AND it-doc-fisico.sequencia    = INT(wh-re2002a-u01-i-sequencia:SCREEN-VALUE) NO-ERROR.

     FIND FIRST rat-lote EXCLUSIVE-LOCK
          WHERE rat-lote.serie-docto  = it-doc-fisico.serie-docto
            AND rat-lote.nro-docto    = it-doc-fisico.nro-docto   
            AND rat-lote.cod-emitente = it-doc-fisico.cod-emitente
            AND rat-lote.nat-operacao = ""
            AND rat-lote.sequencia    = it-doc-fisico.sequencia NO-ERROR.
     IF NOT AVAIL rat-lote THEN DO:
         CREATE rat-lote.
         ASSIGN rat-lote.cod-emitente = it-doc-fisico.cod-emitente
                rat-lote.serie-docto  = it-doc-fisico.serie-docto
                rat-lote.nro-docto    = it-doc-fisico.nro-docto
                rat-lote.nat-operacao = ""
                rat-lote.tipo-nota    = it-doc-fisico.tipo-nota
                rat-lote.sequencia    = it-doc-fisico.sequencia
                rat-lote.int-2        = 1 /* Inclusao */.
     END.
     ELSE
         ASSIGN rat-lote.int-2 = 2 /* Alteracao */.

     ASSIGN rat-lote.it-codigo    = it-doc-fisico.it-codigo
            rat-lote.cod-depos    = wh-re2002a-u01-cod-depos:SCREEN-VALUE
            rat-lote.cod-localiz  = wh-re2002a-u01-cod-localiz:SCREEN-VALUE
            rat-lote.lote         = wh-re2002a-u01-lote:SCREEN-VALUE
            rat-lote.dt-vali-lote = DATE(wh-re2002a-u01-dt-vali-lote:SCREEN-VALUE)
            rat-lote.quantidade   = DEC(wh-re2002a-u01-quantidade:SCREEN-VALUE).

     FIND CURRENT it-doc-fisico EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL it-doc-fisico THEN DO:
         ASSIGN it-doc-fisico.quant-conf = rat-lote.quantidade.

/*          /* Elimina a quantidade contada para subir um novo registro */        */
/*          FIND FIRST es-it-doc-fisico EXCLUSIVE-LOCK OF it-doc-fisico NO-ERROR. */
/*          IF AVAIL es-it-doc-fisico THEN                                        */
/*              DELETE es-it-doc-fisico.                                          */
/*                                                                                */
/*          CREATE es-it-doc-fisico.                                              */
/*          BUFFER-COPY it-doc-fisico TO es-it-doc-fisico.                        */
/*          ASSIGN es-it-doc-fisico.situacao         = 1 /* Pendente*/            */
/*                 es-it-doc-fisico.usuario-bloq = c-seg-usuario                  */
/*                 es-it-doc-fisico.data-bloq    = TODAY                          */
/*                 es-it-doc-fisico.hora-bloq    = STRING(TIME,"HH:MM:SS").       */

         RELEASE it-doc-fisico.
     END.

     RELEASE rat-lote.

     APPLY "choose" TO wh-re2002a-u01-bt-ok.

     FIND FIRST es-doc-fisico EXCLUSIVE-LOCK OF doc-fisico NO-ERROR.
     IF NOT AVAIL es-doc-fisico THEN DO:
         CREATE es-doc-fisico.
         ASSIGN es-doc-fisico.serie-docto  = doc-fisico.serie-docto
                es-doc-fisico.nro-docto    = doc-fisico.nro-docto
                es-doc-fisico.cod-emitente = doc-fisico.cod-emitente
                es-doc-fisico.tipo-nota    = doc-fisico.tipo-nota NO-ERROR.
     END.

     FIND FIRST it-doc-fisico NO-LOCK OF doc-fisico
          WHERE it-doc-fisico.quant-conf <> it-doc-fisico.quantidade
            AND it-doc-fisico.quant-conf  > 0 NO-ERROR.
     IF AVAIL it-doc-fisico THEN DO:
         ASSIGN es-doc-fisico.situacao = "Divergente".
     END.
     ELSE DO:

         FIND FIRST it-doc-fisico NO-LOCK OF doc-fisico
              WHERE it-doc-fisico.quant-conf = it-doc-fisico.quantidade
                AND it-doc-fisico.quant-conf > 0 NO-ERROR.
         IF AVAIL it-doc-fisico THEN
             ASSIGN es-doc-fisico.situacao = "Liberado Nota".
         ELSE
             ASSIGN es-doc-fisico.situacao = "Contagem".
     END.

     RELEASE es-doc-fisico.

END.

IF  p-ind-event  = "CHANGE-PAGE"
AND p-ind-object = "CONTAINER" THEN DO:

    RUN select-page IN wh-re2002a-u01-p-wgh-object (INPUT 1).

END.

IF  p-ind-event  = "AFTER-OPEN-QUERY" 
AND p-ind-object = "BROWSER"
AND p-wgh-object:FILE-NAME MATCHES "*b09in368*" THEN DO:

    IF VALID-HANDLE(wh-re2002a-u01-c-it-codigo) THEN DO:

        IF wh-re2002a-u01-c-it-codigo:SCREEN-VALUE = "" THEN NEXT.

        FIND FIRST item NO-LOCK
             WHERE item.it-codigo = wh-re2002a-u01-c-it-codigo:SCREEN-VALUE NO-ERROR.
        IF AVAIL item THEN DO:

            ASSIGN wh-re2002a-u01-cod-depos:SCREEN-VALUE    = item.deposito-pad
                   wh-re2002a-u01-cod-localiz:SCREEN-VALUE  = item.cod-localiz.

            IF item.tipo-con-est <> 3 THEN
                ASSIGN wh-re2002a-u01-lote:SENSITIVE = NO
                       wh-re2002a-u01-dt-vali-lote:SENSITIVE = NO.
                   
        END.

    END.

END.


IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME MATCHES "*v10in163*" THEN DO:

     ASSIGN c-handle-obj = fc-handle-obj("c-un,rect-1,c-it-codigo,quantidade,i-sequencia",p-wgh-frame)  
            wh-re2002a-u01-c-un        = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
            wh-re2002a-u01-rect-1      = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
            wh-re2002a-u01-c-it-codigo = WIDGET-HANDLE(ENTRY(3,c-handle-obj))
            wh-re2002a-u01-quantidade  = WIDGET-HANDLE(ENTRY(4,c-handle-obj))
            wh-re2002a-u01-i-sequencia = WIDGET-HANDLE(ENTRY(5,c-handle-obj))
            p-wgh-frame:FRAME:HEIGHT   = p-wgh-frame:FRAME:HEIGHT + 4
            p-wgh-frame:HEIGHT         = p-wgh-frame:HEIGHT       + 4.

     IF VALID-HANDLE(wh-re2002a-u01-rect-1) THEN
         ASSIGN wh-re2002a-u01-rect-1:HEIGHT = wh-re2002a-u01-rect-1:HEIGHT + 4.

     IF VALID-HANDLE(wh-re2002a-u01-c-un) THEN DO:

         CREATE TEXT wh-re2002a-u01-cod-depos-txt
         ASSIGN FRAME         =  p-wgh-frame
                WIDTH         =  11
                HEIGHT        =  0.88
                ROW           =  wh-re2002a-u01-c-un:ROW + 1
                COL           =  wh-re2002a-u01-c-un:COL - 6.8
                FORMAT        = "x(11)"
                SCREEN-VALUE  = "Dep¢sito:"
                SENSITIVE     =  NO
                VISIBLE       =  YES.
    
         CREATE FILL-IN wh-re2002a-u01-cod-depos
         ASSIGN FRAME             =  p-wgh-frame
                WIDTH             =  5
                HEIGHT            =  wh-re2002a-u01-cod-depos-txt:HEIGHT
                DATA-TYPE         = "CHARACTER"
                FORMAT            =  "X(3)"
                ROW               =  wh-re2002a-u01-c-un:ROW + 1
                COL               =  wh-re2002a-u01-c-un:COL 
                SENSITIVE         =  YES
                SIDE-LABEL-HANDLE =  wh-re2002a-u01-cod-depos-txt:HANDLE
                VISIBLE           =  YES
                NAME              =  "wh-re2002a-u01-cod-depos"
         TRIGGERS:
         END TRIGGERS.

         CREATE TEXT wh-re2002a-u01-cod-localiz-txt
         ASSIGN FRAME         =  p-wgh-frame
                WIDTH         =  12
                HEIGHT        =  0.88
                ROW           =  wh-re2002a-u01-c-un:ROW + 2
                COL           =  wh-re2002a-u01-c-un:COL - 9
                FORMAT        = "x(12)"
                SCREEN-VALUE  = "Localizaá∆o:"
                SENSITIVE     =  NO
                VISIBLE       =  YES.
    
         CREATE FILL-IN wh-re2002a-u01-cod-localiz
         ASSIGN FRAME             =  p-wgh-frame
                WIDTH             =  11
                HEIGHT            =  wh-re2002a-u01-cod-localiz-txt:HEIGHT
                DATA-TYPE         = "CHARACTER"
                FORMAT            =  "X(20)"
                ROW               =  wh-re2002a-u01-c-un:ROW + 2
                COL               =  wh-re2002a-u01-c-un:COL 
                SENSITIVE         =  YES
                SIDE-LABEL-HANDLE =  wh-re2002a-u01-cod-localiz-txt:HANDLE
                VISIBLE           =  YES
                NAME              =  "wh-re2002a-u01-cod-localiz"
         TRIGGERS:
         END TRIGGERS.

         CREATE TEXT wh-re2002a-u01-lote-txt
         ASSIGN FRAME         =  p-wgh-frame
                WIDTH         =  12
                HEIGHT        =  0.88
                ROW           =  wh-re2002a-u01-c-un:ROW + 3
                COL           =  wh-re2002a-u01-c-un:COL - 4
                FORMAT        = "x(12)"
                SCREEN-VALUE  = "Lote:"
                SENSITIVE     =  NO
                VISIBLE       =  YES.
    
         CREATE FILL-IN wh-re2002a-u01-lote
         ASSIGN FRAME             =  p-wgh-frame
                WIDTH             =  11
                HEIGHT            =  wh-re2002a-u01-lote-txt:HEIGHT
                DATA-TYPE         = "CHARACTER"
                FORMAT            =  "X(20)"
                ROW               =  wh-re2002a-u01-c-un:ROW + 3
                COL               =  wh-re2002a-u01-c-un:COL 
                SENSITIVE         =  YES
                SIDE-LABEL-HANDLE =  wh-re2002a-u01-lote-txt:HANDLE
                VISIBLE           =  YES
                NAME              =  "wh-re2002a-u01-lote"
         TRIGGERS:
         END TRIGGERS.

         CREATE TEXT wh-re2002a-u01-dt-vali-lote-txt
         ASSIGN FRAME         =  p-wgh-frame
                WIDTH         =  12
                HEIGHT        =  0.88
                ROW           =  wh-re2002a-u01-c-un:ROW + 4
                COL           =  wh-re2002a-u01-c-un:COL - 6.8
                FORMAT        = "x(12)"
                SCREEN-VALUE  = "Validade:"
                SENSITIVE     =  NO
                VISIBLE       =  YES.
    
         CREATE FILL-IN wh-re2002a-u01-dt-vali-lote
         ASSIGN FRAME             =  p-wgh-frame
                WIDTH             =  11
                HEIGHT            =  wh-re2002a-u01-dt-vali-lote-txt:HEIGHT
                DATA-TYPE         = "DATE"
                FORMAT            =  "99/99/9999"
                ROW               =  wh-re2002a-u01-c-un:ROW + 4
                COL               =  wh-re2002a-u01-c-un:COL 
                SENSITIVE         =  YES
                SIDE-LABEL-HANDLE =  wh-re2002a-u01-dt-vali-lote-txt:HANDLE
                VISIBLE           =  YES
                NAME              =  "wh-re2002a-u01-dt-vali-lote"
         TRIGGERS:
         END TRIGGERS.

     END.     

END.

/* Fim do Programa */

/**============================================================**
** Altera‡Æo...: Thiago Coutinho
** Empresa.....: CSX Solution
** Data........: Maio/2012
** Objetivo....: Permitir a altera‡Æo do Comprador quando a ordem
** ............: ja esta finalizada ou terminada. 
**=============================================================**/

DEF INPUT PARAMETER p-h-cc0301a         AS WIDGET-HANDLE   NO-UNDO.

define new global shared var wh-cc0301a-i-ordem        as widget-handle no-undo.
define new global shared var wh-cc0301a-i-seq-ordem    as widget-handle no-undo.
define new global shared var wh-cc0301a-cod-comprado   as widget-handle no-undo.
define new global shared var wh-cc0301a-ordem-servic   as widget-handle no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cc0301a-bt-ok-ant      AS WIDGET-HANDLE NO-UNDO.

define new global shared VAR c-seg-usuario   as char format 'x(12)' no-undo.
DEFINE SHARED VAR gl-cc0301a-c-cod-comprado  AS CHAR NO-UNDO.

DEFINE VARIABLE i-codigo-esp AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-narrativa  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-numero-ordem AS INTEGER     NO-UNDO.

ASSIGN i-codigo-esp = 0.

IF VALID-HANDLE(wh-cc0301a-i-ordem) AND
   VALID-HANDLE(wh-cc0301a-i-seq-ordem) and
   valid-handle(wh-cc0301a-cod-comprado) and
   valid-handle(wh-cc0301a-ordem-servic)THEN DO:

    find first ord-prod
         where ord-prod.nr-ord-produ = INT(wh-cc0301a-ordem-servic:SCREEN-VALUE)
         no-lock no-error.
    if  avail ord-prod then do:
        if  ord-prod.estado = 7 OR
            ord-prod.estado = 8 then do:

            ASSIGN i-codigo-esp = 10.

            ASSIGN i-numero-ordem = INT(wh-cc0301a-i-ordem:SCREEN-VALUE + wh-cc0301a-i-seq-ordem:SCREEN-VALUE).

            FIND FIRST ordem-compra
                 WHERE ordem-compra.numero-ordem = i-numero-ordem
                 NO-LOCK NO-ERROR.
            IF AVAIL ordem-compra THEN DO:

                FIND FIRST param-global NO-LOCK NO-ERROR.
                IF ordem-compra.cod-comprado            <> wh-cc0301a-cod-comprado:SCREEN-VALUE AND
                   wh-cc0301a-ordem-servic:SCREEN-VALUE > '0'                                   AND 
                   param-global.modulo-cp               = yes THEN DO:

                    find first ord-prod
                         where ord-prod.nr-ord-produ = INT(wh-cc0301a-ordem-servic:SCREEN-VALUE)
                         no-lock no-error.
                    if  avail ord-prod then do:
                        if  ord-prod.estado = 7 then do:
                            ASSIGN i-codigo-esp = 10.
                        end.
                        if  ord-prod.estado = 8 then do:
                            ASSIGN i-codigo-esp = 10.
                        end.
                    end.

                    IF i-codigo-esp > 0 THEN DO:
                        ASSIGN c-narrativa = ' - '+ CHR(13) + ' Foi alterado o comprador ' + gl-cc0301a-c-cod-comprado + ' para o comprador ' + wh-cc0301a-cod-comprado:SCREEN-VALUE + 
                                             ' por ' + c-seg-usuario + '  no dia  ' + STRING(TODAY,"99/99/9999") + " as " + string(time,"hh:mm:ss") + '.'
                               i-numero-ordem = INT(wh-cc0301a-i-ordem:SCREEN-VALUE + wh-cc0301a-i-seq-ordem:SCREEN-VALUE).

                        FIND FIRST ordem-compra
                             WHERE ordem-compra.numero-ordem = i-numero-ordem
                             EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAIL ordem-compra THEN DO:
                            RUN utp/ut-msgs.p (INPUT "show",
                                               INPUT 27100,
                                               INPUT "Deseja alterar o comprador?" +
                                                 "~~" +
                                                 "Comprador foi alterado, Confirma?").
                            IF RETURN-VALUE = "YES" THEN DO:
                                ASSIGN ordem-compra.cod-comprado = wh-cc0301a-cod-comprado:SCREEN-VALUE
                                       /** ordem-compra.narrativa    = ordem-compra.narrativa + c-narrativa **/
                                       .
                            END.

                        END.

                /*         RUN pi-reposiciona-query IN <HANDLE DO programa chamador cc0301> (INPUT <parametro da upc com ROWID>). */
                        IF VALID-HANDLE(p-h-cc0301a) THEN DO:
                            APPLY "close" to p-h-cc0301a.
                        END.
                    END.

                END.
                ELSE DO:
                    IF VALID-HANDLE(wh-cc0301a-bt-ok-ant) THEN DO:
                        APPLY "CHOOSE":U TO wh-cc0301a-bt-ok-ant.
                    END.
                END.

            END. /* IF AVAIL ordem-compra THEN DO: */
            ELSE DO:
                IF VALID-HANDLE(wh-cc0301a-bt-ok-ant) THEN DO:
                    APPLY "CHOOSE":U TO wh-cc0301a-bt-ok-ant.
                END.
            END.

        end. /* if  ord-prod.estado = 7 OR - Terminada ou Finalizada */
        ELSE DO:
            IF VALID-HANDLE(wh-cc0301a-bt-ok-ant) THEN DO:
                APPLY "CHOOSE":U TO wh-cc0301a-bt-ok-ant.
            END.
        END.
    end. /* if  avail ord-prod then do: */
    ELSE DO:
        IF VALID-HANDLE(wh-cc0301a-bt-ok-ant) THEN DO:
            APPLY "CHOOSE":U TO wh-cc0301a-bt-ok-ant.
        END.
    END.

    /**
    IF valid-handle(wh-cc0301a-i-ordem) THEN DO:
        DELETE PROCEDURE wh-cc0301a-i-ordem.
        ASSIGN wh-cc0301a-i-ordem = ?.
    END.

    IF VALID-HANDLE(wh-cc0301a-i-seq-ordem ) THEN DO:
        DELETE PROCEDURE wh-cc0301a-i-seq-ordem .
        ASSIGN wh-cc0301a-i-seq-ordem = ?.
    END.

    IF VALID-HANDLE(wh-cc0301a-cod-comprado) THEN DO:
        DELETE PROCEDURE wh-cc0301a-cod-comprado.
        ASSIGN wh-cc0301a-cod-comprado  = ?.
    END.

    IF VALID-HANDLE(wh-cc0301a-ordem-servic) THEN DO:
        DELETE PROCEDURE wh-cc0301a-ordem-servic.
        ASSIGN wh-cc0301a-ordem-servic = ?.
    END.
    **/
END.
ELSE DO:
    IF VALID-HANDLE(wh-cc0301a-bt-ok-ant) THEN DO:
        APPLY "CHOOSE":U TO wh-cc0301a-bt-ok-ant.
    END.
END.





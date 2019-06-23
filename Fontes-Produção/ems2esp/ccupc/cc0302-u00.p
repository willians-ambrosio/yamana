/**************************************************************\
***************************************************************
**  Programa: arce0101-u00.p
**  Objetivo: Organizar os chamados de UPC para sub-programas
**  Autor...: Marco Valverde - GWA
**  Data....: 03/01/2002
**  Versao..: I.00.000
***************************************************************
\**************************************************************/

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

DEF VAR wh-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-aux AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR gwh-rt-ult-compra AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR gwh-tg-ult-compra AS WIDGET-HANDLE.

IF p-ind-event = 'INITIALIZE' AND
   p-ind-object = 'CONTAINER' THEN DO:

    ASSIGN wh-objeto = p-wgh-frame.
    DO WHILE VALID-HANDLE(wh-objeto):
        IF wh-objeto:NAME = 'f-pg-par' THEN DO:
            
            ASSIGN wh-aux = wh-objeto:FIRST-CHILD  /* Field-Group */
                   wh-aux = wh-aux:FIRST-CHILD.    /* primeiro campo da frame */

/*             DO WHILE VALID-HANDLE(wh-aux):             */
/*                                                        */
/*                 IF wh-aux:TYPE = 'rectangle' THEN      */
/*                 MESSAGE wh-aux:NAME SKIP               */
/*                         wh-aux:FGCOLOR SKIP            */
/*                                                        */
/*                         VIEW-AS ALERT-BOX TITLE 'aux'. */
/*                                                        */
/*                 wh-aux = wh-aux:NEXT-SIBLING.          */
/*                                                        */
/*             END.                                       */

            IF VALID-HANDLE(gwh-rt-ult-compra) THEN
                DELETE OBJECT gwh-rt-ult-compra.

            CREATE RECTANGLE gwh-rt-ult-compra
                ASSIGN HEIGHT       = 1.3
                       WIDTH        = 69.72
                       COL          = 3.29
                       ROW          = 10
                       FRAME        = wh-objeto
                       VISIBLE      = YES
                       FILLED       = NO
                       GRAPHIC-EDGE = YES
                       EDGE-PIXELS  = 2.

/*         IF VALID-HANDLE(gwh-tg-ult-compra) THEN */
/*             DELETE OBJECT gwh-tg-ult-compra.    */

        CREATE TOGGLE-BOX gwh-tg-ult-compra
            ASSIGN FRAME             = wh-objeto
                   WIDTH             = 25
                   HEIGHT            = 0.88
                   ROW               = 10.17
                   COLUMN            = 25
                   LABEL             = "Imprime éltima Compra"
                   SENSITIVE         = yes
                   VISIBLE           = YES.


            LEAVE.
        END. /* IF wh-objeto:NAME = 'f-pg-par' */
        wh-objeto = wh-objeto:NEXT-SIBLING.

    END. /* DO WHILE VALID-HANDLE(wh-objeto) */

END.

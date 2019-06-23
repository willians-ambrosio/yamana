/***************************************************************************
      Filename: Re1001a1-u01.p
      Comment: upc Re1001a1 - Modifica Documento de Estoque
      Created:  20.01.2016 Joao B. C. Bisneto
****************************************************************************/
def buffer empresa for ems2cadme.empresa.

def input param p-ind-event   as char          no-undo.
def input param p-ind-object  as CHAR          no-undo.
def input param p-wgh-object  as handle        no-undo.
def input param p-wgh-frame   as widget-handle no-undo.
def input param p-cod-table   as char          no-undo.
def input param p-row-table   as rowid         no-undo.

{utp/ut-glob.i}

DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-bt-gera-por-item-re1001    AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-frame1-re1001a1            AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-conta-transit-re1001a1     AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nat-operacao-re1001a1      AS WIDGET-HANDLE  NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-fPage1                     AS WIDGET-HANDLE    NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-fPage2                     AS WIDGET-HANDLE    NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-cod-rep                    AS WIDGET-HANDLE    NO-UNDO.

DEF VAR wh-grupo      AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-child     AS WIDGET-HANDLE NO-UNDO.
DEF VAR wgh-objeto    AS WIDGET-HANDLE NO-UNDO.
DEF VAR wgh-grupo      AS WIDGET-HANDLE NO-UNDO.
DEF VAR wgh-child     AS WIDGET-HANDLE NO-UNDO.




/* MESSAGE "p-ind-event..:" p-ind-event                  SKIP */
/*         "p-ind-object.:" p-ind-object                 SKIP */
/*         "p-cod-table..:" STRING(p-cod-table)          SKIP */
/*         "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP */
/*         "p-row-table..:" STRING(p-row-table) SKIP          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/* IF NOT VALID-HANDLE(wh-conta-transit-re1001a1   ) THEN */
/*   RUN pi-acha-handle.                                  */

if
  p-ind-event  = "AFTER-INITIALIZE"  AND 
  p-ind-object = "CONTAINER"         THEN 
  DO:

     RUN utp\esut0001.p(INPUT p-wgh-frame,
                        INPUT "fPage2",
                        OUTPUT wh-fPage2).

     RUN utp\esut0001.p(INPUT wh-fPage2,
                        INPUT "ct-transit",
                        OUTPUT wh-conta-transit-re1001a1).

     RUN utp\esut0001.p(INPUT p-wgh-frame,
                        INPUT "fPage1",
                        OUTPUT wh-fPage1).

     RUN utp\esut0001.p(INPUT wh-fPage1,
                        INPUT "nat-operacao",
                        OUTPUT wh-nat-operacao-re1001a1).



     /* depuracao - Bisneto
     IF VALID-HANDLE(wh-conta-transit-re1001a1) THEN
       MESSAGE wh-conta-transit-re1001a1:NAME
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     */
     /*--------------------------------------------------------*/
     IF 
       VALID-HANDLE(wh-bt-gera-por-item-re1001) AND 
       VALID-HANDLE(wh-conta-transit-re1001a1)  AND 
       VALID-HANDLE(wh-nat-operacao-re1001a1)   THEN
       DO:
         /* 
         MESSAGE "wh-nat-operacao-re1001a1:VALUE " wh-nat-operacao-re1001a1:SCREEN-VALUE
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         */    
         FIND natur-oper
           NO-LOCK
           WHERE natur-oper.nat-operacao = wh-nat-operacao-re1001a1:SCREEN-VALUE
           NO-ERROR.
         IF AVAIL natur-oper AND natur-oper.terceiros = YES THEN
           DO:
             /* depuracao
             MESSAGE natur-oper.terceiros
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             */
             ASSIGN wh-conta-transit-re1001a1:SENSITIVE = NO.
           END.
       END.
     /*--------------------------------------------------------*/
  END. /* if p-ind-event  = "AFTER-INITIALIZE"  */


/***************************************************************************
      Filename: in0310-esp.p
      Comment: upc para gera‡Æo de pendˆncias para o item da tabela de pre‡o
      Created:  06.07.2008 21:34 Cristiano Martins Pereira
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

DEFINE NEW GLOBAL SHARED VARIABLE tt-hist-hist-alter-inv AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-tipo                 AS INTEGER       NO-UNDO.
DEFINE new global shared VARIABLE h-in0310               AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-browse              AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-query               AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-frame              AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-num-proj            AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-num-ordem           AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cod-estabel         AS WIDGET-HANDLE NO-UNDO.
DEFINE new global shared VARIABLE c-num-ordem            AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-bt-eliminar         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-browser             AS WIDGET-HANDLE NO-UNDO.


DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-ordem-inv LIKE ordem-inv.


if  p-ind-event  = "BEFORE-DISPLAY"
and p-ind-object = "VIEWER" then do:

    
    RUN pi-busca-handle (INPUT "num-projeto",
                         INPUT  p-wgh-frame,
                         OUTPUT wh-num-proj).

   RUN pi-busca-handle (INPUT "cod-est-exec",
                        INPUT  p-wgh-frame,
                        OUTPUT wh-cod-estabel).
          
end.

if  p-ind-event  = "BEFORE-INITIALIZE"
and p-ind-object = "browser" then do:

    assign h-browser = p-wgh-object.

    RUN upc\in0310-a-esp.p PERSISTENT SET h-in0310 (INPUT "",            
                                                        INPUT "",            
                                                        INPUT p-wgh-object,  
                                                        INPUT p-wgh-frame,   
                                                        INPUT "",            
                                                        INPUT p-row-table).

    on ROW-DISPLAY OF h-browser persistent run pi-row-display in h-in0310.

    create button wh-bt-eliminar
           assign frame     = p-wgh-frame
                  width     = 10
                  height    = 1
                  row       = 8.75
                  col       = 21 
                  sensitive = yes
                  visible   = yes
                  label     = "&Eliminar"
                  tooltip   = "teste"
                  triggers:
                      on choose persistent run pi-eliminar in h-in0310.
                  end triggers.

end.

/* Bisnet 12/01/2015
if  p-ind-event  = "display"  /*AFTER-VALUE-CHANGED*/
and p-ind-object = "viewer" then do:


 if valid-handle(h-browser) then
     apply 'row-display' to h-browser .

end.
*/

if  p-ind-event  = "after-OPEN-QUERY"  /*AFTER-VALUE-CHANGED*/
and p-ind-object = "BROWSER" then do:
    
    for each tt-ordem-inv:
        delete tt-ordem-inv.
    end.

    for each  ordem-inv
        where ordem-inv.cod-est-exec = wh-cod-estabel:screen-value
        and   ordem-inv.num-projeto  = int(wh-num-proj:screen-value) no-lock:

        create tt-ordem-inv.
        buffer-copy ordem-inv to tt-ordem-inv.

    end.
end.



if  p-ind-event  = "VALUE-CHANGED"  /*AFTER-VALUE-CHANGED*/
and p-ind-object = "BROWSER" then do:

   run pi-row-display in h-in0310.
    
   
end.



procedure pi-eliminar:

    do trans:
        find first tt-ordem-inv
            where  tt-ordem-inv.cod-est-exec = wh-cod-estabel:screen-value
            and    tt-ordem-inv.num-projeto  = int(wh-num-proj:screen-value)
            and    tt-ordem-inv.num-ordem    = int(c-num-ordem) no-lock no-error.
        if avail tt-ordem-inv  then do:
            for each  hist-alter-inv
                where hist-alter-inv.cod-estabel-exe  = tt-ordem-inv.cod-est-exec
                and   hist-alter-inv.num-proj         = tt-ordem-inv.num-projeto
                and   hist-alter-inv.num-ordem        = tt-ordem-inv.num-ordem  :

                assign hist-alter-inv.tipo = 3.

            end.
        end.
        apply 'choose' to wh-bt-eliminar.
        if return-value = "adm-error" then
            UNDO, RETURN "NOK":U.

    end.
 

end procedure.

PROCEDURE pi-busca-handle:

    DEF INPUT  PARAM p-campo      AS CHAR          NO-UNDO.
    DEF INPUT  PARAM p-frame      AS WIDGET-HANDLE NO-UNDO.
    DEF OUTPUT PARAM p-wh-campo   AS WIDGET-HANDLE NO-UNDO.
    
    def var wh-grupo              as widget-handle no-undo.
    def var wh-child              as widget-handle no-undo.
    
    do:
        assign wh-grupo = p-frame:first-child.
        
        do  while valid-handle(wh-grupo):
            if  wh-grupo:type <> "field-group" then do:
                if  wh-grupo:name = p-campo then do:
                    assign p-wh-campo = wh-grupo.
                    leave.
                end.
                assign wh-grupo = wh-grupo:next-sibling.
            end.
            else do:
                assign wh-grupo = wh-grupo:first-child.
            end.
        end.
    end.

END PROCEDURE.
                                          
PROCEDURE pi-row-display:

    RUN pi-busca-handle (INPUT "bt-eliminar",
                         INPUT  p-wgh-frame,
                         OUTPUT wh-bt-eliminar).

    assign c-num-ordem = "".


    ASSIGN wh-browse = p-wgh-frame:FIRST-CHILD.
    
    DO WHILE VALID-HANDLE(wh-browse):
       IF wh-browse:TYPE = "BROWSE"      AND
          wh-browse:NAME = "br-table" THEN DO:
          IF wh-browse:NUM-SELECTED-ROWS > 0 THEN DO:
             ASSIGN wh-query = wh-browse:QUERY.
             ASSIGN tt-hist-hist-alter-inv = wh-query:GET-BUFFER-HANDLE(1) NO-ERROR.
             DO i-cont = 1 TO wh-browse:NUM-SELECTED-ROWS:
                /* bisneto 12/01/2016
                IF wh-browse:FETCH-SELECTED-ROW(i-cont) THEN DO:
                   ASSIGN wh-num-ordem = tt-hist-hist-alter-inv:BUFFER-FIELD("num-ordem").
                   ASSIGN c-num-ordem = wh-num-ordem:BUFFER-VALUE().
                end.
                */
             END.
          END.
       END.
       IF wh-browse:TYPE = "FIELD-GROUP" THEN
            ASSIGN wh-browse = wh-browse:FIRST-CHILD.
       ELSE ASSIGN wh-browse = wh-browse:NEXT-SIBLING.
    END.

end procedure.

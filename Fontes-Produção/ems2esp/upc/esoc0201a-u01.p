/******************************************************************************
**
**  Programa: upc/esoc0201a-u01.p
**  Data....: Junho 2013
**  Autor...: Fernando Campos - DSC
**  Objetivo: Bloqueia condi‡Æo de pagamento desativada
**
******************************************************************************/
 
{include/i-prgvrs.i esoc0201a-u01 2.06.00.001}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}

{utp/ut-glob.i}

/*-----> Define de Parametros <---------------------------------------*/
def input parameter p-ind-event              as char          no-undo.
def input parameter p-ind-object             as char          no-undo.
def input parameter p-wgh-object             as handle        no-undo.
def input parameter p-wgh-frame              as widget-handle no-undo.
def input parameter p-cod-table              as char          no-undo.
def input parameter p-row-table              as rowid         no-undo.

DEF TEMP-TABLE tt-beneficio
    FIELD cod-beneficio AS INT
    FIELD rid-ben       AS ROWID
    FIELD tp-tax        AS CHAR
    FIELD prior         AS INT.


/*-----> Define de Variaveis <----------------------------------------*/

def new global shared var wh-esoc0201a-cod-cond-pag as handle   no-undo.
def new global shared var wh-esoc0201a-i-ordem      as handle   no-undo.
def new global shared var wh-esoc0201a-i-sequencia  as handle   no-undo.


def var c-handle-obj        as character       no-undo.

/*-----> Main Block <-------------------------------------------------*/

/* message "p-ind-event..:" p-ind-event            skip */
/*         "p-ind-object.:" p-ind-object           skip */
/*         "p-wgh-object.:" p-wgh-object:file-name skip */
/*         "p-wgh-frame..:" string(p-wgh-frame)    skip */
/*         "p-cod-table..:" string(p-cod-table)    skip */
/*         "p-row-table..:" string(p-row-table)    skip */
/*         view-as alert-box info buttons ok.           */

if p-ind-event  = "BEFORE-INITIALIZE"             and
   p-ind-object = "VIEWER"                        and
   p-wgh-object:file-name matches "*oc0201a-v03*" then do:

    assign c-handle-obj              = fc-handle-obj("i-cod-cond-pag", p-wgh-frame)
           wh-esoc0201a-cod-cond-pag = widget-handle(entry(1, c-handle-obj)) no-error.
           
end.

if p-ind-event  = "BEFORE-INITIALIZE"             and
   p-ind-object = "VIEWER"                        and
   p-wgh-object:file-name matches "*oc0201a-v01*" then do:

    assign c-handle-obj              = fc-handle-obj("i-ordem", p-wgh-frame)
           wh-esoc0201a-i-ordem      = widget-handle(entry(1, c-handle-obj)) no-error.
           
    assign c-handle-obj              = fc-handle-obj("i-sequencia", p-wgh-frame)
           wh-esoc0201a-i-sequencia  = widget-handle(entry(1, c-handle-obj)) no-error.
           

end.

if p-ind-event  = "VALIDATE"                      and
   p-ind-object = "VIEWER"                        and
   p-wgh-object:file-name matches "*oc0201a-v01*" then do:

    find es-cond-pagto no-lock
        where es-cond-pagto.cod-cond-pag = int(wh-esoc0201a-cod-cond-pag:screen-value) no-error.
    if avail es-cond-pagto    and
       es-cond-pagto.cd-ativa then do:
        run utp/ut-msgs.p("show",
                          17006,
                          "Condi‡Æo de pagamento desativada~~Condi‡Æo de pagamento desativada. Favor informar uma condi‡Æo de pagamento valida").
        return "NOK".
    end.
    
    
      find ordem-compra where
            ordem-compra.numero-ordem = (( int(wh-esoc0201a-i-ordem:screen-value) * 100) + 
                                          int(wh-esoc0201a-i-sequencia:screen-value)) no-lock no-error.  

         RUN esp\procura-beneficio.p (INPUT ordem-compra.cod-estabel,                 
                                      INPUT ordem-compra.it-codigo,                   
                                      INPUT ordem-compra.cod-emitente,    
                                      OUTPUT TABLE tt-beneficio ).  

         FIND FIRST tt-beneficio NO-LOCK NO-ERROR.
         


         FOR EACH TT-BENEFICIO  BREAK BY  TT-BENEFICIO.tp-tax BY TT-BENEFICIO.PRIOR:


             IF FIRST-OF(TT-BENEFICIO.tp-tax) THEN DO:
            
                 FIND es-ben-estab WHERE ROWID(es-ben-estab) = TT-BENEFICIO.rid-ben NO-LOCK NO-ERROR.
    
                 IF AVAIL es-ben-estab THEN DO:
    
                     IF es-ben-estab.compras-comprador-icms <> 0 AND TT-BENEFICIO.tp-tax = "ICMS" THEN DO:
                         FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-icms NO-LOCK NO-ERROR.
                         IF AVAIL es-mensagem-ben THEN DO:
                             run utp/ut-msgs.p (input "show",                                                                                              
                                               input 17006,                                                                                                      
                                               input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).
                         END.
                     END.
                     IF  es-ben-estab.compras-comprador-pis-confis <> 0  AND TT-BENEFICIO.tp-tax = "PIS" THEN DO:
    
                       
    
                          FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-pis-confis NO-LOCK NO-ERROR.   
                          IF AVAIL es-mensagem-ben THEN DO:                                                                                 
                              run utp/ut-msgs.p (input "show",                                                                              
                                                 input 17006,                                                                               
                                                 input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).                 
                          END.                                                                                                              
                     END.
    
                     CREATE ext-ordem-compra.                                                      
                     ASSIGN ext-ordem-compra.numero-ordem = ordem-compra.numero-ordem              
                            ext-ordem-compra.usuario      = c-seg-usuario                          
                            ext-ordem-compra.data         = TODAY                              
                            ext-ordem-compra.hora         = STRING(TIME,"hh:mm") 
                            ext-ordem-compra.cod-mensagem = es-mensagem-ben.cod-mensagem
                            ext-ordem-compra.it-codigo    = ordem-compra.it-codigo                    
                            ext-ordem-compra.mensagem     = es-mensagem-ben.narrativa.
                 END.
             END.
         END.
    

end.

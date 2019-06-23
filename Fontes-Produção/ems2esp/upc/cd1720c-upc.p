DEF INPUT PARAM p-ind-event        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object       AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object       AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame        AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table        AS ROWID         NO-UNDO.
DEF VAR l-pend AS LOGICAL INITIAL NO NO-UNDO.		

DEF VAR c-objeto AS CHAR NO-UNDO.

ASSIGN c-objeto   = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

IF p-ind-event  = "ASSIGN" AND
   p-ind-object = "VIEWER" THEN DO:

    FIND FIRST doc-pend-aprov WHERE
         ROWID(doc-pend-aprov) = p-row-table NO-LOCK NO-ERROR.
    
    ASSIGN l-pend = NO.

    IF AVAIL doc-pend-aprov AND 
       doc-pend-aprov.ind-tip-doc = 4 THEN DO: /*Pedido*/
        FIND FIRST ordem-compra WHERE 
                   ordem-compra.numero-ordem = doc-pend-aprov.numero-ordem NO-LOCK NO-ERROR.
        
        IF AVAIL ordem-compra THEN 
            RUN cdp/cdapi172.p (INPUT 4,
                                INPUT ROWID(ordem-compra),
                                OUTPUT l-pend).
    END.

    IF AVAIL doc-pend-aprov AND 
       doc-pend-aprov.ind-tip-doc = 8 THEN DO: /*Contrato*/
         FIND LAST item-contrat WHERE 
                   item-contrat.nr-contrato = doc-pend-aprov.nr-contrato NO-LOCK NO-ERROR.
         
        IF AVAIL item-contrat THEN 
            RUN cdp/cdapi172.p (INPUT 8,
                                INPUT ROWID(item-contrat),
                                OUTPUT l-pend).
    END.

    IF NOT l-pend THEN DO:
        IF AVAIL doc-pend-aprov AND 
           doc-pend-aprov.ind-tip-doc = 4 THEN DO: /*Pedido*/
            FIND FIRST proc_docum_ilha WHERE 
                       proc_docum_ilha.Tipo_Serv      = "O"                               AND
                       proc_docum_ilha.Nr_Doc         = STRING(doc-pend-aprov.num-pedido) EXCLUSIVE-LOCK NO-ERROR.
                
            IF AVAIL proc_docum_ilha THEN
                ASSIGN proc_docum_ilha.Status_Doc       = "A"
                       proc_docum_ilha.Obs              = doc-pend-aprov.narrativa-apr
                       proc_docum_ilha.log_atualizado   = YES.
        END.
    
       IF AVAIL doc-pend-aprov AND 
          doc-pend-aprov.ind-tip-doc = 8 THEN DO: /*Contrato*/
           FIND FIRST proc_docum_ilha WHERE 
                      proc_docum_ilha.Tipo_Serv      = "C"                                AND
                      proc_docum_ilha.Nr_Doc         = STRING(doc-pend-aprov.nr-contrato) EXCLUSIVE-LOCK NO-ERROR.
           
           IF AVAIL proc_docum_ilha THEN
               ASSIGN proc_docum_ilha.Status_Doc       = "A"
                      proc_docum_ilha.Obs              = doc-pend-aprov.narrativa-apr
                      proc_docum_ilha.log_atualizado   = YES.
       END.
    END.
END.

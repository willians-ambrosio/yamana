DEF INPUT PARAM p-ind-event        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object       AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object       AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame        AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table        AS ROWID         NO-UNDO.

DEF VAR c-objeto AS CHAR NO-UNDO.

ASSIGN c-objeto   = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

IF p-ind-event  = "ASSIGN" AND
   p-ind-object = "VIEWER" THEN DO:

    FIND FIRST doc-pend-aprov WHERE
         ROWID(doc-pend-aprov) = p-row-table NO-LOCK NO-ERROR.

    IF AVAIL doc-pend-aprov AND
       doc-pend-aprov.ind-tip-doc  = 4 AND 
       doc-pend-aprov.ind-situacao = 3 THEN DO: /*Pedido*/
        FIND FIRST proc_docum_ilha WHERE 
                   proc_docum_ilha.Tipo_Serv      = "O"                               AND
                   proc_docum_ilha.Nr_Doc         = STRING(doc-pend-aprov.num-pedido) AND 
                   proc_docum_ilha.log_atualizado = NO                                EXCLUSIVE-LOCK NO-ERROR.
            
        IF AVAIL proc_docum_ilha THEN
            ASSIGN proc_docum_ilha.Status_Doc       = "R"
                   proc_docum_ilha.Obs              = doc-pend-aprov.narrativa-rej
                   proc_docum_ilha.log_atualizado   = YES.
    END.

    IF AVAIL doc-pend-aprov AND
       doc-pend-aprov.ind-tip-doc  = 8 AND 
       doc-pend-aprov.ind-situacao = 3 THEN DO: /*Contrato*/
        FIND FIRST proc_docum_ilha WHERE 
                   proc_docum_ilha.Tipo_Serv      = "C"                                AND
                   proc_docum_ilha.Nr_Doc         = STRING(doc-pend-aprov.nr-contrato) AND 
                   proc_docum_ilha.log_atualizado = NO                                 EXCLUSIVE-LOCK NO-ERROR.
        
        IF AVAIL proc_docum_ilha THEN
            ASSIGN proc_docum_ilha.Status_Doc       = "R"
                   proc_docum_ilha.Obs              = doc-pend-aprov.narrativa-rej
                   proc_docum_ilha.log_atualizado   = YES.
    END.
END.


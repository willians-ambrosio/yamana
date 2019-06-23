  
FOR EACH nfe-nota-fiscal-rec EXCLUSIVE-LOCK:
    FIND FIRST emitente WHERE
               emitente.cgc = nfe-nota-fiscal-rec.cgc NO-LOCK NO-ERROR.

    IF AVAIL emitente THEN
    DO:
       ASSIGN nfe-nota-fiscal-rec.nome-abrev = emitente.nome-abrev.
    END.
END.

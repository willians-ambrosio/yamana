FOR EACH tipo-rec-desp NO-LOCK:
    CREATE es-tipo-rec-desp.
    ASSIGN es-tipo-rec-desp.tp-codigo = tipo-rec-desp.tp-codigo
           es-tipo-rec-desp.log-ativo = YES.
END.

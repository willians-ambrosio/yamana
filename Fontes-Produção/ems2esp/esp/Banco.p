def buffer banco for ems2cadme.banco.

DEF TEMP-TABLE tt-banco
    FIELD tt-cod-banco  LIKE banco.cod-banco
    FIELD tt-nome-banco LIKE banco.nome-banco.

FOR EACH banco NO-LOCK:
    FIND FIRST es_banco WHERE cod_banco = cod-banco NO-LOCK NO-ERROR.
        IF NOT AVAIL es_banco THEN DO:
            CREATE tt-banco.
            ASSIGN tt-cod-banco  = banco.cod-banco   
                   tt-nome-banco = banco.nome-banco.
    END.
END.

FOR EACH tt-banco NO-LOCK:
    CREATE es_banco.
    ASSIGN es_banco.cod_banco = tt-banco.tt-cod-banco
           es_banco.descricao = tt-banco.tt-nome-banco.    
END.

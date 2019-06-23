/*
FOR EACH es_funcionario:
    FIND FIRST usuar_mestre NO-LOCK
         WHERE dat_fim_valid > TODAY
           AND nom_usuario BEGINS nom_pessoa_fisic NO-ERROR.
    IF AVAIL usuar_mestre THEN
        FIND FIRST usuar-mestre-ext NO-LOCK
             WHERE usuar-mestre-ext.cod-usuario = usuar_mestre.cod_usuario NO-ERROR. 
        IF AVAIL usuar-mestre-ext THEN
            ASSIGN es_funcionario.login_ad = usuar-mestre-ext.cod-usuar-so.
END.
*/



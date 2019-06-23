ON WRITE OF FUNCIONARIO OVERRIDE DO:
END.
ON WRITE OF FUNC_CCUSTO OVERRIDE DO:
END.

DEF VAR v_cdn_func      LIKE funcionario.cdn_funcionario        NO-UNDO.
DEF VAR v_cdn_estab     LIKE funcionario.cdn_estab              NO-UNDO.
DEF VAR v_cod_rh_ccusto AS CHAR LABEL 'Novo Centro de Custo'    NO-UNDO.
DEF VAR v_temp          AS CHAR                                 NO-UNDO.

UPDATE v_cdn_estab v_cdn_func v_cod_rh_ccusto.

FIND FIRST FUNCIONARIO EXCLUSIVE-LOCK
     WHERE funcionario.cdn_estab = v_cdn_estab
       AND funcionario.cdn_funcionario = v_cdn_func NO-ERROR.
IF AVAIL funcionario THEN DO:
    FIND LAST FUNC_CCUSTO EXCLUSIVE-LOCK 
        WHERE func_ccusto.cdn_estab = funcionario.cdn_estab
          AND func_ccusto.cdn_funcionario = funcionario.cdn_funcionario NO-ERROR.
    IF AVAIL func_ccusto THEN DO:
        ASSIGN v_temp                    = func_ccusto.cod_rh_ccusto
               FUNC_CCUSTO.COD_RH_CCUSTO = v_cod_rh_ccusto
               FUNCIONARIO.COD_RH_CCUSTO = v_cod_rh_ccusto.
        MESSAGE 'O Funcion rio abaixo teve o Centro de Custo alterado.' SKIP
                'Funcionario: 'funcionario.nom_pessoa_fisic SKIP 
                '   De Centro de Custo: ' v_temp SKIP
                '   Para Centro de Custo: ' func_ccusto.cod_rh_ccusto
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
        

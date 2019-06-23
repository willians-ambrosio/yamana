FUNCTION fn_retorna_ultimo_dia_mes RETURN DATE (INPUT p_mes AS INT,
                                                    INPUT p_ano AS INT):
    DEF VAR v_dat_retorno AS DATE NO-UNDO.
    
    CASE p_mes:
        WHEN 1 OR WHEN 3 OR WHEN 5  OR
        WHEN 7 OR WHEN 8 OR WHEN 10 OR WHEN 12 THEN
            ASSIGN v_dat_retorno = DATE(p_mes,31,p_ano).
        WHEN 4  OR WHEN 6  OR
        WHEN 9  OR WHEN 11 THEN
            ASSIGN v_dat_retorno = DATE(p_mes,30,p_ano).
        WHEN 2 THEN /* Verifica se o ano ‚ bissexto */
            ASSIGN v_dat_retorno = IF p_ano MODULO 4 = 0 THEN DATE(p_mes,29,p_ano)
                                   ELSE DATE(p_mes,28,p_ano).
    END CASE.
    RETURN v_dat_retorno.
END.

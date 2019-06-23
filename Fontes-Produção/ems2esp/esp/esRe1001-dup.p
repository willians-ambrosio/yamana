/*---------------------------------------------------------------------------
Programa: esRe1001-dup.p
Objetivo: Eliminar e Recriar  a duplicata a pagar para rec de servi‡os.
Autor   : Joao B. C. Bisneto
-----------------------------------------------------------------------------*/
DEF INPUT PARAM p-docum-est               AS ROWID NO-UNDO.
DEF INPUT PARAM p-acao                    AS CHAR FORMAT "X(03)" NO-UNDO.
/*---------------------------------------------------------------------------*/
FIND FIRST docum-est
  NO-LOCK
  WHERE ROWID(docum-est) = p-docum-est
  NO-ERROR.
/*---------------------------------------------------------------------------*/
IF p-acao = "DEL" THEN
  DO:
    FOR EACH dupli-apagar 
      OF docum-est
      EXCLUSIVE-LOCK:
        FOR EACH dupli-imp 
          OF dupli-apagar
          EXCLUSIVE-LOCK:
            ASSIGN dupli-imp.nro-docto = "xyz" + dupli-imp.nro-docto.
          END. /* FOR EACH dupli-imp OF dupli-apagar: */
        ASSIGN dupli-apagar.nro-docto = "xyz" + dupli-apagar.nro-docto.
      END. /* FOR EACH dupli-apagar OF docum-est: */
  END. /* IF p-acao = "DEL" THEN */
/*---------------------------------------------------------------------------*/
IF p-acao = "ADD" THEN
  DO:
    FOR EACH dupli-apagar
      WHERE dupli-apagar.serie-docto  = docum-est.serie-docto  
      AND   dupli-apagar.nro-docto    = "xyz" + docum-est.nro-docto    
      AND   dupli-apagar.cod-emitente = docum-est.cod-emitente 
      AND   dupli-apagar.nat-operacao = docum-est.nat-operacao 
      AND   dupli-apagar.parcela      = "1":
        /*
        FOR EACH dupli-imp
          WHERE dupli-imp.serie-docto  = dupli-apagar.serie-docto  
          AND   dupli-imp.nro-docto    = "xyz" + dupli-apagar.nro-docto    
          AND   dupli-imp.cod-emitente = dupli-apagar.cod-emitente 
          AND   dupli-imp.nat-operacao = dupli-apagar.nat-operacao 
          AND   dupli-imp.parcela      = dupli-apagar.parcela      
          AND   dupli-imp.cod-esp      = dupli-apagar.cod-esp:
        */
        FOR EACH dupli-imp 
          OF dupli-apagar:
            ASSIGN dupli-imp.nro-docto = REPLACE(dupli-imp.nro-docto,"xyz","").     
          END. /* FOR EACH dupli-imp */
        ASSIGN dupli-apagar.nro-docto = REPLACE(dupli-apagar.nro-docto,"xyz","").
      END. /* FOR EACH dupli-apagar */
  END. /* IF p-acao = "ADD" THEN */

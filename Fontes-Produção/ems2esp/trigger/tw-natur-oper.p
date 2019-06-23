{include/i-prgvrs.i tw-natur-oper 2.06.00.000}

/* Variaveis de Par³metros */ 
DEF PARAMETER BUFFER p-table     FOR natur-oper.
DEF PARAMETER BUFFER p-old-table FOR natur-oper.

{UTP/UT-GLOB.I}

IF NEW p-table AND p-table.terceiros THEN DO:
   IF NOT CAN-FIND(FIRST es-conta-cfop 
                   WHERE es-conta-cfop.nat-operacao = p-table.nat-operacao
                   NO-LOCK) THEN DO:
      RUN pi-cria-cfopxconta.
   END.
END.
ELSE DO:
   IF p-table.terceiros THEN DO:
      IF NOT CAN-FIND(FIRST es-conta-cfop 
                      WHERE es-conta-cfop.nat-operacao = p-table.nat-operacao
                      NO-LOCK) THEN DO:
         RUN pi-cria-cfopxconta.
      END.
   END.
   ELSE DO:
      FIND FIRST es-conta-cfop 
           WHERE es-conta-cfop.nat-operacao = p-table.nat-operacao
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(es-conta-cfop) THEN DO:
         DELETE es-conta-cfop.
      END.
   END.
END.

IF i-ep-codigo-usuario <> "201" THEN NEXT.

IF AVAIL p-table THEN DO:
  IF NOT CAN-FIND(FIRST ext-natur-oper WHERE
                  ext-natur-oper.nat-operacao = p-table.nat-operacao) THEN DO:
    CREATE ext-natur-oper.
    ASSIGN ext-natur-oper.nat-operacao  = p-table.nat-operacao
           ext-natur-oper.gera-des      = NO.
  END.
END.

PROCEDURE pi-cria-cfopxconta:
   CREATE es-conta-cfop.
   ASSIGN es-conta-cfop.nat-operacao      = p-table.nat-operacao
          es-conta-cfop.usuar-atualizacao = c-seg-usuario
          es-conta-cfop.data-atual        = TODAY
          es-conta-cfop.hora-atual        = STRING(TIME,"hh:mm:ss").
END PROCEDURE.


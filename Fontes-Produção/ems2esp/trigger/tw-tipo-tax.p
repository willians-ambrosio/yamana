{include/i-prgvrs.i tw-tipo-tax 2.06.00.000}

/* Variaveis de Par³metros */ 
DEF PARAMETER BUFFER p-table     FOR tipo-tax.
DEF PARAMETER BUFFER p-old-table FOR tipo-tax.

{UTP/UT-GLOB.I}

IF i-ep-codigo-usuario <> "201" THEN
    NEXT.

IF AVAIL p-table THEN DO:
  IF NOT CAN-FIND(FIRST mgesp.ext-tipo-tax
                  WHERE mgesp.ext-tipo-tax.cod-tax = p-table.cod-tax) THEN DO:
    CREATE mgesp.ext-tipo-tax.
    ASSIGN mgesp.ext-tipo-tax.cod-tax     = p-table.cod-tax
           mgesp.ext-tipo-tax.gera-des    = NO.
  END.
END.


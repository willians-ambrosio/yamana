{include/i-prgvrs.i td-tipo-tax 2.06.00.000}

DEF PARAMETER BUFFER p-table FOR tipo-tax.

{UTP/UT-GLOB.I}

IF i-ep-codigo-usuario <> "201" THEN NEXT.

IF AVAIL p-table THEN DO:
  FOR FIRST mgesp.ext-tipo-tax EXCLUSIVE-LOCK
      WHERE mgesp.ext-tipo-tax.cod-tax = p-table.cod-tax.
    DELETE mgesp.ext-tipo-tax.
  END.
END.


/*------------------------------------------------------------------------------------------------------------
  Objetivo: Trigger de Write da Tabela contrato-for
  Autor: Gilberto Rissati Garcia / GWA
  Data: 13/10/2003
  ------------------------------------------------------------------------------------------------------------*/

def param buffer p-contrato-for     for contrato-for.
def param buffer p-old-contrato-for for contrato-for.

/*
MESSAGE "PROGRAM-NAME(1)" PROGRAM-NAME(1) SKIP
        "PROGRAM-NAME(2)" PROGRAM-NAME(2) SKIP
        "PROGRAM-NAME(3)" PROGRAM-NAME(3) SKIP
        "PROGRAM-NAME(4)" PROGRAM-NAME(4) SKIP
        "PROGRAM-NAME(5)" PROGRAM-NAME(5) SKIP
        "PROGRAM-NAME(6)" PROGRAM-NAME(6) SKIP
        "PROGRAM-NAME(7)" PROGRAM-NAME(7) SKIP
        "PROGRAM-NAME(8)" PROGRAM-NAME(8) SKIP
 VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

IF R-INDEX(STRING(PROGRAM-NAME(3)),"cn0401rp") > 0 AND 
   R-INDEX(STRING(PROGRAM-NAME(4)),"cn0401") > 0 THEN DO:
   ASSIGN p-contrato-for.impr-contrat = YES.
END.


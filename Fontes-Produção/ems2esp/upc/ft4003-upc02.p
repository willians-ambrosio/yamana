/*-----------------------------------------------------------------------------------
    PROGRAMA : ft4003-upc01.p
    OBJETIVO : Chamanda de UPC do ft4003
    AUTOR    : Wellington Aparecido  (DSC)
    DATA     : 25/08/2008
-----------------------------------------------------------------------------------*/
/*************************************************************************************
                                    VARIAVEIS GLOBAIS
*************************************************************************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft4003-upc01-serie             AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft4003-upc01-cod-estabel       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft4003-upc01-btOk              AS WIDGET-HANDLE NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/

FIND FIRST mgesp.ext-ser-estab NO-LOCK 
  WHERE mgesp.ext-ser-estab.cod-estabel = wh-ft4003-upc01-cod-estabel:SCREEN-VALUE 
  AND   mgesp.ext-ser-estab.serie       = wh-ft4003-upc01-serie:SCREEN-VALUE  
  AND   mgesp.ext-ser-estab.log-gera-nfe  NO-ERROR.
IF NOT AVAIL mgesp.ext-ser-estab THEN DO:
  RUN utp/ut-msgs.p (INPUT "show":U, 
                     INPUT 17006, 
                     INPUT "Serie Invalida" + "~~" +
                     "SÇrie n∆o parametrizada para gerar NFe.":u).
  RETURN "NOK":U.    
END.
ELSE 
  APPLY "choose" TO wh-ft4003-upc01-btOk. 

 

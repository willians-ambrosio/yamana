&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*****************************************************************************
**
**       PROGRAMA: AB9000.i
**
**       DATA....: Julho de 2003
**
**       AUTOR...: Marcio Willwock - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Procedures Gen‚ricas de Frotas
**
*****************************************************************************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 8.17
         WIDTH              = 35.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscaFormatoConta Include 
PROCEDURE buscaFormatoConta :
/*------------------------------------------------------------------------------
  Purpose:     buscaFormatoConta
  Parameters:  sa¡da pConta = Conta
               sa¡da pSub   = Sub Conta
  Notes:       Busca nos parƒmetros globais os formatos para conta e sub-conta
------------------------------------------------------------------------------*/
define output parameter pConta as character no-undo.
define output parameter pSub   as character no-undo.

define buffer bfParam for param-global.

/** Busca parƒmetros globais **/
for first bfParam fields(ct-format sc-format) no-lock:
    assign pConta = bfParam.ct-format
           pSub   = bfParam.sc-format.
end.

/** Como nos parƒmetros o formato ‚ colocado (99999), 
    ‚ feito a contagem dos caracteres e colocado o 
    formato caracter **/
assign pConta = "x(":U + string(length(pConta)) + ")":U
       pSub   = "x(":U + string(length(pSub))   + ")":U.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE converteInvertidaParaNormal Include 
PROCEDURE converteInvertidaParaNormal :
/*------------------------------------------------------------------------------
  Purpose:     converteInvertidaParaNormal
  Parameters:  entrada pInvertida = Valor Decimal da data e hora informada
               sa¡da   pData      = Data de apontamento
               sa¡da   pHora      = Hora de apontamento
  Notes:       Converte a data invertida doapontamento para data e hora normais
------------------------------------------------------------------------------*/
define input  parameter pInvertida as decimal   format "999999999999" no-undo.
define output parameter pData      as date      format "99/99/9999"   no-undo.
define output parameter pHora      as character format "999999"       no-undo.

assign pData = date(integer(substring(string(pInvertida),5,2)),  /** Mˆs **/
                    integer(substring(string(pInvertida),7,2)),  /** Dia **/
                    integer(substring(string(pInvertida),1,4))). /** Ano **/

assign pHora = substring(string(pInvertida),9,4) /** Hora + Minuto **/
               + "00":U.                         /** Segundos **/

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE converteParaHoraInvertida Include 
PROCEDURE converteParaHoraInvertida :
/*------------------------------------------------------------------------------
  Purpose:     converteParaHoraInvertida
  Parameters:  entrada pData      = Data de apontamento
               entrada pHora      = Hora de apontamento
               sa¡da   pInvertida = Valor Decimal da data e hora informada
  Notes:       Converte a data e hora de apontamento para o campo hora invertida
------------------------------------------------------------------------------*/
define input  parameter pData      as character format "99/99/9999"   no-undo.
define input  parameter pHora      as character format "999999"       no-undo.
define output parameter pInvertida as decimal   format "999999999999" no-undo.

assign pInvertida = decimal(trim(substring(pData,7,4)    /** Ano **/
                               + substring(pData,4,2)    /** Mˆs **/
                               + substring(pData,1,2)    /** Dia **/
                               + substring(pHora,1,2)    /** Hora **/
                               + substring(pHora,3,2))). /** Minuto **/

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFormataHoraParaSegundo Include 
PROCEDURE piFormataHoraParaSegundo :
/*------------------------------------------------------------------------------
  Purpose:     piFormataHoraParaSegundo
  Parameters:  entrada pHora    = Hora
               sa¡da   pSegundo = Segundos
  Notes:       Informar as horas para que seja retornado os segundos
------------------------------------------------------------------------------*/
define input  parameter pHora    as character format "99:99:99"   no-undo.
define output parameter pSegundo as integer   format ">>,>>>,>>9" no-undo.

assign pSegundo = integer(substring(pHora,1,2)) * 3600
                + integer(substring(pHora,3,2)) * 60
                + integer(substring(pHora,5,2)).

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFormataHorarioMin Include 
PROCEDURE piFormataHorarioMin :
/*------------------------------------------------------------------------------
  Purpose:     piFormataHorarioMin
  Parameters:  Entrada - Sa¡da pHora = Hor rio
  Notes:       Verifica se a hora informada est  correta.
------------------------------------------------------------------------------*/
define input-output parameter pHora as character format "99:99" no-undo.

/** Verifica se hor rio passado est  informado por 
    completo e inclui zero no final **/
IF LENGTH(pHora) > 0 THEN
    ASSIGN pHora = REPLACE(pHora," ","0").
IF LENGTH(pHora) < 5 THEN
    ASSIGN pHora = pHora + "00".
ELSE
    IF LENGTH(pHora) = 5 THEN
        ASSIGN pHora = pHora + "0".

/** Se estiver informado um hor rio fora do normal,
    mostra mensagem de erro Datasul **/
if int(substr(pHora,4,2)) > 59 or
   int(substr(pHora,1,2)) > 23 then do:
    run utp/ut-msgs.p (input "show":U, 
                       input 29520,
                       input "").
    return "NOK":U.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFormataHorarioSeg Include 
PROCEDURE piFormataHorarioSeg :
/*------------------------------------------------------------------------------
  Purpose:     piFormataHorarioSeg
  Parameters:  Entrada - Sa¡da pHora = Hor rio
  Notes:       Verifica se a hora informada est  correta.
------------------------------------------------------------------------------*/
define input-output parameter pHora as character format "99:99:99" no-undo.

/** Verifica se hor rio passado est  informado por 
    completo e inclui zero no final **/
IF LENGTH(pHora) > 0 THEN
    ASSIGN pHora = REPLACE(pHora," ","0").
IF LENGTH(pHora) < 7 THEN
    ASSIGN pHora = pHora + "00".
ELSE
    IF LENGTH(pHora) = 7 THEN
        ASSIGN pHora = pHora + "0".

/** Se estiver informado um hor rio fora do normal,
    mostra mensagem de erro Datasul **/
if int(substr(pHora,4,2)) > 59 or
   int(substr(pHora,1,2)) > 23 or 
   int(substr(pHora,7,2)) > 59 then do:
    run utp/ut-msgs.p (input "show":U, 
                       input 4786, 
                       input "").
    return "NOK":U.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFormataSegundoParaHora Include 
PROCEDURE piFormataSegundoParaHora :
/*------------------------------------------------------------------------------
  Purpose:     piFormataSegundoParaHora
  Parameters:  entrada pSegundo = Segundos
               sa¡da   pHora    = Horas
  Notes:       Informar os segundos (time) para que seja retornado a hora
------------------------------------------------------------------------------*/
define input  parameter pSegundo  as integer   format ">>,>>>,>>9" no-undo.
define output parameter pHora     as character format "99:99:99"   no-undo.

assign pHora = replace(string(pSegundo,"hh:mm:ss":U),":","").

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


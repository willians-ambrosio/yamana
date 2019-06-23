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
**       PROGRAMA: MV9000.i
**
**       DATA....: Janeiro de 2004
**
**       AUTOR...: Petter Frank Venancio - F†brica Software - DATASUL S.A.
**                 Marcio Willwock - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Convers∆o de Hor†rio Normal para Decimal
**
*****************************************************************************/

/* ***************************  Definitions  ************************** */

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
         HEIGHT             = 5.63
         WIDTH              = 35.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

{abp/abapi001.i2} /** Criaá∆o de Erros **/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscaPeriodoAberto Include 
PROCEDURE buscaPeriodoAberto :
/*------------------------------------------------------------------------------
  Purpose:     buscaPeriodoAberto
  Parameters:  entrada pEstabel       = C¢digo do Estabelecimento
               sa°da   da-iniper-aber = Data Inicial Per°odo em Aberto
               sa°da   da-fimper-aber = Data Final Per°odo em Aberto
               sa°da   i-per-corrente = Per°odo Corrente em Aberto
               sa°da   i-ano-corrente = Ano do Per°odo Corrente em Aberto
               sa°da   da-iniper-fech = Data Inicial Èltimo Per°odo Fechado
               sa°da   da-fimper-fech = Data Final Èltimo Per°odo Fechado
  Notes:       Busca o per°odo em aberto no C†lculo do Fechamento do Estoque do
               EMS.
------------------------------------------------------------------------------*/
define input  parameter pEstabel       as   character                no-undo.
define output parameter da-iniper-aber like param-estoq.ult-fech-dia no-undo.
define output parameter da-fimper-aber like param-estoq.ult-fech-dia no-undo.
define output parameter i-per-corrente as   integer                  no-undo.
define output parameter i-ano-corrente as   integer                  no-undo.
define output parameter da-iniper-fech like param-estoq.ult-fech-dia no-undo.
define output parameter da-fimper-fech like param-estoq.ult-fech-dia no-undo.

define variable lEstabelecimento as logical   no-undo.

for first param-global fields(modulo-ce modulo-cs) no-lock:
    if param-global.modulo-cs and param-global.modulo-ce then do:
        /** Verificaá∆o do Per°odo aberto **/
        for first param-estoq fields(tp-fech ult-per-fech) no-lock:
            /** Verifica Tipo Fechamento **/
            if param-estoq.tp-fech = 2 then
                assign lEstabelecimento = yes.
            /** Fechamento Ènico **/
            if not lEstabelecimento then do:
               run cdp/cdapi005.p (input  param-estoq.ult-per-fech,
                                   output da-iniper-aber,
                                   output da-fimper-aber,
                                   output i-per-corrente,
                                   output i-ano-corrente,
                                   output da-iniper-fech,
                                   output da-fimper-fech).
            end.
        end.
        /** Fechamento por Estabelecimento **/
        if lEstabelecimento then do:
            if not avail estab-mat or estab-mat.cod-estabel <> pEstabel then
                for first estab-mat fields(cod-estabel ult-per-fech)
                    where estab-mat.cod-estabel = pEstabel no-lock:
                end.
            /** N∆o existe estabelecimento material para estabelecimento **/
            if not avail estab-mat then do:
                run piCriaErro in this-procedure (input 29438,
                                                  input "EMS":U,
                                                  input pEstabel).
                return "NOK":U.
            end.
            else do:
                run cdp/cdapi005.p (input  estab-mat.ult-per-fech,
                                    output da-iniper-aber,
                                    output da-fimper-aber,
                                    output i-per-corrente,
                                    output i-ano-corrente,
                                    output da-iniper-fech,
                                    output da-fimper-fech).
            end.
        end.
    end.
    /** Quando retorna "NESTOQ", Ç para n∆o validar datas de fechamento, 
        pois o Estoque n∆o est† implantado **/
    else
        return "NESTOQ":U.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE converteDecimalparaNormal Include 
PROCEDURE converteDecimalparaNormal :
/*------------------------------------------------------------------------------
  Purpose:     converteDecimalparaNormal
  Parameters:  entrada pDecimal = Hor†rio decimal
               sa°da   pNormal  = Hor†rio normal
  Notes:       Converte hor†rio decimal para hor†rio normal
------------------------------------------------------------------------------*/
define input  parameter pDecimal as decimal   format ">>9.99"   no-undo.
define output parameter pNormal  as character format "99:99:99" no-undo.

assign pNormal  = substring(string(pDecimal,"99.99"),1,2) +                             /** Hora **/
                  string(decimal(substring(string(pDecimal,"99.99"),4,2)) * 0.6,"99") + /** Minuto **/
                  "00".                                                                 /** Segungo **/

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE converteNormalparaDecimal Include 
PROCEDURE converteNormalparaDecimal :
/*------------------------------------------------------------------------------
  Purpose:     converteNormalparaDecimal
  Parameters:  entrada pNormal  = Hor†rio normal
               sa°da   pDecimal = Hor†rio decimal
  Notes:       Converte hor†rio normal para hor†rio decimal
------------------------------------------------------------------------------*/
define input  parameter pNormal  as character format "99:99:99" no-undo.
define output parameter pDecimal as decimal   format ">>9.99"   no-undo.

assign pDecimal = decimal(substring(pNormal,1,2)) +      /** Hora **/
                 (decimal(substring(pNormal,3,2)) / 60). /** Minuto **/

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
*****************************************************************************
**
**       PROGRAMA: ABAPI001.i3
**
**       DATA....: Julho de 2003
**
**       AUTOR...: Marcio Willwock - Manufatura - DATASUL S.A.
**
**       OBJETIVO: API para Atualiza‡Æo de Apontamentos de Abastecimento e
**                 Lubrifica‡Æo
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
         HEIGHT             = 4.63
         WIDTH              = 26.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piKilometragem Include 
PROCEDURE piKilometragem :
/*------------------------------------------------------------------------------
  Purpose:     piKilometragem
  Parameters:  entrada pDocto  = Documento Origem (MAB ou PNP)
               entrada pStatus = Status do Abastecimento 
               entrada pOrigem = Origem do documento
               entrada pLog    = Abastecimento prim rio?
  Notes:       Cria‡Æo do registro de Hist¢rico Kilometragem
------------------------------------------------------------------------------*/
define input parameter pDocto  as integer no-undo.
define input parameter pStatus as integer no-undo.
define input parameter pOrigem as integer no-undo.
define input parameter pLog    as logical no-undo.

DEFINE VARIABLE hDBOKM AS HANDLE NO-UNDO.

DEFINE VARIABLE deValKm LIKE mab-movto-km-eqpto.val-km-real NO-UNDO.
/** Documentos consistidos sem erros **/
if pStatus > 20 then do:
    if not can-find(first mab-movto-km-eqpto
                    where mab-movto-km-eqpto.num-docto            = pDocto
                    and   mab-movto-km-eqpto.idi-origem           = pOrigem 
                    and   mab-movto-km-eqpto.ep-codigo            = ttDocumento.ep-codigo
                    and   mab-movto-km-eqpto.cod-eqpto            = ttDocumento.cod-eqpto no-lock) and
       not can-find(first mab-movto-km-eqpto
                    where mab-movto-km-eqpto.ep-codigo            = ttDocumento.ep-codigo
                    and   mab-movto-km-eqpto.cod-eqpto            = ttDocumento.cod-eqpto
                    and   mab-movto-km-eqpto.val-dat-hora-invrtda = ttDocumento.val-dat-hora-invrtda no-lock)
                    then do:
        empty temp-table ttKM.
        /*--- BO Kilometragem ---*/
        IF NOT VALID-HANDLE(hDBOKM) OR
           hDBOKM:TYPE <> "PROCEDURE":U OR
           hDBOKM:FILE-NAME <> "frbo/bofr060.p":U THEN DO:
            run frbo/bofr060.p persistent set hDBOKM.
        END.
        RUN openQueryStatic IN hDBOKM (input "Main":U) NO-ERROR.
        for last  mab-movto-km-eqpto fields(cod-eqpto val-km-real dat-movto val-livre-2) USE-INDEX mbmvtkmq-id
            where mab-movto-km-eqpto.ep-codigo            =  ttDocumento.ep-codigo
            and   mab-movto-km-eqpto.cod-eqpto            =  ttDocumento.cod-eqpto
            and   mab-movto-km-eqpto.val-dat-hora-invrtda <  ttDocumento.val-dat-hora-invrtda
            and   mab-movto-km-eqpto.val-km-real          <> 0 no-lock:
            /** Vida Anterior **/
            assign deValKm = mab-movto-km-eqpto.val-km-real.
        END.
        create ttKM.
        assign ttKM.ep-codigo            = ttDocumento.ep-codigo
               ttKM.cod-eqpto            = ttDocumento.cod-eqpto
               ttKM.val-dat-hora-invrtda = ttDocumento.val-dat-hora-invrtda
               ttKM.dat-movto            = ttDocumento.dat-movto
               ttKM.hra-inicial          = ttDocumento.hra-inicial
               ttKM.idi-origem           = pOrigem
               ttKM.num-docto            = ttDocumento.num-docto
               ttKM.log-abastec-prim     = pLog
               ttKM.val-hodom-horim      = ttDocumento.val-hodom-horim
               ttKM.val-livre-1          = ttDocumento.val-hodom-horim-sec /* Referente ao campo val-hodom-horim-sec (Valida‡Æo Contador Secund rio) */
               ttKM.val-km-real          = ttDocumento.val-km-percur + deValKm
               ttKM.val-livre-2          = 0. /* Referente ao campo val-km-sec (Valida‡Æo Contador Secund rio) */
        /** Limpa temp-tables de erros **/
        run emptyRowErrors in hDBOKM.               
        run criaKM in hDBOKM (input table ttKM).
        if return-value = "NOK":U then do:
            /** Busca Erros da BO **/
            run getRowErrors in hDBOKM (output table RowErrors).
            /** Limpa temp-tables de erros **/
            run emptyRowErrors in hDBOKM.
            /** Kilometragem **/
            if valid-handle(hDBOKM) then
                run destroy in hDBOKM.
            return "NOK":U.        
        end.
        /** Kilometragem **/
        if valid-handle(hDBOKM) then
            run destroy in hDBOKM.
        empty temp-table ttKM.
    end.
    else do:
        /** Valida se ‚ abastecimento / lubrifica‡Æo **/
        if pOrigem = 1 then do:
            /*****************************************************************************************
            ** Documentos de abastecimento/lubrifica‡Æo tˆm prioridade, para o vencimento de 
            ** planos de manuten‡Æo e de lubrifica‡äes, deve-se alterar a origem da quilometragem 
            *****************************************************************************************/
            for first mab-movto-km-eqpto
                where mab-movto-km-eqpto.ep-codigo            = ttDocumento.ep-codigo           
                and   mab-movto-km-eqpto.cod-eqpto            = ttDocumento.cod-eqpto            
                and   mab-movto-km-eqpto.val-dat-hora-invrtda = ttDocumento.val-dat-hora-invrtda exclusive-lock:
                assign mab-movto-km-eqpto.idi-origem           = 1
                       mab-movto-km-eqpto.num-docto            = pDocto
                       mab-movto-km-eqpto.val-dat-hora-invrtda = ttDocumento.val-dat-hora-invrtda
                       mab-movto-km-eqpto.dat-movto            = ttDocumento.dat-movto           
                       mab-movto-km-eqpto.hra-inicial          = ttDocumento.hra-inicial         
                       mab-movto-km-eqpto.val-hodom-horim      = ttDocumento.val-hodom-horim
                       mab-movto-km-eqpto.val-livre-1          = ttDocumento.val-hodom-horim-sec /* Referente ao campo val-hodom-horim-sec (Valida‡Æo Contador Secund rio)*/
                       mab-movto-km-eqpto.log-abastec-prim     = pLog.
            end.        
        end.
        else do:
            /*****************************************************************************************
            ** Documentos j  existentes de outras origens, devem apenas ser alterados 
            *****************************************************************************************/
            for first mab-movto-km-eqpto
                where mab-movto-km-eqpto.num-docto  = pDocto
                and   mab-movto-km-eqpto.idi-origem = pOrigem exclusive-lock:
                assign mab-movto-km-eqpto.val-dat-hora-invrtda = ttDocumento.val-dat-hora-invrtda
                       mab-movto-km-eqpto.dat-movto            = ttDocumento.dat-movto           
                       mab-movto-km-eqpto.hra-inicial          = ttDocumento.hra-inicial         
                       mab-movto-km-eqpto.val-hodom-horim      = ttDocumento.val-hodom-horim
                       mab-movto-km-eqpto.val-livre-1          = ttDocumento.val-hodom-horim-sec. /* Referente ao campo val-hodom-horim-sec (Valida‡Æo Contador Secund rio)*/
            end.
        end.
    end.
end.
/** Documentos com erros **/
else do:
    empty temp-table ttKM.
    /** Cria Temp-table para elimina‡Æo do hist¢rico de KM **/
    for each  mab-movto-km-eqpto
        where mab-movto-km-eqpto.num-docto  = pDocto
        and   mab-movto-km-eqpto.idi-origem = pOrigem no-lock:
        create ttKM.
        buffer-copy mab-movto-km-eqpto to ttKM
            assign ttKM.r-rowid = rowid(mab-movto-km-eqpto).
    end.
    if can-find(first ttKM no-lock) then do:
        /*--- BO Kilometragem ---*/
        IF NOT VALID-HANDLE(hDBOKM) OR
           hDBOKM:TYPE <> "PROCEDURE":U OR
           hDBOKM:FILE-NAME <> "frbo/bofr060.p":U THEN DO:
            run frbo/bofr060.p persistent set hDBOKM.
        END.
        RUN openQueryStatic IN hDBOKM (input "Main":U) NO-ERROR.
        /** Limpa temp-tables de erros **/
        run emptyRowErrors in hDBOKM.        
        /** Elimina a Quilometragem **/
        run deleteKM in hDBOKM (input table ttKM).
        if return-value = "NOK":U then do:
            /** Busca Erros da BO **/
            run getRowErrors in hDBOKM (output table RowErrors).
            /** Limpa temp-tables de erros **/
            run emptyRowErrors in hDBOKM.
            /** Kilometragem **/
            if valid-handle(hDBOKM) then
                run destroy in hDBOKM.
            return "NOK":U.
        end.
        /** Kilometragem **/
        if valid-handle(hDBOKM) then
            run destroy in hDBOKM.
    end.
    empty temp-table ttKM.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


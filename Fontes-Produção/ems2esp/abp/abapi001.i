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
**       PROGRAMA: ABAPI001.i
**
**       DATA....: Junlho de 2003
**
**       AUTOR...: Marcio Willwock - Manufatura - DATASUL S.A.
**
**       OBJETIVO: API para Atualizaá∆o de Apontamentos de Abastecimento e
**                 Lubrificaá∆o
**
*****************************************************************************/

/*--- Temporary Table Definitions ---*/
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field dataIni          as date
    field dataFim          as date
    field iEmpIni          like mab-eqpto.ep-codigo
    field iEmpFim          like mab-eqpto.ep-codigo
    field cEstabIni        like mab-histor-ativid.cod-estab
    field cEstabFim        like mab-histor-ativid.cod-estab
    field cEqptoIni        like mab-eqpto.cod-eqpto
    field cEqptoFim        like mab-eqpto.cod-eqpto
    field cTagIni          like mab-histor-ativid.cd-tag
    field cTagFim          like mab-histor-ativid.cd-tag
    field cCCini           like mab-histor-ativid.cc-codigo
    field cCCfim           like mab-histor-ativid.cc-codigo
    field cPostoIni        like mab-abastec-lubrific.cod-posto
    field cPostoFim        like mab-abastec-lubrific.cod-posto
    field num-docto        like mab-abastec-lubrific.num-docto initial ?
    field iConsiste        as integer     /** 1 = Somente com erros - 2 = Todos **/
    field descConsiste     as char format "x(20)":U
    field lFicha           as logical
    field tipo-liberacao   as integer     /** 1 = Normal - 2 = Padr∆o - 3 = Usu†rio **/
    &IF DEFINED(bf_mnt_ems206b) &THEN
    FIELD cUnidNegocIni    AS CHARACTER FORMAT "x(03)":U
    FIELD cUnidNegocFim    AS CHARACTER FORMAT "x(03)":U
    &ENDIF
    .

/** Utilizada pela API ABAPI002 **/
define temp-table ttDocLib no-undo
    field num-docto          like mab-abastec-lubrific.num-docto
    field consumo-padrao     as decimal
    field consumo-padrao-sec as decimal
    field consumo-real       as decimal
    field consumo-real-sec   as decimal /*(Validaá∆o contador Secund†rio)*/
    field uso-real           as decimal
    field uso-real-sec       as DECIMAL /*(Validaá∆o contador Secund†rio)*/
    field tp-consumo         as integer
    field tp-consumo-sec     as integer /*(Validaá∆o contador Secund†rio)*/
    field qtd-abast          as decimal
    field log-abast          as logical
    field idi-consist        as integer
    field r-rowid            as rowid
    field log-primario       as logical
    index documento num-docto.

/*****************************************************************************/
                                                         /** Fim da Include **/

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
         HEIGHT             = 15.83
         WIDTH              = 41.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



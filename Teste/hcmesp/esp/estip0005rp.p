&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

{include/i-prgvrs.i ESTIP0005RP 2.12.00.004}

/*----- TEMP-TABLE DE PARAMETROS -----*/
{esp/estip0005tt.i}    

/*---- DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.


/* ***************************  Definitions  ************************** */
def var v_han_acomp         as handle no-undo. 

DEFINE VARIABLE chExcel AS COMPONENT-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
create tt-param.
raw-transfer raw-param to tt-param.


RUN pi-exporta.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-exporta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exporta Procedure 
PROCEDURE pi-exporta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN tt-param.arquivo = replace(replace(REPLACE(tt-param.arquivo,'.lst','.csv'),'txt','.csv'),'.tmp','.csv').

RUN utp/ut-acomp.p PERSISTENT SET v_han_acomp.
RUN pi-inicializar IN v_han_acomp('Processando').

output to value(tt-param.arquivo) convert target 'ISO8859-1'.

IF tt-param.idi_tip_relat = 2 THEN
    PUT UNFORMATTED
        'Empresa;Estabelecimento;Matr¡cula;Nome;Centro de Custo;Descri‡Æo;Per¡odo;Cargo;Descri‡Æo;Sal rio;TIP;Percentual;Avos;In¡cio;T‚rmino' SKIP.
ELSE
    PUT UNFORMATTED
        'Empresa;Estabelecimento;Matr¡cula;Nome;Centro de Custo;Descri‡Æo;Per¡odo;Cargo;Descri‡Æo;Sal rio;PPR;Percentual;Avos;In¡cio;T‚rmino;C¢digo Situa‡Æo;Descri‡Æo;In¡cio Sit;T‚rmino Sit' SKIP.

FOR EACH es_import_ppr NO-LOCK WHERE
         es_import_ppr.cdn_empresa             >= tt-param.cdn_empresa_ini              AND
         es_import_ppr.cdn_empresa             <= tt-param.cdn_empresa_fim              AND
         es_import_ppr.cdn_estab               >= tt-param.cdn_estab_ini                AND
         es_import_ppr.cdn_estab               <= tt-param.cdn_estab_fim                AND
         es_import_ppr.cdn_funcionario         >= tt-param.cdn_funcionario_ini          AND
         es_import_ppr.cdn_funcionario         <= tt-param.cdn_funcionario_fim          AND
         es_import_ppr.cdn_cargo_basic         >= tt-param.cdn_cargo_basic_ini          AND
         es_import_ppr.cdn_cargo_basic         <= tt-param.cdn_cargo_basic_fim          AND
         es_import_ppr.num_anotip               = tt-param.num_anotip,
   FIRST funcionario NO-LOCK WHERE
         funcionario.cdn_empresa                = es_import_ppr.cdn_empresa             AND
         funcionario.cdn_estab                  = es_import_ppr.cdn_estab               AND
         funcionario.cdn_funcionario            = es_import_ppr.cdn_funcionario,
   FIRST rh_ccusto NO-LOCK WHERE
         rh_ccusto.cod_rh_ccusto                = es_import_ppr.cod_rh_ccusto,
   FIRST cargo_basic NO-LOCK WHERE
         cargo_basic.cdn_cargo_basic            = es_import_ppr.cdn_cargo_basic:

    RUN pi-acompanhar IN v_han_acomp('Emp ' + es_import_ppr.cdn_empresa + ' Estab ' + es_import_ppr.cdn_estab + ' Func ' + STRING(es_import_ppr.cdn_funcionario)).

    PUT UNFORMATTED
        funcionario.cdn_empresa                                         ';'
        funcionario.cdn_estab                                           ';'
        funcionario.cdn_funcionario                                     ';'
        funcionario.nom_pessoa_fisic                                    ';'
        es_import_ppr.cod_rh_ccusto                                     ';'
        rh_ccusto.des_rh_ccusto                                         ';'
        es_import_ppr.num_anotip                                        ';'
        es_import_ppr.cdn_cargo_basic                                   ';'
        cargo_basic.des_cargo_basic                                     ';'
        es_import_ppr.val_salario_period                                ';'
        es_import_ppr.val_ppr                                           ';'
        es_import_ppr.val_percent                                       ';'
        es_import_ppr.qti_avos                                          ';'
        es_import_ppr.dat_inicio                                        ';'
        es_import_ppr.dat_fim                                           ';' SKIP.

    IF tt-param.idi_tip_relat = 1 THEN DO:

        FOR EACH sit_afast_func NO-LOCK OF funcionario WHERE
                 sit_afast_func.dat_inic_sit_afast <= es_import_ppr.dat_fim                 AND
                 sit_afast_func.dat_term_sit_afast >= es_import_ppr.dat_inicio,
           FIRST sit_afast NO-LOCK OF sit_afast_func,
           FIRST es_controle_sit NO-LOCK WHERE
                 es_controle_sit.cdn_empresa        = sit_afast_func.cdn_empresa            AND
                 es_controle_sit.cdn_estab          = sit_afast_func.cdn_estab              AND
                 es_controle_sit.num_anotip         = es_import_ppr.num_anotip              AND
                 es_controle_sit.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func:
    
            PUT UNFORMATTED
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                ';'
                sit_afast.cdn_sit_afast_func                                                ';'
                sit_afast.des_sit_afast_func                                                ';'
                sit_afast_func.dat_inic_sit_afast                                           ';'
                sit_afast_func.dat_term_sit_afast   SKIP.
    
        END.

    END.

END.

output close.

CREATE 'Excel.application' chExcel.
chExcel:WorkBooks:OPEN(tt-param.arquivo).

chExcel:ActiveWindow:DisplayGridLines = FALSE.

chExcel:ActiveSheet:Range('A1:AA1'):FONT:Bold = TRUE.
chExcel:ActiveSheet:Range('A1:AA1'):EntireColumn:Autofit.

chExcel:VISIBLE = TRUE.

RELEASE OBJECT chExcel.

RUN pi-finalizar IN v_han_acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


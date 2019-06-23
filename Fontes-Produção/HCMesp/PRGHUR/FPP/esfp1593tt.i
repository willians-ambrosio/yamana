&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : ESFP1593tt.i
    Purpose     :

    Syntax      :

    Description : Include Temp-table - Listagem Geral de Funcion rios

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


{include/i_dbvers.i}
{prghur/fpp/fp9240.i8}
define {1} {2} temp-table tt-param
    {prghur/fpp/fp9240.i8}
    field parametro               as logical /* Imprime parametros */
    field destino                 as integer
    field arquivo                       as char
    field modelo-rtf                   as char
    field l-habilitaRtf                 as log
    field usuario                 as char format "x(15)"
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(50)"
    field v_cdn_empres_usuar    like param_empres_rh.cdn_empresa
    field v_num_tip_aces_usuar    as integer format "9" 
    field v_cod_grp_usuar         as char
    field v_num_opcao             as int  format "9"
    field v_des_opcao             as char format "x(10)"
    field v_dat_valid             as date format "99/99/9999"
    field v_log_expande_estrut    as log
    field v_num_salta_pg          as integer
    field v_num_quebra            as integer
    field v_num_faixa             as integer
    field v_cod_unid_lotac_ini  like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim  like unid_lotac.cod_unid_lotac
    field i-es-ini              like rh_estab.cdn_estab
    field i-es-fim              like rh_estab.cdn_estab
    field i-fc-ini              like funcionario.cdn_funcionario
    field i-fc-fim              like funcionario.cdn_funcionario
    field cod_rh_ccusto_ini     like funcionario.cod_rh_ccusto
    field cod_rh_ccusto_fim     like funcionario.cod_rh_ccusto
    field cdn_tip_contrat_ini   like funcionario.cdn_tip_contrat_func
    field cdn_tip_contrat_fim   like funcionario.cdn_tip_contrat_func
    field log_mensal              as log
    field log_horista             as log
    field log_semanal             as log
    field log_quinzenal           as log
    field log_tarefa              as log
    field log_diarista            as log    
    field log_lista_demit         as log
    field l-func                  as log
    field l-estag                 as log
    field l-aposent               as log
    field l-empreg                as log
    field l-prazo-determ          as log
    field l-tempo-parcial         as log
    field l-aprendiz              as log
    field v-data-aposent          as date format "99/99/9999"
    field idi_tip_cargo_funcao    like cargo.idi_tip_cargo_funcao
    field des_tip_cargo_funcao    as char format "x(10)".

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



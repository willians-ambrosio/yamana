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

/* ***************************  Definitions  ************************** */

{esp\estip0004tt.i}


/*---- DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.
DEFINE VARIABLE c-arq-saida AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE tt_es_movto_ppr LIKE es_movto_ppr.

/*----- INCLUDES PARA O RELATORIO -----*/
def temp-table tt-log-import no-undo
     field cdn_empresa      LIKE es_movto_ppr.cdn_empresa    
     field cdn_estab        LIKE es_movto_ppr.cdn_estab      
     field cdn_funcionario  LIKE es_movto_ppr.cdn_funcionario
     field desc-log      as char format "x(40)".

{utp/ut-glob.i}

    /*----- DEFINICAO DE STREAM -----*/
DEF  STREAM s-exp.
/* define new shared stream str-rp. */


/*----- INCLUDES PARA O RELATORIO -----*/
{include/i-rpvar.i}

def var h-acomp as handle no-undo.

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
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.46
         WIDTH              = 37.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
find first param-global no-lock no-error.


{include/i-prgvrs.i ES0004RP 2.12.00.004}

/* bloco principal do programa */
assign  c-programa      = "ESTIP0004RP"
        c-versao        = "2.12"
        c-revisao       = ".00.003"
        c-empresa       = param-global.grupo
        c-sistema       = "Especifico"
        c-titulo-relat = "Geraá∆o de Movto PPR".


    
/*----- DIRECIONAMENTO DO RELATORIO -----*/

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Importando *}


run pi-inicializar in h-acomp (input RETURN-VALUE).

create tt-param.
raw-transfer raw-param to tt-param.

FIND FIRST tt-param.


EMPTY TEMP-TABLE tt-log-import.
EMPTY TEMP-TABLE tt_es_movto_ppr.


RUN pi-processa.

IF tt-param.l-simula = YES THEN DO:
    FOR EACH tt_es_movto_ppr:
           FIND FIRST es_movto_ppr WHERE es_movto_ppr.cdn_param_calc           = tt_es_movto_ppr.cdn_param_calc                     
                                    and  es_movto_ppr.cdn_empresa              = tt_es_movto_ppr.cdn_empresa             
                                    and  es_movto_ppr.cdn_estab                = tt_es_movto_ppr.cdn_estab               
                                    and  es_movto_ppr.cdn_funcionario          = tt_es_movto_ppr.cdn_funcionario         
                                    and  es_movto_ppr.num_mes_refer_calc_efetd = tt_es_movto_ppr.num_mes_refer_calc_efetd
                                    and  es_movto_ppr.num_ano_refer_calc_efetd = tt_es_movto_ppr.num_ano_refer_calc_efetd
                                    and  es_movto_ppr.idi_tip_folha_pagto_ppr  = tt_es_movto_ppr.idi_tip_folha_pagto_ppr 
                                    and  es_movto_ppr.qti_parc_habilit_calc_fp = tt_es_movto_ppr.qti_parc_habilit_calc_fp
                                    and  es_movto_ppr.cdn_event_fp             = tt_es_movto_ppr.cdn_event_fp            NO-LOCK NO-ERROR.
            IF NOT AVAIL es_movto_ppr THEN DO:
                ASSIGN  tt_es_movto_ppr.val_base_calc_fp  = 0
                        tt_es_movto_ppr.num_livre_1       = 0.
                CREATE es_movto_ppr.
                BUFFER-COPY tt_es_movto_ppr TO es_movto_ppr.
            END.
      
    END.
    
FOR EACH es_movto_ppr:
    FIND FIRST es_import_ppr WHERE es_import_ppr.cdn_empresa     = string(es_movto_ppr.cdn_empresa)      
                               AND es_import_ppr.cdn_estab       = string(es_movto_ppr.cdn_estab)       
                               AND es_import_ppr.cdn_funcionario = es_movto_ppr.cdn_funcionario 
                               AND es_import_ppr.cdn_empresa_dest <> ""
                                NO-LOCK NO-ERROR.
    IF AVAIL es_import_ppr THEN DO:
        
        FIND FIRST param_calc_ppr WHERE PARAM_calc_ppr.cdn_param_calc_ppr = tt-param.cdn_param_calc_ppr NO-LOCK NO-ERROR.
        IF AVAIL param_calc_ppr THEN DO:
    
        ASSIGN  es_movto_ppr.cdn_empresa      = int(es_import_ppr.cdn_empresa_dest)
                es_movto_ppr.cdn_estab        = INT(es_import_ppr.cdn_estab_dest)
                es_movto_ppr.cdn_funcionario  = es_import_ppr.cdn_funcionario_dest
                es_movto_ppr.cdn_param_calc     = PARAM_calc_ppr.cdn_param_calc  .
        END.
        
        FIND FIRST param_calc_ppr WHERE PARAM_calc_ppr.cdn_param_calc_ppr = tt-param.cdn_param_calc_ppr NO-LOCK NO-ERROR.
        IF AVAIL param_calc_ppr THEN DO:
            
            FIND FIRST funcionario WHERE funcionario.cdn_estab         = es_import_ppr.cdn_estab_dest
                                     AND funcionario.cdn_funcionario   = es_import_ppr.cdn_funcionario_dest
                                     NO-LOCK NO-ERROR.
           IF AVAIL funcionario THEN DO:
    
               FIND FIRST habilit_calc_ppr NO-LOCK WHERE habilit_calc_ppr.cdn_estab       = funcionario.cdn_estab 
                                                 AND habilit_calc_ppr.cdn_param_calc      = PARAM_calc_ppr.cdn_param_calc_ppr 
                                                 AND habilit_calc_ppr.cdn_categ_sal       = funcionario.cdn_categ_sal NO-ERROR.
    
               IF AVAIL habilit_calc_ppr THEN DO:
    
                    ASSIGN es_movto_ppr.cdn_param_calc             = habilit_calc_ppr.cdn_param_calc     
                           es_movto_ppr.num_mes_refer_calc_efetd   = habilit_calc_ppr.num_mes_refer_calc_efetd                                                                                      
                           es_movto_ppr.num_ano_refer_calc_efetd   = habilit_calc_ppr.num_ano_refer_calc_efetd                                                                                      
                           es_movto_ppr.idi_tip_folha_pagto_ppr    = habilit_calc_ppr.idi_tip_folha_pagto_ppr                                                                                       
                           es_movto_ppr.cdn_event_fp               = PARAM_calc_ppr.cdn_event_pagto_ppr                                                                                             
                            es_movto_ppr.cdn_param_calc          =      PARAM_calc_ppr.cdn_param_calc  .
               END.                                                                                               
           END.
        END.
    END.
END.
    
    RELEASE es_movto_ppr.
   
    RUN pi-calcula.
END.
         
ASSIGN c-arq-saida = SESSION:TEMP-DIR + "estip0004_" + string(MONTH(TODAY)) + "_" + string(YEAR(TODAY)) + "_" + replace(string(TIME,"HH:MM:SS"),":","_") + ".csv".
output TO value(c-arq-saida ). 
PUT UNFORMAT "Cod. Param.; Empresa; Estab.; Matr.; Mes Refer.; Ano Refer.; Tipo Folha; Evento; Resultado" SKIP.

FOR EACH tt_es_movto_ppr:
    
    PUT UNFORMAT tt_es_movto_ppr.cdn_param_calc             ";" 
                 tt_es_movto_ppr.cdn_empresa                ";" 
                 tt_es_movto_ppr.cdn_estab                  ";" 
                 tt_es_movto_ppr.cdn_funcionario            ";" 
                 tt_es_movto_ppr.num_mes_refer_calc_efetd   ";" 
                 tt_es_movto_ppr.num_ano_refer_calc_efetd   ";" 
                 tt_es_movto_ppr.idi_tip_folha_pagto_ppr    ";" 
                 tt_es_movto_ppr.cdn_event_fp               ";" 
                 tt_es_movto_ppr.val_calcul_efp             SKIP.
END.
OUTPUT CLOSE.
OS-COMMAND NO-WAIT START  VALUE(c-arq-saida).

output stream s-exp TO value(replace(c-arq-saida,"csv","log")). 

for each tt-log-import:

    disp stream s-exp tt-log-import.cdn_empresa 
                       tt-log-import.cdn_estab 
                       tt-log-import.cdn_funcionario     
                       tt-log-import.desc-log    format "x(120)" column-label "Status"  
                   with width 200 no-box stream-io.

end.

output stream s-exp CLOSE.
OS-COMMAND  NO-WAIT START VALUE(replace(c-arq-saida,"csv","log")).
run pi-finalizar in h-acomp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-calcula) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula Procedure 
PROCEDURE pi-calcula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-log-import.
/*DISABLE TRIGGERS FOR load OF hcm.movto_ppr.*/
FOR EACH es_movto_ppr  WHERE es_movto_ppr.cdn_empresa                 =  int(tt-param.cdn_empresa)
                         AND es_movto_ppr.cdn_estab                   =  INT(tt-param.cdn_estab)   :

    FIND FIRST es_import_ppr WHERE es_import_ppr.cdn_empresa_dest     = string(es_movto_ppr.cdn_empresa)      
                               AND es_import_ppr.cdn_estab_dest       = string(es_movto_ppr.cdn_estab)       
                               AND es_import_ppr.cdn_funcionario_dest = es_movto_ppr.cdn_funcionario 
                               AND es_import_ppr.cdn_empresa_dest <> ""
                                NO-LOCK NO-ERROR.
    IF AVAIL es_import_ppr THEN DO:
        
        FIND FIRST param_calc_ppr WHERE PARAM_calc_ppr.cdn_param_calc_ppr = tt-param.cdn_param_calc_ppr NO-LOCK NO-ERROR.
        IF AVAIL PARAM_calc_ppr THEN DO:

            FIND FIRST funcionario WHERE funcionario.cdn_estab         = es_import_ppr.cdn_estab_dest 
                                     AND funcionario.cdn_funcionario   = es_import_ppr.cdn_funcionario_dest
                                     NO-LOCK NO-ERROR.
            IF AVAIL funcionario THEN DO:
            
                FIND FIRST habilit_calc_ppr NO-LOCK WHERE habilit_calc_ppr.cdn_estab           = funcionario.cdn_estab 
                                                      AND habilit_calc_ppr.cdn_param_calc      = PARAM_calc_ppr.cdn_param_calc_ppr 
                                                      AND habilit_calc_ppr.cdn_categ_sal       = funcionario.cdn_categ_sal NO-ERROR.
        
                IF AVAIL habilit_calc_ppr THEN DO:
            
                    /*ASSIGN es_movto_ppr.cdn_param_calc     = PARAM_calc_ppr.cdn_param_calc  .*/
                     ASSIGN es_movto_ppr.cdn_param_calc             = habilit_calc_ppr.cdn_param_calc     
                            es_movto_ppr.num_mes_refer_calc_efetd   = habilit_calc_ppr.num_mes_refer_calc_efetd                                                                                      
                            es_movto_ppr.num_ano_refer_calc_efetd   = habilit_calc_ppr.num_ano_refer_calc_efetd                                                                                      
                            es_movto_ppr.idi_tip_folha_pagto_ppr    = habilit_calc_ppr.idi_tip_folha_pagto_ppr                                                                                       
                            es_movto_ppr.cdn_event_fp               = PARAM_calc_ppr.cdn_event_pagto_ppr                                                                                             
                            es_movto_ppr.cdn_param_calc             = PARAM_calc_ppr.cdn_param_calc 
                            es_movto_ppr.qti_parc_habilit_calc_fp   = habilit_calc_ppr.qti_parc_habilit_calc_fp .
                END.
            END.
        END.
    END.
   
    FIND FIRST movto_ppr WHERE movto_ppr.cdn_param_calc              = es_movto_ppr.cdn_param_calc 
                           AND movto_ppr.cdn_empresa                 = string(es_movto_ppr.cdn_empresa)
                           and movto_ppr.cdn_estab                   = STRING(es_movto_ppr.cdn_estab)       
                           and movto_ppr.cdn_funcionario             = es_movto_ppr.cdn_funcionario         
                           and movto_ppr.num_mes_refer_calc_efetd    = es_movto_ppr.num_mes_refer_calc_efetd
                           and movto_ppr.num_ano_refer_calc_efetd    = es_movto_ppr.num_ano_refer_calc_efetd
                           and movto_ppr.idi_tip_folha_pagto_ppr     = es_movto_ppr.idi_tip_folha_pagto_ppr 
                           AND movto_ppr.qti_parc_habilit_calc_fp    = es_movto_ppr.qti_parc_habilit_calc_fp 
                           AND movto_ppr.cdn_event_fp                = es_movto_ppr.cdn_event_fp
                           NO-ERROR.
    
    IF NOT AVAIL movto_ppr THEN DO:
        
         FIND FIRST funcionario WHERE funcionario.cdn_estab         = string(es_movto_ppr.cdn_estab )    
                                  AND funcionario.cdn_funcionario   = es_movto_ppr.cdn_funcionario
                                  NO-LOCK NO-ERROR.
         IF AVAIL funcionario THEN DO:
            
            run pi-acompanhar in h-acomp (input "Calculando./ Funcion†rio " + string(funcionario.cdn_funcionario) ).
             
            IF tt-param.l-simula = YES THEN DO:
                
                CREATE movto_ppr.
                ASSIGN movto_ppr.cdn_param_calc              = es_movto_ppr.cdn_param_calc              
                       movto_ppr.cdn_estab                   = funcionario.cdn_estab            
                       movto_ppr.cdn_empresa                 = funcionario.cdn_empresa
                       movto_ppr.cdn_funcionario             = funcionario.cdn_funcionario            
                       movto_ppr.num_mes_refer_calc_efetd    = es_movto_ppr.num_mes_refer_calc_efetd    
                       movto_ppr.num_ano_refer_calc_efetd    = es_movto_ppr.num_ano_refer_calc_efetd    
                       movto_ppr.idi_tip_folha_pagto_ppr     = es_movto_ppr.idi_tip_folha_pagto_ppr    
                       movto_ppr.qti_parc_habilit_calc_fp    = es_movto_ppr.qti_parc_habilit_calc_fp
                       movto_ppr.cdn_event_fp                = es_movto_ppr.cdn_event_fp                
                       movto_ppr.val_calcul_efp              = es_movto_ppr.val_calcul_efp               
                       movto_ppr.idi_orig_movto_ppr          = 1.
                 
                CREATE tt-log-import.
                ASSIGN tt-log-import.cdn_empresa     = INT(es_movto_ppr.cdn_empresa)
                       tt-log-import.cdn_estab       = INT(es_movto_ppr.cdn_estab)         
                       tt-log-import.cdn_funcionario =  es_movto_ppr.cdn_funcionario 
                       tt-log-import.desc-log        = "Registro Calculado com Sucesso!" . 
            END.
           
         END.
    END.
    ELSE DO:
          CREATE tt-log-import.
          ASSIGN tt-log-import.cdn_empresa     = INT(es_movto_ppr.cdn_empresa)
                 tt-log-import.cdn_estab       = INT(es_movto_ppr.cdn_estab)         
                 tt-log-import.cdn_funcionario =  es_movto_ppr.cdn_funcionario 
                 tt-log-import.desc-log        = "REGISTRO NAO PODE SER CALCULADO! - Calculo ja foi efetuado FP3130" . 
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-movto-ppr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-movto-ppr Procedure 
PROCEDURE pi-cria-movto-ppr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cdn-empresa AS CHAR.
DEFINE INPUT PARAMETER cdn-estab   AS CHAR.
DEFINE INPUT PARAMETER cdn-func    AS INT.
DEFINE INPUT PARAMETER cdn_cargo   AS INT.
DEFINE BUFFER bf_es_import_ppr FOR es_import_ppr .
DEFINE BUFFER bf_cargo  FOR cargo.
DEFINE BUFFER bf_es_controle_pag FOR es_controle_pag.
DEFINE BUFFER bf_histor_sal_func FOR histor_sal_func.

DEF VAR d-salario LIKE funcionario.val_salario_atual.

FIND FIRST param_calc_ppr WHERE PARAM_calc_ppr.cdn_param_calc_ppr = tt-param.cdn_param_calc_ppr NO-LOCK NO-ERROR.
IF AVAIL param_calc_ppr THEN DO:

    ASSIGN d-salario = 0.

    FIND FIRST bf_es_import_ppr WHERE bf_es_import_ppr.cdn_empresa      = es_import_ppr.cdn_empresa    
                                  AND bf_es_import_ppr.cdn_estab        = es_import_ppr.cdn_estab      
                                  AND bf_es_import_ppr.cdn_funcionario  = es_import_ppr.cdn_funcionario
                                  AND bf_es_import_ppr.cdn_cargo_basic <> es_import_ppr.cdn_cargo_basic   NO-LOCK NO-ERROR.

     FIND FIRST funcionario WHERE funcionario.cdn_estab         = es_import_ppr.cdn_estab
                              AND funcionario.cdn_funcionario   = es_import_ppr.cdn_funcionario
                                  NO-LOCK NO-ERROR.
         IF AVAIL funcionario THEN DO:
             FIND FIRST habilit_calc_ppr NO-LOCK WHERE habilit_calc_ppr.cdn_estab       = funcionario.cdn_estab 
                                               AND habilit_calc_ppr.cdn_param_calc      = PARAM_calc_ppr.cdn_param_calc_ppr 
                                               AND habilit_calc_ppr.cdn_categ_sal       = funcionario.cdn_categ_sal NO-ERROR.

             IF AVAIL habilit_calc_ppr THEN DO:

                  FOR EACH PARAM_calc_ppr OF  habilit_calc_ppr NO-LOCK:

                      FIND LAST histor_sal_func WHERE histor_sal_func.cdn_empresa      = es_import_ppr.cdn_empresa
                                                   AND histor_sal_func.cdn_estab       = es_import_ppr.cdn_estab           
                                                   AND histor_sal_func.cdn_funcionario = es_import_ppr.cdn_funcionario     
                                                   AND histor_sal_func.cdn_cargo_basic = es_import_ppr.cdn_cargo_basic
                                                   AND year(histor_sal_func.dat_liber_sal) <= es_import_ppr.num_anotip   NO-LOCK NO-ERROR.  

                      IF es_import_ppr.cdn_empresa_dest <> "" THEN
                      FIND LAST histor_sal_func WHERE histor_sal_func.cdn_empresa      = es_import_ppr.cdn_empresa_dest
                                                  AND histor_sal_func.cdn_estab       = es_import_ppr.cdn_estab_dest           
                                                  AND histor_sal_func.cdn_funcionario = es_import_ppr.cdn_funcionario_dest     
                                                  AND histor_sal_func.cdn_cargo_basic = es_import_ppr.cdn_cargo_basic
                                                  AND year(histor_sal_func.dat_liber_sal) <= es_import_ppr.num_anotip   NO-LOCK NO-ERROR. 
                      IF AVAIL histor_sal_func THEN
                      FIND FIRST cargo WHERE cargo.cdn_cargo_basic = cdn_cargo 
                                         AND cargo.cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo
                                         NO-LOCK NO-ERROR.
                      ELSE 
                      FIND FIRST cargo WHERE cargo.cdn_cargo_basic = cdn_cargo 
                                    AND cargo.cdn_niv_cargo   = funcionario.cdn_niv_cargo
                                    NO-LOCK NO-ERROR.

                      IF AVAIL cargo THEN DO:
                      
                          FIND FIRST es_controle_pag WHERE es_controle_pag.cdn_empresa          = es_import_ppr.cdn_empresa
                                                       AND es_controle_pag.cdn_estab            = es_import_ppr.cdn_estab      
                                                       AND es_controle_pag.num_anotip           = es_import_ppr.num_anotip
                                                       AND es_controle_pag.cdn_niv_hier_funcnal = cargo.cdn_niv_hier_funcnal
                                                       NO-LOCK NO-ERROR.                                                    
                      
                          IF AVAIL es_controle_pag THEN DO:
                      
                              IF AVAIL histor_sal_func  THEN DO:
                                               
                                  FIND FIRST bf_cargo WHERE bf_cargo.cdn_cargo_basic = bf_es_import_ppr.cdn_cargo_basic
                                                        AND bf_cargo.cdn_niv_cargo   = funcionario.cdn_niv_cargo
                                                        NO-LOCK NO-ERROR.
                                  IF AVAIL bf_cargo THEN DO:
                                      FIND FIRST bf_es_controle_pag WHERE bf_es_controle_pag.cdn_empresa          = es_import_ppr.cdn_empresa
                                                                      AND bf_es_controle_pag.cdn_estab            = es_import_ppr.cdn_estab      
                                                                      AND bf_es_controle_pag.num_anotip           = es_import_ppr.num_anotip
                                                                      AND bf_es_controle_pag.cdn_niv_hier_funcnal = bf_cargo.cdn_niv_hier_funcnal
                                                                      NO-LOCK NO-ERROR.                                                    
                                  END.
                                  
                                 IF AVAIL  bf_es_controle_pag AND es_controle_pag.num_qtdsal <> bf_es_controle_pag.num_qtdsal  THEN DO:
                                     ASSIGN d-salario = histor_sal_func.val_salario_categ.
                                 END.
                                 ELSE DO:
                                     FIND LAST bf_histor_sal_func WHERE bf_histor_sal_func.cdn_empresa  = es_import_ppr.cdn_empresa
                                                                    AND bf_histor_sal_func.cdn_estab       = es_import_ppr.cdn_estab           
                                                                    AND bf_histor_sal_func.cdn_funcionario = es_import_ppr.cdn_funcionario     
                                                                  /*  AND bf_histor_sal_func.cdn_cargo_basic = es_import_ppr.cdn_cargo_basic */
                                                                    AND year(bf_histor_sal_func.dat_liber_sal) <= es_import_ppr.num_anotip NO-LOCK NO-ERROR.
                                     IF AVAIL bf_histor_sal_func  THEN DO:
                                         
                                         ASSIGN d-salario = bf_histor_sal_func.val_salario_categ.
                                     END.
                                     ELSE DO:
                                        ASSIGN d-salario = funcionario.val_salario_atual.
                                     END.
                                 END.

                             END.
                            
                             ASSIGN es_import_ppr.val_salario_period = d-salario. /*Ajuste para o Relat¢rio ESTIP0005*/

                          
                              FIND FIRST tt_es_movto_ppr WHERE 
                                         tt_es_movto_ppr.cdn_param_calc                 = habilit_calc_ppr.cdn_param_calc 
                                     AND tt_es_movto_ppr.cdn_empresa                    = int(es_controle_pag.cdn_empresa) 
                                     and tt_es_movto_ppr.cdn_estab                      = INT(habilit_calc_ppr.cdn_estab)  
                                     and tt_es_movto_ppr.cdn_funcionario                = funcionario.cdn_funcionario      
                                     and tt_es_movto_ppr.num_mes_refer_calc_efetd       = habilit_calc_ppr.num_mes_refer_calc_efetd
                                     and tt_es_movto_ppr.num_ano_refer_calc_efetd       = habilit_calc_ppr.num_ano_refer_calc_efetd
                                     AND tt_es_movto_ppr.qti_parc_habilit_calc_fp       = habilit_calc_ppr.qti_parc_habilit_calc_fp 
                                     and tt_es_movto_ppr.idi_tip_folha_pagto_ppr        = habilit_calc_ppr.idi_tip_folha_pagto_ppr
                                     NO-ERROR.
                              IF NOT AVAIL tt_es_movto_ppr THEN DO:
                                                 
                                  run pi-acompanhar in h-acomp (input "Calculando Dados. Funcion†rio " + string(es_import_ppr.cdn_funcionario) ).
                                  
                                  CREATE tt_es_movto_ppr.
                                  ASSIGN tt_es_movto_ppr.val_base_calc_fp           = d-salario
                                         tt_es_movto_ppr.num_livre_1                = es_controle_pag.cdn_niv_hier_funcnal  /*Necessario para pegar ultimo salario se for mesmo nivel*/
                                         tt_es_movto_ppr.cdn_param_calc             = habilit_calc_ppr.cdn_param_calc           
                                         tt_es_movto_ppr.cdn_empresa                = int(es_controle_pag.cdn_empresa) 
                                         tt_es_movto_ppr.cdn_estab                  = INT(habilit_calc_ppr.cdn_estab)  
                                         tt_es_movto_ppr.cdn_funcionario            = funcionario.cdn_funcionario        
                                         tt_es_movto_ppr.num_mes_refer_calc_efetd   = habilit_calc_ppr.num_mes_refer_calc_efetd                                                                                      
                                         tt_es_movto_ppr.num_ano_refer_calc_efetd   = habilit_calc_ppr.num_ano_refer_calc_efetd                                                                                      
                                         tt_es_movto_ppr.idi_tip_folha_pagto_ppr    = habilit_calc_ppr.idi_tip_folha_pagto_ppr                                                                                       
                                         tt_es_movto_ppr.cdn_event_fp               = PARAM_calc_ppr.cdn_event_pagto_ppr                                                                                             
                                         tt_es_movto_ppr.qti_parc_habilit_calc_fp   = habilit_calc_ppr.qti_parc_habilit_calc_fp .
                                         tt_es_movto_ppr.val_calcul_efp             = (((d-salario * es_controle_pag.num_qtdsal) / 12) * es_import_ppr.qti_avos) * ( 1 * (es_import_ppr.val_percent / 100) ).
                                                                                       
                               /*   IF es_import_ppr.cdn_empresa_dest <> "" THEN 
                                      ASSIGN tt_es_movto_ppr.cdn_empresa      = int(es_import_ppr.cdn_empresa_dest) 
                                             tt_es_movto_ppr.cdn_estab        = int(es_import_ppr.cdn_estab)       
                                             tt_es_movto_ppr.cdn_funcionario  = es_import_ppr.cdn_funcionario   . */
                                  
                              END.
                              ELSE DO:
                                
                                  IF  tt_es_movto_ppr.num_livre_1 =  es_controle_pag.cdn_niv_hier_funcnal THEN
                                     ASSIGN tt_es_movto_ppr.val_calcul_efp  = tt_es_movto_ppr.val_calcul_efp  + ((( tt_es_movto_ppr.val_base_calc_fp  * es_controle_pag.num_qtdsal) / 12) * es_import_ppr.qti_avos) * ( 1 * (es_import_ppr.val_percent / 100 )).
                                  ELSE 
                                     ASSIGN tt_es_movto_ppr.val_calcul_efp  = tt_es_movto_ppr.val_calcul_efp  + (((d-salario * es_controle_pag.num_qtdsal) / 12) * es_import_ppr.qti_avos) * ( 1 * (es_import_ppr.val_percent / 100 )).
                              END.
                      
                          END.
                          ELSE DO:
                              
                              CREATE tt-log-import.
                              ASSIGN tt-log-import.cdn_empresa     = INT(cdn-empresa)
                                     tt-log-import.cdn_estab       = INT(cdn-estab)        
                                     tt-log-import.cdn_funcionario = cdn-func   
                                     tt-log-import.desc-log        = "Parametro de Quantidade x Salario x Nivel Hierarquico n∆o cadastrado - ESTIP0001! " + " Nivel Hier."+ STRING(cargo.cdn_niv_hier_funcnal )  + "/ Cargo " + STRING(cargo.cdn_niv_hier_funcnal) . 
                              
                              
                          
                          END.
                      
                      END.
                      ELSE DO:
                          
                          CREATE tt-log-import.
                          ASSIGN tt-log-import.cdn_empresa     = INT(cdn-empresa)
                                 tt-log-import.cdn_estab       = INT(cdn-estab)         
                                 tt-log-import.cdn_funcionario =  cdn-func
                                 tt-log-import.desc-log        = "Cargo nao encontrado!" . 
                      END.
             END.

         END.

    END.
    ELSE DO:
        
          CREATE tt-log-import.
          ASSIGN tt-log-import.cdn_empresa     = INT(cdn-empresa)
                 tt-log-import.cdn_estab       = INT(cdn-estab)         
                 tt-log-import.cdn_funcionario =  cdn-func
                 tt-log-import.desc-log        = "Parametro PLR N∆o Habilitado na ABA Pagamento" . 
    END.
END.
ELSE DO:
    
    CREATE tt-log-import.
    ASSIGN tt-log-import.cdn_empresa     = INT(cdn-empresa)
           tt-log-import.cdn_estab       = INT(cdn-estab)         
           tt-log-import.cdn_funcionario =  cdn-func 
           tt-log-import.desc-log        = "Parametro PLR Nao Encontrado FP0900" . 

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa Procedure 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


FOR EACH es_import_ppr WHERE es_import_ppr.num_anotip  = tt-param.num_anotip 
                         AND es_import_ppr.cdn_empresa = tt-param.cdn_empresa 
                         AND es_import_ppr.cdn_estab   = tt-param.cdn_estab   
                         /*AND es_import_ppr.cdn_empresa_dest = ""*/  BY es_import_ppr.cdn_cargo_basic DESC:
   
        RUN pi-cria-movto-ppr(es_import_ppr.cdn_empresa    ,
                              es_import_ppr.cdn_estab      ,   
                              es_import_ppr.cdn_funcionario, 
                              es_import_ppr.cdn_cargo_basic).
   

END.
/*
FOR EACH  tt_es_movto_ppr:
    
    FIND FIRST es_import_ppr WHERE es_import_ppr.cdn_empresa     = string(tt_es_movto_ppr.cdn_empresa)      
                               AND es_import_ppr.cdn_estab       = string(tt_es_movto_ppr.cdn_estab)       
                               AND es_import_ppr.cdn_funcionario = tt_es_movto_ppr.cdn_funcionario 
                               AND es_import_ppr.cdn_empresa_dest <> ""
                               NO-LOCK NO-ERROR.
    IF AVAIL es_import_ppr THEN DO:





    END.


END.
  */

/*Transferidos
FOR EACH es_import_ppr WHERE es_import_ppr.num_anotip       = tt-param.num_anotip 
                         AND es_import_ppr.cdn_empresa      = tt-param.cdn_empresa 
                         AND es_import_ppr.cdn_estab        = tt-param.cdn_estab 
                         AND es_import_ppr.cdn_empresa_dest <> ""   NO-LOCK:

  
      RUN pi-cria-movto-ppr(es_import_ppr.cdn_empresa_dest    ,
                            es_import_ppr.cdn_estab_dest      ,
                            es_import_ppr.cdn_funcionario_dest,
                            es_import_ppr.cdn_cargo_basic).

END.          */
/*
FOR EACH  tt_es_movto_ppr WHERE tt_es_movto_ppr.val_calcul_efp = ?:
    DELETE tt_es_movto_ppr.
END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


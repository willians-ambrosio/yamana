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

{include/i-prgvrs.i ES0002RP 2.12.00.004}

/*----- TEMP-TABLE DE PARAMETROS -----*/
                                      

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    FIELD l-reimp          AS LOG
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.
    


define temp-table tt-funcionario
    FIELD cdn_empresa      LIKE funcionario.cdn_empresa
    field cdn_estab        like histor_sal_func.cdn_estab    
    field cdn_funcionario  like funcionario.cdn_funcionario  
    FIELD cdn_cargo_basic  LIKE cargo.cdn_cargo_basic        
    FIELD dat_liber_sal    LIKE histor_sal_func.dat_liber_sal
    FIELD cdn_niv_cargo    LIKE cargo.cdn_niv_cargo
    FIELD num_anotip       LIKE es_import_ppr.num_anotip   .

define temp-table tt-funcionario-tot
    FIELD cdn_empresa      LIKE funcionario.cdn_empresa
    field cdn_estab        like histor_sal_func.cdn_estab    
    field cdn_funcionario  like funcionario.cdn_funcionario  
    FIELD cdn_cargo_basic  LIKE cargo.cdn_cargo_basic        
    FIELD dat_liber_sal    LIKE histor_sal_func.dat_liber_sal
    FIELD cdn_niv_cargo    LIKE cargo.cdn_niv_cargo   
    FIELD num_anotip       LIKE es_import_ppr.num_anotip     .
    


/* Transfer Definitions */
def temp-table tt-raw-digita
   field raw-digita      as raw.
 
/*---- DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.


/* ***************************  Definitions  ************************** */
def var h-acomp         as handle no-undo. 
DEFINE VARIABLE chExcel AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chLivro AS COM-HANDLE NO-UNDO. 
DEFINE VARIABLE chFolha AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i-error AS INTEGER     NO-UNDO.
DEF  STREAM s-exp.
/*DEFINE TEMP-TABLE tt_es_import_ppr LIKE es_import_ppr.*/

DEFINE TEMP-TABLE tt_es_import_ppr LIKE es_import_ppr.
DEFINE BUFFER bf_histor_sal_fun FOR histor_sal_fun.

define temp-table tt-func-per
    field cdn_empresa     LIKE es_import_ppr.cdn_empresa    
    field cdn_estab       like es_import_ppr.cdn_estab      
    field cdn_funcionario like es_import_ppr.cdn_funcionario
    FIELD cdn_cargo_basic LIKE es_import_ppr.cdn_cargo_basic
    FIELD num_anotip      LIKE es_import_ppr.num_anotip 
    FIELD val_calcul_simul LIKE es_movto_ppr.val_calcul_efp
    FIELD val_percent      LIKE es_import_ppr.val_percent
    field cdn_empresa_dest      LIKE es_import_ppr.cdn_empresa_dest         
    field cdn_estab_dest        LIKE es_import_ppr.cdn_estab_dest          
    field cdn_funcionario_dest  LIKE es_import_ppr.cdn_funcionario_dest 
    FIELD l-perda         AS LOG
    FIELD desc-calc       AS CHAR
    FIELD desc-log        AS CHAR
    FIELD qti_avos        AS INT
    field dt-ini          as DATE
    field dt-fim          as DATE.

def temp-table tt-log-import no-undo
     field cdn_empresa      LIKE es_movto_ppr.cdn_empresa    
     field cdn_estab        LIKE es_movto_ppr.cdn_estab      
     field cdn_funcionario  LIKE es_movto_ppr.cdn_funcionario
     field desc-log      as char format "x(40)".

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
         HEIGHT             = 15
         WIDTH              = 38.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
create tt-param.
raw-transfer raw-param to tt-param.

FIND FIRST tt-param.
/*
MESSAGE tt-param.arq-entrada
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/



RUN pi-importa-planilha.

output stream s-exp TO value(session:TEMP-DIRECTOR + "estip0002.LOG"). 

for each tt-func-per:

  
    

    DISP stream s-exp tt-func-per.cdn_empresa     
         tt-func-per.cdn_estab       
         tt-func-per.cdn_funcionario 
         tt-func-per.dt-ini          
         tt-func-per.dt-fim    
         tt-func-per.qti_avos
      /*   tt-func-per.val_calcul_simul
         tt-func-per.desc-calc FORMAT "X(50)"*/
         tt-func-per.desc-log FORMAT "X(120)"
                   with width 280 no-box stream-io.   

end.

FOR EACH tt-log-import:

    disp stream s-exp tt-log-import.cdn_empresa 
                       tt-log-import.cdn_estab 
                       tt-log-import.cdn_funcionario     
                       tt-log-import.desc-log    format "x(90)" column-label "Status"  
                   with width 200 no-box stream-io.
END.

output stream s-exp CLOSE.
IF SEARCH(session:TEMP-DIRECTOR + "estip0002.LOG") <> ? THEN
OS-COMMAND NO-WAIT START VALUE(session:TEMP-DIRECTOR + "estip0002.LOG").

FOR EACH es_import_ppr WHERE  es_import_ppr.cdn_empresa_dest <> "":  /*Transferencia */


   ASSIGN  es_import_ppr.cdn_funcionario  =  es_import_ppr.cdn_funcionario_dest
           es_import_ppr.cdn_empresa      =  es_import_ppr.cdn_empresa_dest 
           es_import_ppr.cdn_estab        =  es_import_ppr.cdn_estab_dest.   
                                          
END.


/*
input from value(tt-param.arq-entrada).
repeat on stop undo, leave:
    
    CREATE tt_es_import_ppr.
    import DELIMITER ";" tt_es_import_ppr.
   
END.
                                 

FOR EACH tt_es_import_ppr:
  
    FIND FIRST param_empres_rh WHERE
               param_empres_rh.cdn_empresa = TRIM(tt_es_import_ppr.col-01) 
               NO-LOCK NO-ERROR.
    IF AVAIL param_empres_rh THEN DO:
        
        FIND FIRST funcionario WHERE funcionario.cdn_empresa     = param_empres_rh.cdn_empresa 
                                 AND funcionario.cdn_estab       = tt_es_import_ppr.col-02
                                 AND funcionario.cdn_funcionario = int(tt_es_import_ppr.col-05)
                                 NO-LOCK NO-ERROR.
        IF AVAIL funcionario THEN DO:
            CREATE es_import_ppr.
            ASSIGN es_import_ppr.cdn_empresa     = funcionario.cdn_empresa    
                   es_import_ppr.cdn_estab       = funcionario.cdn_estab      
                   es_import_ppr.cdn_funcionario = funcionario.cdn_funcionario
                   es_import_ppr.cod_rh_ccusto   = funcionario.cod_rh_ccusto
                   es_import_ppr.cod_id_feder    = funcionario.cod_id_feder
                   es_import_ppr.cdn_cargo_basic = int(tt_es_import_ppr.col-08)
                   es_import_ppr.val_percent     = DEC(tt_es_import_ppr.col-09) .





                   
        END.
        ELSE 
            MESSAGE param_empres_rh.cdn_empresa SKIP  
                    tt_es_import_ppr.col-02      SKIP 
                    int(tt_es_import_ppr.col-03)  

                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                    

    END.


END.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-avos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-avos Procedure 
PROCEDURE pi-avos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER dt-ini AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER dt-fim AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER i-ano  AS INTEGER.
DEFINE OUTPUT PARAMETER i-avos AS INTEGER.

DEFINE VARIABLE i-cont AS INTEGER   INITIAL 0  NO-UNDO.

IF dt-ini < DATE("01/01/" + STRING(i-ano)) THEN
    ASSIGN dt-ini = DATE("01/01/" + STRING(i-ano)).

IF YEAR(dt-fim) > i-ano THEN
    ASSIGN dt-fim = DATE("31/12/" + string(i-ano)). 

IF dt-ini <> dt-fim THEN DO:

    IF day(dt-ini) >= 15 OR DAY(dt-fim) <= 15  THEN DO:
        
    
        IF day(dt-ini) >= 15 THEN DO:
            IF MONTH(dt-ini) = 12 THEN
                dt-ini = date(STRING(day(dt-ini)) + "/" + string(MONTH(dt-ini)) + "/" + string(YEAR(dt-ini))).
            ELSE
                dt-ini = date(STRING(28) + "/" + string((MONTH(dt-ini) + 1)) + "/" + string(YEAR(dt-ini))).
        END.
        IF day(dt-fim) <= 15 THEN DO:
            IF MONTH(dt-fim) = 12 OR MONTH(dt-fim) = 1  THEN
                dt-fim = date(STRING(day(dt-fim)) + "/" + string((MONTH(dt-fim))) + "/" + string(YEAR(dt-fim))).
            ELSE
               dt-fim = date(STRING(day(dt-fim)) + "/" + string((MONTH(dt-fim) - 1)) + "/" + string(YEAR(dt-fim))).
        END.
        DO i-cont = month(dt-ini) TO MONTH(dt-fim) :
        
            ASSIGN i-avos = i-avos + 1.
      
        
        END.
       
    END.
    ELSE DO:
    
         DO i-cont = month(dt-ini) TO MONTH(dt-fim) :
        
            ASSIGN i-avos = i-avos + 1.
           
        
        END.
    
    
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-movto-es) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-movto-es Procedure 
PROCEDURE pi-cria-movto-es :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-qtd-avos AS INTEGER     NO-UNDO.
DEFINE VARIABLE d-salario AS DECIMAL     NO-UNDO.

FOR EACH tt_es_import_ppr WHERE tt_es_import_ppr.num_anotip  <> ?:
 
    
    FIND FIRST param_empres_rh WHERE
               param_empres_rh.cdn_empresa = string(INT(tt_es_import_ppr.cdn_empresa)) 
               NO-LOCK NO-ERROR.
    IF AVAIL param_empres_rh THEN DO:
       
        FIND FIRST funcionario WHERE funcionario.cdn_empresa     = param_empres_rh.cdn_empresa 
                                 AND funcionario.cdn_estab       = STRING(int(tt_es_import_ppr.cdn_estab))
                                 AND funcionario.cdn_funcionario = int(tt_es_import_ppr.cdn_funcionario)
                                 NO-LOCK NO-ERROR.
        IF AVAIL funcionario THEN DO:

            FIND FIRST es_import_ppr WHERE es_import_ppr.cdn_empresa     = funcionario.cdn_empresa     
                                       AND es_import_ppr.cdn_estab       = funcionario.cdn_estab      
                                       AND es_import_ppr.cdn_funcionario = funcionario.cdn_funcionario
                                       AND es_import_ppr.cdn_cargo_basic = int(tt_es_import_ppr.cdn_cargo_basic) NO-LOCK NO-ERROR.
            IF NOT AVAIL es_import_ppr THEN DO:
          
                FIND FIRST tt-func-per WHERE tt-func-per.cdn_empresa     = string(INT(tt_es_import_ppr.cdn_empresa))
                                         AND tt-func-per.cdn_estab       = STRING(INT(tt_es_import_ppr.cdn_estab))       
                                         AND tt-func-per.cdn_funcionario = tt_es_import_ppr.cdn_funcionario 
                                         AND tt-func-per.cdn_cargo_basic = tt_es_import_ppr.cdn_cargo_basic NO-LOCK NO-ERROR.

                FIND FIRST es_import_ppr WHERE es_import_ppr.cdn_empresa     = funcionario.cdn_empresa       
                                           AND es_import_ppr.cdn_estab       = funcionario.cdn_estab         
                                           AND es_import_ppr.cdn_funcionario = funcionario.cdn_funcionario   
                                           AND es_import_ppr.cdn_cargo_basic = int(tt_es_import_ppr.cdn_cargo_basic) NO-LOCK NO-ERROR.
                
                IF NOT AVAIL es_import_ppr THEN DO:


                    FIND LAST histor_sal_fun WHERE  histor_sal_fun.cdn_estab      = tt_es_import_ppr.cdn_estab
                                                AND histor_sal_fun.cdn_funcionario = tt_es_import_ppr.cdn_funcionario
                                                AND histor_sal_fun.cdn_cargo_basic = tt_es_import_ppr.cdn_cargo_basic
                                                AND year(histor_sal_func.dat_liber_sal) = tt_es_import_ppr.num_anotip
                                                    NO-LOCK NO-ERROR.

                    IF AVAIL histor_sal_func  THEN
                            ASSIGN d-salario = histor_sal_func.val_salario_categ.
                        ELSE 
                            ASSIGN d-salario = funcionario.val_salario_atual.

                    IF tt-func-per.dt-fim > date("31/12/" + STRING(tt_es_import_ppr.num_anotip)) THEN
                        ASSIGN tt-func-per.dt-fim = date("31/12/" + STRING(tt_es_import_ppr.num_anotip)).

                    CREATE es_import_ppr.
                    ASSIGN es_import_ppr.cdn_empresa         = funcionario.cdn_empresa    
                           es_import_ppr.cdn_estab           = funcionario.cdn_estab      
                           es_import_ppr.cdn_funcionario     = funcionario.cdn_funcionario
                           es_import_ppr.cod_rh_ccusto       = funcionario.cod_rh_ccusto
                           es_import_ppr.cod_id_feder        = funcionario.cod_id_feder
                           es_import_ppr.cdn_cargo_basic     = int(tt_es_import_ppr.cdn_cargo_basic)
                           es_import_ppr.val_percent         = DEC(tt_es_import_ppr.val_percent) 
                           es_import_ppr.num_anotip          = tt_es_import_ppr.num_anotip  
                           es_import_ppr.sequencia           = tt_es_import_ppr.sequencia
                           es_import_ppr.qti_avos            = tt-func-per.qti_avos WHEN AVAIL tt-func-per 
                           es_import_ppr.dat_inicio          = tt-func-per.dt-ini   WHEN AVAIL tt-func-per 
                           es_import_ppr.dat_fim             = tt-func-per.dt-fim   WHEN AVAIL tt-func-per
                           es_import_ppr.val_salario_period  = d-salario
                           es_import_ppr.cdn_empresa_dest     = tt-func-per.cdn_empresa_dest
                           es_import_ppr.cdn_estab_dest       = tt-func-per.cdn_estab_dest      
                           es_import_ppr.cdn_funcionario_dest = tt-func-per.cdn_funcionario_dest
                           es_import_ppr.log_livre_1          = tt_es_import_ppr.log_livre_1   .
                    IF es_import_ppr.dat_fim > date("31/12/" + STRING(es_import_ppr.num_anotip)) THEN
                        MESSAGE  es_import_ppr.dat_fim 
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.

                END.
                
                FIND FIRST es_movto_arq WHERE es_movto_arq.cdn_empres = funcionario.cdn_empresa 
                                          AND es_movto_arq.num_anotip = tt_es_import_ppr.num_anotip  NO-LOCK NO-ERROR.

                IF NOT AVAIL es_movto_arq THEN DO:
                    CREATE es_movto_arq.
                    ASSIGN es_movto_arq.cdn_empres    = funcionario.cdn_empresa 
                           es_movto_arq.num_anotip    = tt_es_import_ppr.num_anotip
                           es_movto_arq.arq_importado = tt-param.arq-entrada
                           es_movto_arq.dt_importacao = TODAY.
                END.
              
            END.             
        END.
    END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-reg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-reg Procedure 
PROCEDURE pi-cria-reg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.
DEFINE BUFFER bf_es_import_ppr FOR es_import_ppr.

/*OUTPUT TO "D:\quarentena\alteracao_funcao_a.csv".
PUT UNFORMAT "Estab;Funcionario; Cargo; Dt. Liberacao; Nivel Cargo" SKIP.*/
FOR EACH es_import_ppr WHERE es_import_ppr.qti_avos < 12 NO-LOCK:
    
    FIND FIRST funcionario WHERE funcionario.cdn_empresa     = es_import_ppr.cdn_empresa
                             AND funcionario.cdn_estab       = es_import_ppr.cdn_estab      
                             AND funcionario.cdn_funcionario = es_import_ppr.cdn_funcionario
                             /*AND funcionario.cdn_cargo_basic = tt-func-per.cdn_cargo_basic*/ NO-LOCK NO-ERROR.

    FOR EACH histor_sal_func WHERE histor_sal_func.cdn_empresa     = funcionario.cdn_empresa    
                               AND histor_sal_func.cdn_estab       = funcionario.cdn_estab      
                               AND histor_sal_func.cdn_funcionario = funcionario.cdn_funcionario 
                               AND histor_sal_func.cdn_cargo_basic = es_import_ppr.cdn_cargo_basic NO-LOCK BREAK BY histor_sal_func.dat_liber_sal:

         FIND FIRST cargo WHERE cargo.cdn_cargo_basic = histor_sal_func.cdn_cargo_basic 
                           AND cargo.cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo
                           NO-LOCK NO-ERROR.
        IF YEAR(histor_sal_func.dat_liber_sal) = es_import_ppr.num_anotip THEN DO:

          /*  PUT histor_sal_func.cdn_estab     ";"
                funcionario.cdn_funcionario   ";"
                cargo.cdn_cargo_basic         ";"
                histor_sal_func.dat_liber_sal ";"
                cargo.cdn_niv_cargo           SKIP.*/

            CREATE tt-funcionario.                   
            ASSIGN tt-funcionario.cdn_empresa       = funcionario.cdn_empresa
                   tt-funcionario.cdn_estab         = histor_sal_func.cdn_estab    
                   tt-funcionario.cdn_funcionario   = funcionario.cdn_funcionario  
                   tt-funcionario.cdn_cargo_basic   = cargo.cdn_cargo_basic        
                   tt-funcionario.dat_liber_sal     = histor_sal_func.dat_liber_sal
                   tt-funcionario.cdn_niv_cargo     = cargo.cdn_niv_cargo   
                   tt-funcionario.num_anotip        = es_import_ppr.num_anotip .      
                   

        END.

    END.
END.
/*OUTPUT CLOSE.*/

FOR EACH tt-funcionario:


    FIND FIRST tt-funcionario-tot WHERE tt-funcionario-tot.cdn_empresa       = tt-funcionario.cdn_empresa      
                                    and tt-funcionario-tot.cdn_estab         = tt-funcionario.cdn_estab        
                                    and tt-funcionario-tot.cdn_funcionario   = tt-funcionario.cdn_funcionario  
                                    and tt-funcionario-tot.cdn_cargo_basic   = tt-funcionario.cdn_cargo_basic 
                                    and tt-funcionario-tot.dat_liber_sal     = tt-funcionario.dat_liber_sal   NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-funcionario-tot THEN DO:
            CREATE tt-funcionario-tot.   
            ASSIGN tt-funcionario-tot.cdn_empresa       = tt-funcionario.cdn_empresa    
                   tt-funcionario-tot.cdn_estab         = tt-funcionario.cdn_estab      
                   tt-funcionario-tot.cdn_funcionario   = tt-funcionario.cdn_funcionario
                   tt-funcionario-tot.cdn_cargo_basic   = tt-funcionario.cdn_cargo_basic
                   tt-funcionario-tot.dat_liber_sal     = tt-funcionario.dat_liber_sal  
                   tt-funcionario-tot.cdn_niv_cargo     = tt-funcionario.cdn_niv_cargo 
                   tt-funcionario-tot.num_anotip        = tt-funcionario.num_anotip .
        END.
     
    FIND LAST histor_sal_func WHERE histor_sal_func.cdn_empresa     = tt-funcionario.cdn_empresa    
                               AND histor_sal_func.cdn_estab       = tt-funcionario.cdn_estab      
                               AND histor_sal_func.cdn_funcionario = tt-funcionario.cdn_funcionario 
                               AND histor_sal_func.dat_liber_sal   < tt-funcionario.dat_liber_sal   NO-LOCK NO-ERROR.

    IF AVAIL histor_sal_func THEN DO:

        FIND FIRST tt-funcionario-tot WHERE tt-funcionario-tot.cdn_empresa       = tt-funcionario.cdn_empresa      
                                        and tt-funcionario-tot.cdn_estab         = tt-funcionario.cdn_estab        
                                        and tt-funcionario-tot.cdn_funcionario   = tt-funcionario.cdn_funcionario  
                                        and tt-funcionario-tot.cdn_cargo_basic   = histor_sal_func.cdn_cargo_basic 
                                        and tt-funcionario-tot.dat_liber_sal     = histor_sal_func.dat_liber_sal   NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-funcionario-tot THEN DO:
             CREATE tt-funcionario-tot.                   
             ASSIGN tt-funcionario-tot.cdn_empresa       = tt-funcionario.cdn_empresa
                    tt-funcionario-tot.cdn_estab         = tt-funcionario.cdn_estab    
                    tt-funcionario-tot.cdn_funcionario   = tt-funcionario.cdn_funcionario  
                    tt-funcionario-tot.cdn_cargo_basic   = histor_sal_func.cdn_cargo_basic        
                    tt-funcionario-tot.dat_liber_sal     = histor_sal_func.dat_liber_sal
                    tt-funcionario-tot.cdn_niv_cargo     = histor_sal_func.cdn_niv_cargo 
                    tt-funcionario-tot.num_anotip        = tt-funcionario.num_anotip   .      
        END.
       
    END.

END.

/*OUTPUT TO "D:\quarentena\alteracao_funcao.csv".
PUT UNFORMAT "Empresa;Estab;Funcionario; Cargo;Desc.Cargo; Dt. Liberacao; Nivel Cargo;Nivel Func.;Qtd.Sal;Cria TB" SKIP.*/
FOR EACH tt-funcionario-tot  /*WHERE tt-funcionario-tot.cdn_funcionario = 11649*/ /*11649*/
                             BY tt-funcionario-tot.cdn_funcionario
                             BY tt-funcionario-tot.dat_liber_sal DESCENDING:

     FIND FIRST cargo WHERE cargo.cdn_cargo_basic = tt-funcionario-tot.cdn_cargo_basic 
                           AND cargo.cdn_niv_cargo   = tt-funcionario-tot.cdn_niv_cargo
                           NO-LOCK NO-ERROR.

     FIND FIRST es_controle_pag WHERE es_controle_pag.cdn_empresa          = tt-funcionario-tot.cdn_empresa  
                                  AND es_controle_pag.cdn_estab            = tt-funcionario-tot.cdn_estab    
                                  AND es_controle_pag.num_anotip           = tt-funcionario-tot.num_anotip
                                  AND es_controle_pag.cdn_niv_hier_funcnal = cargo.cdn_niv_hier_funcnal
                                  NO-LOCK NO-ERROR. 

     FIND FIRST es_import_ppr WHERE es_import_ppr.cdn_empresa     = tt-funcionario-tot.cdn_empresa     
                                AND es_import_ppr.cdn_estab       = tt-funcionario-tot.cdn_estab        
                                AND es_import_ppr.cdn_funcionario = tt-funcionario-tot.cdn_funcionario  
                                AND es_import_ppr.cdn_cargo_basic = tt-funcionario-tot.cdn_cargo_basic  NO-LOCK NO-ERROR.
     IF NOT AVAIL es_import_ppr THEN DO:
   

         FIND FIRST bf_es_import_ppr WHERE bf_es_import_ppr.cdn_empresa  = tt-funcionario-tot.cdn_empresa                         
                                    AND bf_es_import_ppr.cdn_estab       = tt-funcionario-tot.cdn_estab                           
                                    AND bf_es_import_ppr.cdn_funcionario = tt-funcionario-tot.cdn_funcionario                     
                                    /*AND bf_es_import_ppr.cdn_cargo_basic = tt-funcionario-tot.cdn_cargo_basic*/  NO-LOCK NO-ERROR.  

         IF AVAIL bf_es_import_ppr THEN DO:

        
             
             CREATE tt_es_import_ppr.                                                                          
             ASSIGN tt_es_import_ppr.num_anotip      = bf_es_import_ppr.num_anotip        
                    tt_es_import_ppr.cdn_empresa     = bf_es_import_ppr.cdn_empresa    
                    tt_es_import_ppr.cdn_estab       = bf_es_import_ppr.cdn_estab      
                    tt_es_import_ppr.cod_rh_ccusto   = bf_es_import_ppr.cod_rh_ccusto  
                    tt_es_import_ppr.cdn_funcionario = bf_es_import_ppr.cdn_funcionario
                    tt_es_import_ppr.cod_id_feder    = bf_es_import_ppr.cod_id_feder   
                    tt_es_import_ppr.cdn_cargo_basic = tt-funcionario-tot.cdn_cargo_basic 
                    tt_es_import_ppr.val_percent     = bf_es_import_ppr.val_percent  
                    tt_es_import_ppr.sequencia       = i-linha    
                    tt_es_import_ppr.log_livre_1     = NO
                    i-linha = i-linha + 1.                                                                     
         END.





     END.
     ELSE DO:

     /*   PUT tt-funcionario-tot.cdn_empresa     ";"
            tt-funcionario-tot.cdn_estab       ";"
            tt-funcionario-tot.cdn_funcionario ";"
            tt-funcionario-tot.cdn_cargo_basic ";"
            cargo.des_cargo                    ";"
            tt-funcionario-tot.dat_liber_sal   ";"
            tt-funcionario-tot.cdn_niv_cargo   ";" 
            cargo.cdn_niv_hier_funcnal         ";"
            es_controle_pag.num_qtdsal         ";"
            "Nao" SKIP.*/

     END.




END.
/*OUTPUT CLOSE.              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-importa-planilha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importa-planilha Procedure 
PROCEDURE pi-importa-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first param-global no-lock.

DEFINE VARIABLE c-ano AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-qtd-avos AS INTEGER     NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Inicializando Atualizacao...").

DEFINE VAR i-linha  AS   INT INITIAL 0.

CREATE "Excel.Application" chExcel NO-ERROR.

IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
DO:
    RUN utp/ut-msgs.p (INPUT "show",
                       INPUT 17006,
                       INPUT "Nao foi possivel abrir o Excel").
    RETURN.
END.

chExcel:VISIBLE = FALSE.
chLivro = chExcel:WorkBooks:OPEN( tt-param.arq-entrada ) NO-ERROR.
chFolha = chLivro:Sheets:ITEM(1).

EMPTY TEMP-TABLE tt_es_import_ppr.
def var c-data as char no-undo.

ASSIGN i-linha = 2.
REPEAT:
    IF  chFolha:Range("A" + STRING(i-linha)):VALUE = "" OR  chFolha:Range("A" + STRING(i-linha)):VALUE = ? THEN
    DO:
        LEAVE.
    END.

    RUN pi-acompanhar IN h-acomp (INPUT STRING(int(chFolha:Range("A" + STRING(i-linha)):VALUE))).

    ASSIGN c-ano = "".

    CREATE tt_es_import_ppr.              
    ASSIGN tt_es_import_ppr.num_anotip      = INT(chFolha:Range("A" + STRING(i-linha)):VALUE)  
           tt_es_import_ppr.cdn_empresa     = string(INT(chFolha:Range("B" + STRING(i-linha)):VALUE))  
           tt_es_import_ppr.cdn_estab       = STRING(int(chFolha:Range("C" + STRING(i-linha)):VALUE))       
           tt_es_import_ppr.cod_rh_ccusto   = (chFolha:Range("D" + STRING(i-linha)):VALUE)  
           tt_es_import_ppr.cdn_funcionario = Int(chFolha:Range("F" + STRING(i-linha)):VALUE)  
           tt_es_import_ppr.cod_id_feder    = (chFolha:Range("H" + STRING(i-linha)):VALUE ) 
           tt_es_import_ppr.cdn_cargo_basic = int(chFolha:Range("I" + STRING(i-linha)):VALUE)  
           tt_es_import_ppr.val_percent     = dec(chFolha:Range("J" + STRING(i-linha)):VALUE)
           tt_es_import_ppr.log_livre_1     = YES
           tt_es_import_ppr.sequencia       = i-linha
           i-linha = i-linha + 1.

END.

run pi-finalizar in h-acomp.

chExcel:QUIT.

RELEASE OBJECT chExcel no-error. 
RELEASE OBJECT chLivro no-error.  
RELEASE OBJECT chFolha no-error.  

RUN pi-reimporta.

RUN pi-Validate.
RUN pi-cria-movto-es.


EMPTY TEMP-TABLE tt_es_import_ppr.

RUN pi-cria-reg.  /*Cria Registro que n∆o foi informado na Planilha - acordado com o cliente Yamana*/
RUN pi-Validate.
RUN pi-cria-movto-es.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-limpa-import) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa-import Procedure 
PROCEDURE pi-limpa-import :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER l-limpa AS LOG.
DEFINE INPUT PARAMETER i-anotip AS INT.
IF l-limpa = YES  THEN DO:
    FOR EACH es_import_ppr WHERE es_import_ppr.num_anotip = i-anotip:
        DELETE es_import_ppr.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-reimporta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-reimporta Procedure 
PROCEDURE pi-reimporta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE l-limpa AS LOGICAL   INITIAL YES  NO-UNDO.
DEFINE VARIABLE i-anotip AS INTEGER     NO-UNDO.
IF tt-param.l-reimp THEN DO:
   
    FOR EACH  tt_es_import_ppr:
        
        FOR EACH es_import_ppr WHERE es_import_ppr.cdn_empresa     = tt_es_import_ppr.cdn_empresa
                                 AND es_import_ppr.cdn_estab       = tt_es_import_ppr.cdn_estab    
                                 AND es_import_ppr.cdn_funcionario = tt_es_import_ppr.cdn_funcionario
                                 AND es_import_ppr.num_anotip      = tt_es_import_ppr.num_anotip 
                                 AND es_import_ppr.cdn_cargo_basic = tt_es_import_ppr.cdn_cargo_basic :
            
            IF tt_es_import_ppr.num_anotip > 0 THEN
                ASSIGN i-anotip = tt_es_import_ppr.num_anotip.
            
            FIND FIRST habilit_calc_ppr NO-LOCK WHERE habilit_calc_ppr.cdn_estab           = es_import_ppr.cdn_estab
                                                  /*AND habilit_calc_ppr.cdn_param_calc      = 100*/ NO-ERROR.
            IF AVAIL habilit_calc_ppr THEN DO:
                
                 FOR EACH PARAM_calc_ppr OF  habilit_calc_ppr NO-LOCK:
            
                     FIND FIRST funcionario WHERE funcionario.cdn_estab         = habilit_calc_ppr.cdn_estab     
                                              AND funcionario.cdn_funcionario   = es_import_ppr.cdn_funcionario
                                              NO-LOCK NO-ERROR.
            
                     IF AVAIL funcionario THEN DO:
                         
                         FIND FIRST cargo WHERE cargo.cdn_cargo_basic = funcionario.cdn_cargo_basic 
                                            AND cargo.cdn_niv_cargo   = funcionario.cdn_niv_cargo
                                            NO-LOCK NO-ERROR.
            
                         IF AVAIL cargo THEN DO:
            
                             FIND FIRST es_controle_pag WHERE es_controle_pag.cdn_empresa          = es_import_ppr.cdn_empresa
                                                          AND es_controle_pag.cdn_estab            = es_import_ppr.cdn_estab
                                                          AND es_controle_pag.num_anotip           = es_import_ppr.num_anotip
                                                          AND es_controle_pag.cdn_niv_hier_funcnal = cargo.cdn_niv_hier_funcnal
                                                          NO-LOCK NO-ERROR.                                                    
            
                             IF AVAIL es_controle_pag THEN DO:
                                                               
                                 FIND FIRST es_movto_ppr WHERE 
                                            es_movto_ppr.cdn_param_calc                 = habilit_calc_ppr.cdn_param_calc 
                                        AND es_movto_ppr.cdn_empresa                    = INT(es_controle_pag.cdn_empresa) 
                                        and es_movto_ppr.cdn_estab                      = INT(habilit_calc_ppr.cdn_estab)      
                                        and es_movto_ppr.cdn_funcionario                = es_import_ppr.cdn_funcionario                     
                                        and es_movto_ppr.num_mes_refer_calc_efetd       = habilit_calc_ppr.num_mes_refer_calc_efetd
                                        and es_movto_ppr.num_ano_refer_calc_efetd       = habilit_calc_ppr.num_ano_refer_calc_efetd
                                        and es_movto_ppr.idi_tip_folha_pagto_ppr        = habilit_calc_ppr.idi_tip_folha_pagto_ppr
                                        NO-ERROR.
                                 IF NOT AVAIL es_movto_ppr THEN DO:

                                         /*DELETE es_import_ppr. */

                                       /*  RUN pi-limpa-import.*/
                                          
                                    
                                 END.
                                 ELSE DO:
                                     
                                     ASSIGN l-limpa = NO.
                                     CREATE tt-log-import.
                                     ASSIGN tt-log-import.cdn_empresa       = int(es_controle_pag.cdn_empresa) .
                                            tt-log-import.cdn_estab         = int(habilit_calc_ppr.cdn_estab )      .
                                            tt-log-import.cdn_funcionario   = es_import_ppr.cdn_funcionario    .
                                            tt-log-import.desc-log          = "Erro! NAO SERA POSSIVEL A REIMPORTACAO - Calculo ja efetuado FP3130!".

                                 END.
                             END.
                         END.
                     END.
                 END.
            END.
                
        END.
         FOR EACH es_import_ppr WHERE es_import_ppr.cdn_empresa     = tt_es_import_ppr.cdn_empresa
                                 AND es_import_ppr.cdn_estab       = tt_es_import_ppr.cdn_estab    
                                 AND es_import_ppr.cdn_funcionario = tt_es_import_ppr.cdn_funcionario
                                 AND es_import_ppr.num_anotip      = tt_es_import_ppr.num_anotip
                                 AND es_import_ppr.log_livre_1     = NO :  /*Elimina Registros criados fora da planilha*/

             FIND FIRST habilit_calc_ppr NO-LOCK WHERE habilit_calc_ppr.cdn_estab           = es_import_ppr.cdn_estab
                                                  /*AND habilit_calc_ppr.cdn_param_calc      = 100*/ NO-ERROR.
            IF AVAIL habilit_calc_ppr THEN DO:
                
                 FOR EACH PARAM_calc_ppr OF  habilit_calc_ppr NO-LOCK:
            
                     FIND FIRST funcionario WHERE funcionario.cdn_estab         = habilit_calc_ppr.cdn_estab     
                                              AND funcionario.cdn_funcionario   = es_import_ppr.cdn_funcionario
                                              NO-LOCK NO-ERROR.
            
                     IF AVAIL funcionario THEN DO:
                         
                         FIND FIRST cargo WHERE cargo.cdn_cargo_basic = funcionario.cdn_cargo_basic 
                                            AND cargo.cdn_niv_cargo   = funcionario.cdn_niv_cargo
                                            NO-LOCK NO-ERROR.
            
                         IF AVAIL cargo THEN DO:
            
                             FIND FIRST es_controle_pag WHERE es_controle_pag.cdn_empresa          = es_import_ppr.cdn_empresa
                                                          AND es_controle_pag.cdn_estab            = es_import_ppr.cdn_estab
                                                          AND es_controle_pag.num_anotip           = es_import_ppr.num_anotip
                                                          AND es_controle_pag.cdn_niv_hier_funcnal = cargo.cdn_niv_hier_funcnal
                                                          NO-LOCK NO-ERROR.                                                    
            
                             IF AVAIL es_controle_pag THEN DO:
                                                               
                                 FIND FIRST es_movto_ppr WHERE 
                                            es_movto_ppr.cdn_param_calc                 = habilit_calc_ppr.cdn_param_calc 
                                        AND es_movto_ppr.cdn_empresa                    = INT(es_controle_pag.cdn_empresa) 
                                        and es_movto_ppr.cdn_estab                      = INT(habilit_calc_ppr.cdn_estab)      
                                        and es_movto_ppr.cdn_funcionario                = es_import_ppr.cdn_funcionario                     
                                        and es_movto_ppr.num_mes_refer_calc_efetd       = habilit_calc_ppr.num_mes_refer_calc_efetd
                                        and es_movto_ppr.num_ano_refer_calc_efetd       = habilit_calc_ppr.num_ano_refer_calc_efetd
                                        and es_movto_ppr.idi_tip_folha_pagto_ppr        = habilit_calc_ppr.idi_tip_folha_pagto_ppr
                                        NO-ERROR.
                                 IF NOT AVAIL es_movto_ppr THEN DO:

                                         DELETE es_import_ppr. 
                                          
                                    
                                 END.
                             END.
                         END.
                     END.
                 END.
            END.
         END.
    END.
END.

RUN pi-limpa-import(INPUT l-limpa, 
                    INPUT i-anotip).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-validate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate Procedure 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Inicializando Atualizacao...").


DEF VAR d-salario LIKE funcionario.val_salario_atual.
DEFINE VARIABLE dt-ini-func AS DATE   NO-UNDO.
DEFINE VARIABLE dt-fim-func AS DATE   NO-UNDO.

DEFINE VARIABLE i-qtd-avos AS INTEGER     NO-UNDO.

DEFINE BUFFER bf_histor_sal_fun FOR histor_sal_fun.

FOR EACH tt_es_import_ppr WHERE tt_es_import_ppr.cdn_empresa <> "" :
    
    FIND FIRST funcionario WHERE funcionario.cdn_empresa      = string(int(tt_es_import_ppr.cdn_empresa))
                             AND funcionario.cdn_estab       = string(int(tt_es_import_ppr.cdn_estab))
                             AND funcionario.cdn_funcionario = tt_es_import_ppr.cdn_funcionario
                              NO-LOCK NO-ERROR.
     IF AVAIL funcionario THEN DO:
         
         CREATE tt-func-per.
         ASSIGN tt-func-per.cdn_empresa     = funcionario.cdn_empresa     
                tt-func-per.cdn_estab       = funcionario.cdn_estab       
                tt-func-per.cdn_funcionario = funcionario.cdn_funcionario 
                tt-func-per.cdn_cargo_basic = tt_es_import_ppr.cdn_cargo_basic
                tt-func-per.num_anotip      = tt_es_import_ppr.num_anotip
                tt-func-per.val_percent     = tt_es_import_ppr.val_percent.

         FOR EACH histor_sal_fun OF funcionario NO-LOCK WHERE histor_sal_fun.cdn_cargo_basic = tt_es_import_ppr.cdn_cargo_basic BREAK BY histor_sal_fun.cdn_cargo_basic:
 
             IF FIRST-OF(histor_sal_fun.cdn_cargo_basic) THEN DO:
              
                ASSIGN tt-func-per.dt-ini = histor_sal_fun.dat_liber_sal.
             END.
             IF LAST-OF(histor_sal_fun.cdn_cargo_basic) THEN DO:
                ASSIGN tt-func-per.dt-fim = histor_sal_fun.dat_liber_sal.
                IF tt-func-per.dt-fim > date("31/12/" + STRING(tt_es_import_ppr.num_anotip)) THEN
                    ASSIGN tt-func-per.dt-fim = date("31/12/" + STRING(tt_es_import_ppr.num_anotip)).
             END.

             
         END.
       
     END.
END.


FOR EACH tt-func-per:

    FIND FIRST funcionario WHERE funcionario.cdn_empresa     = tt-func-per.cdn_empresa 
                             AND funcionario.cdn_estab       = tt-func-per.cdn_estab
                             AND funcionario.cdn_funcionario = tt-func-per.cdn_funcionario
                             NO-LOCK NO-ERROR.
    
    FIND FIRST histor_sal_fun OF funcionario WHERE histor_sal_fun.dat_liber_sal > tt-func-per.dt-fim NO-LOCK NO-ERROR.
    IF AVAIL histor_sal_fun THEN 
        ASSIGN tt-func-per.dt-fim = histor_sal_fun.dat_liber_sal - 1.
    ELSE 
        ASSIGN tt-func-per.dt-fim = date("31/12/" + string(tt-func-per.num_anotip)). 
       
    IF tt-func-per.dt-fim < date("01/01/" + string(tt-func-per.num_anotip)) THEN DO:
        ASSIGN tt-func-per.dt-ini = date("01/01/" + string(tt-func-per.num_anotip))
               tt-func-per.dt-fim = date("31/12/" + string(tt-func-per.num_anotip)).
    END.

    IF tt-func-per.dt-ini < date("01/01/" + string(tt-func-per.num_anotip)) THEN DO:

        ASSIGN tt-func-per.dt-ini = date("01/01/" + string(tt-func-per.num_anotip)).
       
    END.

    FOR EACH sit_afast_func 
       WHERE sit_afast_func.cdn_empresa                 = tt-func-per.cdn_empresa    
         AND sit_afast_func.cdn_estab                   = tt-func-per.cdn_estab      
         AND sit_afast_func.cdn_funcionario             = tt-func-per.cdn_funcionario 
         AND sit_afast_func.dat_inic_sit_afast         >= tt-func-per.dt-ini 
         AND sit_afast_func.dat_term_sit_afast         <= TODAY
         AND (sit_afast_func.cdn_sit_afast_func = 47)
         NO-LOCK:
         
           ASSIGN tt-func-per.cdn_empresa_dest     = sit_afast_func.cdn_empres_dest
                   tt-func-per.cdn_estab_dest       = sit_afast_func.cdn_estab_dest       
                   tt-func-per.cdn_funcionario_dest = sit_afast_func.cdn_func_dest 
                   tt-func-per.dt-fim               = sit_afast_func.dat_inic_sit_afast.

    END.

    FOR EACH sit_afast_func 
       WHERE sit_afast_func.cdn_empresa                 = tt-func-per.cdn_empresa    
         AND sit_afast_func.cdn_estab                   = tt-func-per.cdn_estab      
         AND sit_afast_func.cdn_funcionario             = tt-func-per.cdn_funcionario 
         AND sit_afast_func.dat_inic_sit_afast         >= tt-func-per.dt-ini 
         AND sit_afast_func.dat_term_sit_afast         <= tt-func-per.dt-fim 
         AND (sit_afast_func.cdn_sit_afast_func = 40 OR  sit_afast_func.cdn_sit_afast_func = 45)
         NO-LOCK:

        IF sit_afast_func.cdn_sit_afast_func = 40 THEN DO:
           
            ASSIGN tt-func-per.dt-ini = sit_afast_func.dat_inic_sit_afast.
          
        END.
        ELSE DO:
           
            ASSIGN tt-func-per.dt-fim = sit_afast_func.dat_term_sit_afast.
               
          
        END.
       

    END.
   

    FOR EACH es_controle_sit WHERE es_controle_sit.cdn_empresa = tt-func-per.cdn_empresa  
                               AND es_controle_sit.cdn_estab   = tt-func-per.cdn_estab  
                               AND es_controle_sit.num_anotip  = tt-func-per.num_anotip NO-LOCK:
        
        ASSIGN i-qtd-avos = 0.
       
        FOR EACH sit_afast_func 
           WHERE sit_afast_func.cdn_empresa                 = tt-func-per.cdn_empresa    
             AND sit_afast_func.cdn_estab                   = tt-func-per.cdn_estab      
             AND sit_afast_func.cdn_funcionario             = tt-func-per.cdn_funcionario 
            /* AND (year(sit_afast_func.dat_inic_sit_afast)   = tt-func-per.num_anotip 
              OR year(sit_afast_func.dat_term_sit_afast)    = tt-func-per.num_anotip  )*/
             AND (sit_afast_func.cdn_sit_afast_func = es_controle_sit.cdn_sit_afast_func) /*Buscar na tabela de cadastro*/
             NO-LOCK:
                                                                                   
           ASSIGN dt-ini-func = ?
                  dt-fim-func = ?.

            IF year(sit_afast_func.dat_inic_sit_afast) <=  es_controle_sit.num_anotip THEN DO:
                ASSIGN dt-ini-func = DATE("01/01/" + STRING(es_controle_sit.num_anotip)).
                IF sit_afast_func.dat_inic_sit_afast <= date("31/12/" + STRING(es_controle_sit.num_anotip)) THEN
                    ASSIGN dt-ini-func = sit_afast_func.dat_inic_sit_afast .
            END.
            

            IF (sit_afast_func.dat_term_sit_afast) >=  DATE("01/01/" + STRING(es_controle_sit.num_anotip)) THEN DO:
                
                ASSIGN dt-fim-func = sit_afast_func.dat_term_sit_afast.
                IF dt-fim-func > DATE("31/12/" +  STRING(es_controle_sit.num_anotip)) THEN
                    ASSIGN dt-fim-func = DATE("31/12/" +  STRING(es_controle_sit.num_anotip)).

            END.

            RUN pi-acompanhar IN h-acomp (INPUT "1A - " + STRING(dt-ini-func) +  " - " + STRING(dt-fim-func)).
            IF dt-fim-func <> ?  THEN DO:
                
                 RUN pi-avos(INPUT date(dt-ini-func),
                      INPUT date(dt-fim-func),
                      INPUT (es_controle_sit.num_anotip),
                      OUTPUT i-qtd-avos).
            END.


                            
            ASSIGN tt-func-per.qti_avos = (tt-func-per.qti_avos - i-qtd-avos) .
            IF i-qtd-avos > 0 THEN
                   ASSIGN tt-func-per.desc-log = tt-func-per.desc-log + " - Perda de " + string(i-qtd-avos) + " Avos por " + STRING(es_controle_sit.cdn_sit_afast_func)
                          tt-func-per.l-perda  = YES.

                
        END.
    
        ASSIGN dt-ini-func = ?
               dt-ini-func = ?.
        IF tt-func-per.qti_avos = 0 THEN DO:
            FOR EACH sit_afast_func 
               WHERE sit_afast_func.cdn_empresa                 = tt-func-per.cdn_empresa    
                 AND sit_afast_func.cdn_estab                   = tt-func-per.cdn_estab      
                 AND sit_afast_func.cdn_funcionario             = tt-func-per.cdn_funcionario 
                 AND sit_afast_func.dat_inic_sit_afast   <= tt-func-per.dt-ini
                 AND sit_afast_func.dat_term_sit_afast   <= tt-func-per.dt-fim 
                 AND (sit_afast_func.cdn_sit_afast_func = es_controle_sit.cdn_sit_afast_func)
                 NO-LOCK:
    
                IF sit_afast_func.dat_inic_sit_afast < tt-func-per.dt-ini  AND sit_afast_func.dat_term_sit_afast < tt-func-per.dt-ini THEN NEXT.
                   
                IF year(sit_afast_func.dat_inic_sit_afast) <  es_controle_sit.num_anotip THEN
                    ASSIGN dt-ini-func = DATE("01/01/" + STRING(tt-func-per.num_anotip)).

                IF year(sit_afast_func.dat_term_sit_afast) >  es_controle_sit.num_anotip THEN
                    ASSIGN dt-fim-func = DATE("31/12/" + STRING(tt-func-per.num_anotip)).
               
                RUN pi-acompanhar IN h-acomp (INPUT "A - " + STRING(dt-ini-func) +  " - " + STRING(dt-fim-func)).

                RUN pi-avos(INPUT date(dt-ini-func),
                            INPUT date(dt-ini-func),
                            INPUT int(tt-func-per.num_anotip ),
                            OUTPUT i-qtd-avos).
                 
                ASSIGN tt-func-per.qti_avos = (tt-func-per.qti_avos - i-qtd-avos) .
                IF i-qtd-avos > 0 THEN
                    ASSIGN tt-func-per.desc-log = tt-func-per.desc-log + " - Perda de " + string(i-qtd-avos) + " Avos por " + STRING(es_controle_sit.cdn_sit_afast_func)
                           tt-func-per.l-perda  = YES.
            
            END.
        END.

         ASSIGN dt-ini-func = ?
               dt-ini-func = ?.
        IF tt-func-per.qti_avos = 0 THEN DO:
            FOR EACH sit_afast_func 
               WHERE sit_afast_func.cdn_empresa                 = tt-func-per.cdn_empresa    
                 AND sit_afast_func.cdn_estab                   = tt-func-per.cdn_estab      
                 AND sit_afast_func.cdn_funcionario             = tt-func-per.cdn_funcionario 
                 AND sit_afast_func.dat_inic_sit_afast   >= tt-func-per.dt-ini
                 AND sit_afast_func.dat_term_sit_afast   <= tt-func-per.dt-fim 
                 AND (sit_afast_func.cdn_sit_afast_func = es_controle_sit.cdn_sit_afast_func)
                 NO-LOCK:
    
                IF sit_afast_func.dat_inic_sit_afast < tt-func-per.dt-ini  AND sit_afast_func.dat_term_sit_afast < tt-func-per.dt-ini THEN NEXT.
                
                IF year(sit_afast_func.dat_inic_sit_afast) <  es_controle_sit.num_anotip THEN
                    ASSIGN dt-ini-func = DATE("01/01/" + STRING(tt-func-per.num_anotip)).

                IF year(sit_afast_func.dat_term_sit_afast) >  es_controle_sit.num_anotip THEN
                    ASSIGN dt-fim-func = DATE("31/12/" + STRING(tt-func-per.num_anotip)).
               
                RUN pi-acompanhar IN h-acomp (INPUT "B - " + STRING(dt-ini-func) +  " - " + STRING(dt-fim-func)).
                RUN pi-avos(INPUT date(dt-ini-func),
                            INPUT date(dt-ini-func),
                            INPUT int(tt-func-per.num_anotip ),
                            OUTPUT i-qtd-avos).
                  
            
                ASSIGN tt-func-per.qti_avos = (tt-func-per.qti_avos - i-qtd-avos) .
                IF i-qtd-avos > 0 THEN
                    ASSIGN tt-func-per.desc-log = tt-func-per.desc-log + " - Perda de " + string(i-qtd-avos) + " Avos por " + STRING(es_controle_sit.cdn_sit_afast_func)
                           tt-func-per.l-perda  = YES.
                
            
            END.
        END.
                               
        IF tt-func-per.qti_avos < 0 THEN
                ASSIGN tt-func-per.qti_avos = tt-func-per.qti_avos + 12.

          
    END.
    
END.


FOR EACH tt-func-per WHERE tt-func-per.qti_avos = 0
                       AND tt-func-per.l-perda = NO:
  
    IF DAY(tt-func-per.dt-ini) > 15 THEN
        ASSIGN tt-func-per.qti_avos = (month(tt-func-per.dt-fim) - month(tt-func-per.dt-ini)) .
    ELSE DO:
        RUN pi-acompanhar IN h-acomp (INPUT "C -" + STRING( tt-func-per.dt-ini) +  " - " + STRING(tt-func-per.dt-fim)).

        RUN pi-avos(INPUT tt-func-per.dt-ini,
                    INPUT tt-func-per.dt-fim,
                    INPUT int(tt-func-per.num_anotip ),
                    OUTPUT i-qtd-avos).
        ASSIGN tt-func-per.qti_avos = i-qtd-avos.

    END.
        


    IF tt-func-per.dt-ini = ? THEN DO:
        FIND FIRST funcionario WHERE funcionario.cdn_empresa     = tt-func-per.cdn_empresa    
                                 AND funcionario.cdn_estab       = tt-func-per.cdn_estab      
                                 AND funcionario.cdn_funcionario = tt-func-per.cdn_funcionario
                                 AND funcionario.cdn_cargo_basic = tt-func-per.cdn_cargo_basic NO-LOCK NO-ERROR.
        IF NOT AVAIL funcionario THEN DO:
            ASSIGN tt-func-per.dt-fim = ?.
            ASSIGN tt-func-per.desc-log = "Funcionario nao encontrado Emp/Estab/Matr/Cargo - " + tt-func-per.cdn_empresa             + "/"  
                                                                                               + tt-func-per.cdn_estab               + "/"
                                                                                               + STRING(tt-func-per.cdn_funcionario) + "/"
                                                                                               + STRING(tt-func-per.cdn_cargo_basic) .
        END.
        ELSE DO:
            
            FIND LAST histor_sal_func WHERE histor_sal_func.cdn_empresa     = tt-func-per.cdn_empresa
                                        AND histor_sal_func.cdn_estab       = tt-func-per.cdn_estab     
                                        AND histor_sal_func.cdn_funcionario = tt-func-per.cdn_funcionario 
                                        AND histor_sal_func.cdn_cargo_basic = tt-func-per.cdn_cargo_basic NO-LOCK NO-ERROR.  

           IF AVAIL histor_sal_func THEN
           FIND FIRST cargo WHERE cargo.cdn_cargo_basic = tt-func-per.cdn_cargo_basic
                              AND cargo.cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo
                              NO-LOCK NO-ERROR.
           ELSE 
           FIND FIRST cargo WHERE cargo.cdn_cargo_basic = tt-func-per.cdn_cargo_basic
                         AND cargo.cdn_niv_cargo   = funcionario.cdn_niv_cargo
                         NO-LOCK NO-ERROR.


           IF AVAIL cargo THEN DO:      
                FIND FIRST es_controle_pag WHERE es_controle_pag.cdn_empresa          = tt-func-per.cdn_empresa   
                                             AND es_controle_pag.cdn_estab            = tt-func-per.cdn_estab     
                                             AND es_controle_pag.num_anotip           = tt-func-per.num_anotip
                                             AND es_controle_pag.cdn_niv_hier_funcnal = cargo.cdn_niv_hier_funcnal
                                             NO-LOCK NO-ERROR.   
                IF AVAIL es_controle_pag THEN DO: 
                    
                    IF AVAIL histor_sal_func  THEN
                            ASSIGN d-salario = histor_sal_func .val_salario_categ.
                        ELSE 
                            ASSIGN d-salario = funcionario.val_salario_atual.

                                                          
                    ASSIGN tt-func-per.val_calcul_simul = (((d-salario * es_controle_pag.num_qtdsal) / 12) * tt-func-per.qti_avos) * ( 1 * tt-func-per.val_percent ).
                END.

            END.
        END.

    END.

END.
FOR EACH tt-func-per:
    
    FIND FIRST funcionario WHERE funcionario.cdn_estab         = tt-func-per.cdn_estab          
                             AND funcionario.cdn_funcionario   = tt-func-per.cdn_funcionario  
                             NO-LOCK NO-ERROR.

    IF AVAIL funcionario THEN DO:

        FIND FIRST cargo WHERE cargo.cdn_cargo_basic = funcionario.cdn_cargo_basic 
                           AND cargo.cdn_niv_cargo   = funcionario.cdn_niv_cargo
                           NO-LOCK NO-ERROR.

       IF AVAIL cargo THEN DO:
       
           FIND FIRST es_controle_pag WHERE es_controle_pag.cdn_empresa          = tt-func-per.cdn_empresa   
                                        AND es_controle_pag.cdn_estab            = tt-func-per.cdn_estab     
                                        AND es_controle_pag.num_anotip           = tt-func-per.num_anotip
                                        AND es_controle_pag.cdn_niv_hier_funcnal = cargo.cdn_niv_hier_funcnal
                                        NO-LOCK NO-ERROR.   
           IF AVAIL es_controle_pag THEN DO:

               ASSIGN tt-func-per.val_calcul_simul = (((funcionario.val_salario_atual * es_controle_pag.num_qtdsal) / 12) * tt-func-per.qti_avos) * ( 1 * (tt-func-per.val_percent / 100))
                      tt-func-per.desc-calc        = "(((" + string(funcionario.val_salario_atual) + " * " + STRING(es_controle_pag.num_qtdsal) + ") / 12) * " +  string(tt-func-per.qti_avos) + " ) * ( 1 * " + string(tt-func-per.val_percent) + ")".
               
           END.
          

       END.
        
     END.
     ELSE DO:

         ASSIGN tt-func-per.desc-log = "Funcionario nao encontrado Emp/Estab/Matr/Cargo - " + tt-func-per.cdn_empresa             + "/"  
                                                                                            + tt-func-per.cdn_estab               + "/"
                                                                                            + STRING(tt-func-per.cdn_funcionario) + "/"
                                                                                            + STRING(tt-func-per.cdn_cargo_basic) .

        
     END.
     IF tt-func-per.cdn_empresa_dest  <> ""   THEN
         ASSIGN tt-func-per.desc-log = "Funcionario Transferido para Emp/Estab/Matr -" + tt-func-per.cdn_empresa_dest     + "/"  
                                                                                       + tt-func-per.cdn_estab_dest       + "/"  
                                                                                   + string(tt-func-per.cdn_funcionario_dest) .
END.


run pi-finalizar in h-acomp.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


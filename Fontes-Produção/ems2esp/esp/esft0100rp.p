&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* --------------------------------------------------------------------------------------- 
                                                                                        
   Sistema................: TOTVS                                                       
   Modulo.................:                                                  
                                                                                           
   Programa...............: esft0100                                          
   Sub Programa...........:                                                                
                                                                                            
   Descricao..............: Integra‡Æo TOTVS x MES
   programa para consultar: 
                             
   Entidade Desenvolvedora: DSC PRAXIS
   
   tabelas usadas.........: 
   importante.............:  
                                                                                           
   Historico Programa -------------------------------------------------------------------+ 
   | Data       | Autor               | Descricao                                        | 
   +----------- +---------------------+--------------------------------------------------+ 
   | 21/02/2017 | Marcos A.Souza      | Desenvolvimento do Programa                      | 
   +------------+---------------------+--------------------------------------------------+ */
   
{include/i-prgvrs.i esft0100rp 2.12.10.001}

{utp/ut-glob.i}                                              
{esp\esft0100.i}

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita       AS RAW.

/* definicao de parametros */
DEF INPUT PARAM raw-param  AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

/* transferencia de parametros */
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i} 

/* defini‡Æo de vari veis  */

DEFINE VARIABLE h_acomp           AS HANDLE                            NO-UNDO. 
DEFINE VARIABLE c-modelo          AS CHAR FORMAT "X(256)"              NO-UNDO.
DEFINE VARIABLE excelappl         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE workbooks         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE worksheets        AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE i-lin             AS INTEGER                           NO-UNDO.
DEFINE VARIABLE i-col             AS INTEGER                           NO-UNDO.

DEFINE VARIABLE dt-data-ini AS DATE        NO-UNDO.
DEFINE VARIABLE dt-data-fim AS DATE        NO-UNDO.

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
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
FIND FIRST tt-param NO-LOCK NO-ERROR.
IF NOT AVAIL tt-param THEN NEXT.

FIND FIRST param-global NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h_acomp.
RUN pi-inicializar IN h_acomp (INPUT "Gerando Resumo").

IF tt-param.mesCorent THEN DO: /*verifica se deve considerar o mes atual*/
dt-data-ini  = DATE(MONTH(TODAY), 01, YEAR(TODAY)).
dt-data-fim  = date(MONTH(TODAY), DAY(DATE(MONTH(DATE(MONTH(TODAY),25,YEAR(TODAY)) + 10), 01, YEAR(TODAY)) - 1) , YEAR(TODAY)). 
END.
ELSE IF tt-param.l-rpw THEN DO: /* pega um intervalo de 30 dias */
   ASSIGN dt-data-ini = TODAY - 30
          dt-data-fim = TODAY - 1.
END.
ELSE DO:

ASSIGN dt-data-ini = tt-param.dt-emis-nota-ini
       dt-data-fim = tt-param.dt-emis-nota-fim.
END.

run pi-importar.        

RUN pi-finalizar IN h_acomp.
{include\i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-importar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar Procedure 
PROCEDURE pi-importar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.idi-sit-nf-eletro = 3  /*autorizada*/
    AND   nota-fiscal.dt-emis-nota >= dt-data-ini 
    AND   nota-fiscal.dt-emis-nota <= dt-data-fim 
    AND   nota-fiscal.cod-estabel  >= tt-param.cod-estabel-ini 
    AND   nota-fiscal.cod-estabel  <= tt-param.cod-estabel-fim.

    RUN pi-acompanhar IN h_acomp (INPUT "Nota Fiscal:" + STRING( nota-fiscal.nr-nota-fis) + "  " +  STRING (nota-fiscal.dt-emis-nota)).

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK
        WHERE it-nota-fisc.it-codigo >= tt-param.it-codigo-ini
        AND   it-nota-fisc.it-codigo <= tt-param.it-codigo-fim
        AND   it-nota-fisc.ind-sit-nota  <> 4. /* Cancelada */
        FIND FIRST es_it_nota_fisc
/*             WHERE es_it_nota_fisc.ep_codigo   = i-ep-codigo-usuario     */
/*             AND   es_it_nota_fisc.cod_estabel = nota-fiscal.cod-estabel */
/*             AND   es_it_nota_fisc.nr_nota_fis = nota-fiscal.nr-nota-fis */
/*             AND   es_it_nota_fisc.serie       = nota-fiscal.serie       */
/*             AND   es_it_nota_fisc.nr_seq_fat  = it-nota-fisc.nr-seq-fat */
/*             AND   es_it_nota_fisc.it_codigo   = it-nota-fisc.it-codigo  */
             WHERE chave_nota_fisc = i-ep-codigo-usuario +  nota-fiscal.cod-estabel + nota-fiscal.nr-nota-fis + nota-fiscal.serie  + STRING(it-nota-fisc.nr-seq-fat) + it-nota-fisc.it-codigo 
         EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL es_it_nota_fisc THEN
        DO:
            CREATE es_it_nota_fisc.
               ASSIGN chave_nota_fisc = i-ep-codigo-usuario +  nota-fiscal.cod-estabel + nota-fiscal.nr-nota-fis + nota-fiscal.serie  + STRING(it-nota-fisc.nr-seq-fat) + it-nota-fisc.it-codigo.
/*             ASSIGN es_it_nota_fisc.ep_codigo         = i-ep-codigo-usuario     */
/*                    es_it_nota_fisc.cod_estabel       = nota-fiscal.cod-estabel */
/*                    es_it_nota_fisc.nr_nota_fis       = nota-fiscal.nr-nota-fis */
/*                    es_it_nota_fisc.serie             = nota-fiscal.serie       */
/*                    es_it_nota_fisc.nr_seq_fat        = it-nota-fisc.nr-seq-fat */
/*                    es_it_nota_fisc.it_codigo         = it-nota-fisc.it-codigo. */
       END.

       ASSIGN es_it_nota_fisc.idi_sit_nf_eletro = nota-fiscal.idi-sit-nf-eletro
              /**/
              es_it_nota_fisc.ep_codigo         = i-ep-codigo-usuario
              es_it_nota_fisc.cod_estabel       = nota-fiscal.cod-estabel
              es_it_nota_fisc.nr_nota_fis       = nota-fiscal.nr-nota-fis
              es_it_nota_fisc.serie             = nota-fiscal.serie
              es_it_nota_fisc.nr_seq_fat        = it-nota-fisc.nr-seq-fat
              es_it_nota_fisc.it_codigo         = it-nota-fisc.it-codigo
              /**/
              es_it_nota_fisc.dt_emis_nota      = it-nota-fisc.dt-emis-nota  
              es_it_nota_fisc.nat_operacao      = it-nota-fisc.nat-operacao
              es_it_nota_fisc.peso_bru_tot      = nota-fiscal.peso-bru-tot
              es_it_nota_fisc.peso_liq_tot      = nota-fiscal.peso-liq-tot
              es_it_nota_fisc.qt_faturada       = it-nota-fisc.qt-faturada[1]
              es_it_nota_fisc.cod_emitente      = nota-fiscal.cod-emitente
              es_it_nota_fisc.nome_ab_cli       = nota-fiscal.nome-ab-cli 
              es_it_nota_fisc.nome_transp       = nota-fiscal.nome-transp 
              es_it_nota_fisc.un_fatur          = it-nota-fisc.un-fatur[1] 
              es_it_nota_fisc.vl_tot_nota       = nota-fiscal.vl-tot-nota
              es_it_nota_fisc.placa             = nota-fiscal.placa.
          
       
       FOR FIRST ITEM NO-LOCK
           WHERE ITEM.it-codigo = it-nota-fisc.it-codigo.
           ASSIGN es_it_nota_fisc.desc_item = ITEM.desc-item.
       END.

       FOR FIRST estabelec NO-LOCK
           WHERE estabelec.cod-estabel = nota-fiscal.cod-estabel.
           ASSIGN es_it_nota_fisc.desc_estab = estabelec.nome.
       END.

       FOR FIRST ems2cadme.empresa NO-LOCK
           WHERE ems2cadme.empresa.ep-codigo = i-ep-codigo-usuario.

           ASSIGN es_it_nota_fisc.desc_empresa = ems2cadme.empresa.razao-social.
       END.

       FOR FIRST natur-oper NO-LOCK
           WHERE natur-oper.nat-operacao = it-nota-fisc.nat-operacao.

           ASSIGN es_it_nota_fisc.desc_nat_oper = natur-oper.denominacao.
       END.

       FOR LAST  ret-nf-eletro NO-LOCK
           WHERE ret-nf-eletro.cod-estabel  = nota-fiscal.cod-estabel  
           AND   ret-nf-eletro.cod-serie    = nota-fiscal.serie    
           AND   ret-nf-eletro.nr-nota-fis  = nota-fiscal.nr-nota-fis     
           BY ret-nf-eletro.dat-ret 
           BY ret-nf-eletro.hra-ret.

           ASSIGN es_it_nota_fisc.cod_msg =  ret-nf-eletro.cod-msg 
                  es_it_nota_fisc.dat_ret =  ret-nf-eletro.dat-ret
                  es_it_nota_fisc.hra_ret =  ret-nf-eletro.hra-ret.

       END.
    END.
END.        


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


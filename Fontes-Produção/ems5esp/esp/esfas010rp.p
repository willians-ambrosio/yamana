&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*******************************************************************************
** Programa : ESIN0010RP
** Descriªío: IMPORTAÄ«O DE CONTRATOS ATRAVêS DE PLANILHA
** Autor    : Maurilio - 07-04-2016
*******************************************************************************/

{include/i-prgvrs.i ESCN0206RP 12.10.1.000}

/* ***************************  Definitions  ************************** */
{utp/ut-glob.i}

/*------------- Definiá∆o de Temp-Table --------------------------------------*/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    field cod_cta_pat_fim          as char
    field cod_cta_pat_ini          as char
    field cod_empresa_fim          as char
    field cod_empresa_ini          as char
    field num_bem_pat_fim          as INT
    field num_bem_pat_ini          as INT
    field num_seq_bem_pat_fim      as INT
    field num_seq_bem_pat_ini      as INT
    FIELD dat_aquis_bem_pat_ini    AS DATE
    FIELD dat_aquis_bem_pat_fim    AS DATE
    FIELD cod_estab_ini            AS CHAR
    FIELD cod_estab_fim            AS CHAR
    field dat_sdo_bem_pat          as DATE
    FIELD gera-planilha-recalc     AS LOG
    FIELD atualiza-recalc          AS LOG
    FIELD atualiza-param           AS LOG
    FIELD cod_cenar_ctbl_ori       AS CHAR
    FIELD cod_finalid_econ_ori     AS CHAR
    FIELD cod_cenar_ctbl_des       AS CHAR
    FIELD cod_finalid_econ_cor_des AS CHAR
    FIELD plan-destino             AS CHAR
    .

Define Temp-Table tt-raw-digita Field raw-digita As Raw.

/*-------------- Definiªío Par¸metros -----------------------------------*/
Define Input Parameter raw-param As Raw No-Undo.
Define Input Parameter Table For tt-raw-digita.

/*------------- DefiniØ o de Buffers ------------------------------------*/

/*--------------- DefiniØ o de Variaveis --------------------------------*/
def var h-acomp                AS HANDLE     NO-UNDO.
def var ct                     as int        NO-UNDO.

DEFINE VARIABLE i-linha                AS INTEGER    NO-UNDO.
DEFINE VARIABLE chexcelapplication     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet            AS COM-HANDLE NO-UNDO.

DEF TEMP-TABLE tt-ord-inv-item NO-UNDO
    FIELD num-ord-inv    LIKE item-contrat-ext.num-ord-inv
    FIELD num-seq-item   LIKE item-contrat-ext.num-seq-item
    FIELD it-codigo      LIKE ITEM.it-codigo
    INDEX id01 num-seq-item.

DEF TEMP-TABLE tt-dados
    FIELD cod_empresa                 like bem_pat.cod_empresa     
    FIELD cod_cta_pat                 like bem_pat.cod_cta_pat     
    FIELD num_bem_pat                 like bem_pat.num_bem_pat     
    FIELD num_seq_bem_pat             like bem_pat.num_seq_bem_pat 
    FIELD num_seq_incorp_bem_pat      LIKE sdo_bem_pat.num_seq_incorp_bem_pat
    FIELD num_seq_incorp_bem_pat_des  LIKE sdo_bem_pat.num_seq_incorp_bem_pat
    FIELD num_seq_movto_bem_pat      LIKE sdo_bem_pat.num_seq_movto_bem_pat 
    FIELD cod_estab                  LIKE bem_pat.cod_estab
    FIELD dat_aquis_bem_pat          LIKE bem_pat.dat_aquis_bem_pat
    FIELD num_id_bem_pat             LIKE bem_pat.num_id_bem_pat
    /* descr */
    FIELD des_bem_pat                LIKE bem_pat.des_bem_pat
    FIELD des_cta_pat                LIKE cta_pat.des_cta_pat  
    /***/
    FIELD valor_saldo_depre_calc     LIKE sdo_bem_pat.val_original
    FIELD taxa_depre_calc            LIKE sdo_bem_pat.val_original
    FIELD val_perc_anual_dpr         LIKE param_calc_bem_pat.val_perc_anual_dpr
    FIELD qtd_anos_vida_util         LIKE param_calc_bem_pat.qtd_anos_vida_util
    FIELD val_perc_anual_dpr_des     LIKE param_calc_bem_pat.val_perc_anual_dpr
    FIELD qtd_anos_vida_util_des     LIKE param_calc_bem_pat.qtd_anos_vida_util

    /***/
    FIELD perc_ja_depre              LIKE sdo_bem_pat.val_perc_dpr_acum
    FIELD valor_depreciado           LIKE sdo_bem_pat.val_dpr_val_origin
    FIELD valor_depreciado_des       LIKE sdo_bem_pat.val_dpr_val_origin  
    FIELD valor_corrigido            LIKE sdo_bem_pat.val_dpr_cm         
    FIELD valor_original_des         LIKE sdo_bem_pat.val_original

    /**/
    FIELD fator_recalc               AS DEC DECIMALS 6
    FIELD valor_original_des_cor     LIKE sdo_bem_pat.val_original
    FIELD valor_depreciado_des_cor   LIKE sdo_bem_pat.val_dpr_val_origin
    INDEX id01 cod_empresa    
               cod_cta_pat    
               num_bem_pat    
               num_seq_bem_pat.

DEFINE VARIABLE n-linhas AS INTEGER     NO-UNDO.

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
         HEIGHT             = 10.63
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
Create tt-param. 
Raw-Transfer raw-param To tt-param.
Find First tt-param No-Lock No-Error.
/*
FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.
*/

Find First param-global No-Lock No-Error.
/*Find ems5.empresa Where empresa.ep-codigo=param-global.empresa-prin No-Lock No-Error.*/
 
{include/i-rpvar.i}

{include/i-rpout.i /*&stream="stream str-rp"*/ &tofile=tt-param.arquivo}

Assign c-empresa      = "YAMANA" /*empresa.razao-social*/
       c-titulo-relat = "ALTERAÄ«O DE TAXA/VALORES DEPRECIAÄ«O"
       c-sistema      = ""
       c-programa     = "ESFAS010"
       c-versao       = "12.10"
       c-revisao      = "1.000".

{include/i-rpcab.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.    
/*----------------- DefiniØ o de Frames ----------------------------------*/

/*---------------- Bloco Principal ---------------------------------------*/

Run utp/ut-acomp.p persistent set h-acomp.

RUN pi-inicializar IN h-acomp ('Iniciando processo').

RUN pi-executar.

RUN pi-finalizar IN h-acomp.

{include/i-rpclo.i}

RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-atualiza-param) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-param Procedure 
PROCEDURE pi-atualiza-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-inicializar IN h-acomp ('Atualizando Parametros').

i-linha = 0.

FOR EACH tt-dados NO-LOCK
    /*WHERE tt-dados.fator_recalc <> 0*/.

    i-linha = i-linha + 1.
    RUN pi-acompanhar IN h-acomp("Registro: " + STRING(i-linha) + "/" + STRING(n-linhas)).

        
    FIND FIRST param_calc_bem_pat 
        WHERE param_calc_bem_pat.num_id_bem_pat = tt-dados.num_id_bem_pat
          AND param_calc_bem_pat.cod_tip_calc = 'DP'
          AND param_calc_bem_pat.cod_cenar_ctbl = tt-param.cod_cenar_ctbl_des
          AND param_calc_bem_pat.cod_finalid_econ = tt-param.cod_finalid_econ_cor_des
        NO-ERROR.
    IF NOT AVAIL param_calc_bem_pat THEN
        FIND FIRST param_calc_bem_pat 
             WHERE param_calc_bem_pat.num_id_bem_pat = tt-dados.num_id_bem_pat
               AND param_calc_bem_pat.cod_tip_calc = 'AM'
               AND param_calc_bem_pat.cod_cenar_ctbl = tt-param.cod_cenar_ctbl_des
               AND param_calc_bem_pat.cod_finalid_econ = tt-param.cod_finalid_econ_cor_des
             NO-ERROR.
    IF AVAIL param_calc_bem_pat THEN
        ASSIGN PARAM_calc_bem_pat.val_perc_anual_dpr = tt-dados.val_perc_anual_dpr
               PARAM_calc_bem_pat.qtd_anos_vida_util = tt-dados.qtd_anos_vida_util .

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-atualiza-recalc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-recalc Procedure 
PROCEDURE pi-atualiza-recalc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-inicializar IN h-acomp ('Atualizando Recalc').

i-linha = 0.

FOR EACH tt-dados NO-LOCK
    WHERE tt-dados.fator_recalc <> 0.

    i-linha = i-linha + 1.
    RUN pi-acompanhar IN h-acomp("Registro: " + STRING(i-linha) + "/" + STRING(n-linhas)).

    /* busca saldo destino a ser aplicado o fator , finalidade corrente */
    FIND FIRST sdo_bem_pat            
        WHERE sdo_bem_pat.num_id_bem_pat         = tt-dados.num_id_bem_pat        
          AND sdo_bem_pat.num_seq_incorp_bem_pat = tt-dados.num_seq_incorp_bem_pat_des
          AND sdo_bem_pat.cod_cenar_ctbl         = tt-param.cod_cenar_ctbl_des
          AND sdo_bem_pat.cod_finalid_econ       = tt-param.cod_finalid_econ_cor_des
          AND sdo_bem_pat.dat_sdo_bem_pat        = tt-param.dat_sdo_bem_pat       
          AND sdo_bem_pat.num_seq_movto_bem_pat  = tt-dados.num_seq_movto_bem_pat
        NO-ERROR.

    IF AVAIL sdo_bem_pat THEN DO:
        /*ASSIGN sdo_bem_pat.val_original  = tt-dados.valor_original_des_cor.*/
        ASSIGN sdo_bem_pat.val_dpr_val_origin = tt-dados.valor_depreciado_des_cor.
    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-dados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-dados Procedure 
PROCEDURE pi-cria-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR num_seq_incorp_bem_pat_aux AS INT NO-UNDO.
DEFINE VARIABLE num_ult AS INTEGER     NO-UNDO.
DEFINE VARIABLE seq_incorp AS INTEGER     NO-UNDO.

DEFINE VARIABLE vezes AS INTEGER     NO-UNDO.

/* verifica se acabaram as incorporaá‰es */
FIND LAST sdo_bem_pat OF bem_pat NO-LOCK
    WHERE sdo_bem_pat.dat_sdo_bem_pat        <= tt-param.dat_sdo_bem_pat       
      AND sdo_bem_pat.cod_cenar_ctbl         = tt-param.cod_cenar_ctbl_ori    
      AND sdo_bem_pat.cod_finalid_econ       = tt-param.cod_finalid_econ_ori
    NO-ERROR.
IF NOT AVAIL sdo_bem_pat THEN RETURN.
num_ult = sdo_bem_pat.num_seq_incorp_bem_pat.

seq_incorp = 0.

DO num_seq_incorp_bem_pat_aux = 0 TO num_ult:

    /* verifica se acabaram as incorporaá‰es */
    FIND LAST sdo_bem_pat OF bem_pat NO-LOCK
        WHERE sdo_bem_pat.dat_sdo_bem_pat        <= tt-param.dat_sdo_bem_pat       
          AND sdo_bem_pat.cod_cenar_ctbl         = tt-param.cod_cenar_ctbl_ori    
          AND sdo_bem_pat.cod_finalid_econ       = tt-param.cod_finalid_econ_ori
          AND sdo_bem_pat.num_seq_incorp_bem_pat = num_seq_incorp_bem_pat_aux
          /*AND sdo_bem_pat.val_origin_corrig <> 0*/
        NO-ERROR.
    IF NOT AVAIL sdo_bem_pat THEN NEXT.

    CREATE tt-dados.
    BUFFER-COPY bem_pat TO tt-dados.
    n-linhas = n-linhas + 1.

    seq_incorp = seq_incorp + 1.

    ASSIGN tt-dados.des_bem_pat = bem_pat.des_bem_pat  
           tt-dados.des_cta_pat = cta_pat.des_cta_pat.  

    FOR LAST sdo_bem_pat OF bem_pat NO-LOCK
        WHERE sdo_bem_pat.dat_sdo_bem_pat  <= tt-param.dat_sdo_bem_pat       
          AND sdo_bem_pat.cod_cenar_ctbl   = tt-param.cod_cenar_ctbl_ori    
          AND sdo_bem_pat.cod_finalid_econ = tt-param.cod_finalid_econ_ori
          AND sdo_bem_pat.num_seq_incorp_bem_pat = num_seq_incorp_bem_pat_aux
          /*AND sdo_bem_pat.val_origin_corrig <> 0*/:

        ASSIGN tt-dados.perc_ja_depre          = sdo_bem_pat.val_perc_dpr_acum
               tt-dados.valor_depreciado       = sdo_bem_pat.val_dpr_val_origin
               tt-dados.valor_corrigido        = sdo_bem_pat.val_origin_corrig
               tt-dados.fator_recalc           = tt-dados.valor_depreciado / tt-dados.valor_corrigido * 100
               tt-dados.num_seq_incorp_bem_pat = sdo_bem_pat.num_seq_incorp_bem_pat.

    END.



    vezes = 0.
    FOR EACH sdo_bem_pat OF bem_pat NO-LOCK
        WHERE sdo_bem_pat.dat_sdo_bem_pat        <= tt-param.dat_sdo_bem_pat       
          AND sdo_bem_pat.cod_cenar_ctbl         = tt-param.cod_cenar_ctbl_des    
          AND sdo_bem_pat.cod_finalid_econ       = tt-param.cod_finalid_econ_cor_des
        BREAK BY sdo_bem_pat.num_seq_incorp_bem_pat:
        IF NOT LAST-OF(sdo_bem_pat.num_seq_incorp_bem_pat) THEN NEXT.
        vezes = vezes + 1.
        IF vezes > (seq_incorp) THEN LEAVE.
        IF vezes < (seq_incorp) THEN NEXT.


        ASSIGN tt-dados.valor_original_des         = sdo_bem_pat.val_origin_corrig
               tt-dados.valor_depreciado_des       = sdo_bem_pat.val_dpr_val_origin
               tt-dados.valor_depreciado_des_cor   = round(tt-dados.valor_original_des *  tt-dados.fator_recalc / 100,2)
               tt-dados.num_seq_incorp_bem_pat_des = sdo_bem_pat.num_seq_incorp_bem_pat.

    END.

END.

/* parametros origem */
FIND FIRST param_calc_bem_pat OF bem_pat NO-LOCK
    WHERE param_calc_bem_pat.cod_tip_calc = 'DP'
      AND param_calc_bem_pat.cod_cenar_ctbl = tt-param.cod_cenar_ctbl_ori
      AND param_calc_bem_pat.cod_finalid_econ = tt-param.cod_finalid_econ_ori
    NO-ERROR.
IF NOT AVAIL param_calc_bem_pat THEN
    FIND FIRST param_calc_bem_pat OF bem_pat NO-LOCK
        WHERE param_calc_bem_pat.cod_tip_calc = 'AM'
          AND param_calc_bem_pat.cod_cenar_ctbl = tt-param.cod_cenar_ctbl_ori
          AND param_calc_bem_pat.cod_finalid_econ = tt-param.cod_finalid_econ_ori
        NO-ERROR.
IF AVAIL param_calc_bem_pat THEN
    ASSIGN tt-dados.val_perc_anual_dpr = PARAM_calc_bem_pat.val_perc_anual_dpr
           tt-dados.qtd_anos_vida_util = PARAM_calc_bem_pat.qtd_anos_vida_util.

/* parametros destino */
FIND FIRST param_calc_bem_pat OF bem_pat NO-LOCK
    WHERE param_calc_bem_pat.cod_tip_calc = 'DP'
      AND param_calc_bem_pat.cod_cenar_ctbl = tt-param.cod_cenar_ctbl_des
      AND param_calc_bem_pat.cod_finalid_econ = tt-param.cod_finalid_econ_cor_des
    NO-ERROR.
IF NOT AVAIL param_calc_bem_pat THEN
    FIND FIRST param_calc_bem_pat OF bem_pat NO-LOCK
        WHERE param_calc_bem_pat.cod_tip_calc = 'AM'
          AND param_calc_bem_pat.cod_cenar_ctbl = tt-param.cod_cenar_ctbl_des
          AND param_calc_bem_pat.cod_finalid_econ = tt-param.cod_finalid_econ_cor_des
        NO-ERROR.
IF AVAIL param_calc_bem_pat THEN
    ASSIGN tt-dados.val_perc_anual_dpr_des = PARAM_calc_bem_pat.val_perc_anual_dpr
           tt-dados.qtd_anos_vida_util_des = PARAM_calc_bem_pat.qtd_anos_vida_util.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-excel-finalizar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-finalizar Procedure 
PROCEDURE pi-excel-finalizar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF i-num-ped-exec-rpw  = 0 THEN DO:
        chexcelapplication:windowstate = -4137.
        chexcelapplication:VISIBLE                   = TRUE.
    END.
    
    chexcelapplication:Workbooks:item(1):SaveAs(tt-param.plan-destino,,,,,,).
    
    IF i-num-ped-exec-rpw  <> 0 THEN DO:
        chexcelapplication:QUIT().
    END.
    
    RELEASE OBJECT chexcelapplication:Workbooks     NO-ERROR.
    RELEASE OBJECT chexcelapplication:chworksheet   NO-ERROR.
    RELEASE OBJECT chexcelapplication               NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-excel-inicializar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-inicializar Procedure 
PROCEDURE pi-excel-inicializar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN pi-inicializar IN h-acomp ("Abrindo Excel...").

    CREATE "Excel.Application" chexcelapplication.

    chexcelapplication:APPLICATION:DisplayAlerts = FALSE.
    chexcelapplication:VISIBLE                   = FALSE.

    chWorkbook = chExcelApplication:Workbooks:OPEN(FILE-INFO:FULL-PATHNAME). 
    chworksheet = chexcelapplication:sheets:ITEM(1).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-executar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar Procedure 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FILE-INFO:FILE-NAME = 'esp/esfas010.xltx'.
IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
    
    PUT "N«O FOI POSS÷VEL ABRIR PLANILHA MODELO !!!" SKIP
        "N∆o foi poss°vel abrir a planilha modelo para geraá∆o do relat¢rio" SKIP.
        
    RETURN.

END.
ELSE DO:

    RUN pi-leitura-dados.

    IF tt-param.atualiza-recalc THEN DO:
        RUN pi-atualiza-recalc.
    END.

    IF tt-param.atualiza-param THEN DO:
        RUN pi-atualiza-param.
    END.

    IF tt-param.gera-planilha-recalc THEN do:
        RUN pi-excel-inicializar.
        RUN pi-gera-planilha.
        RUN pi-excel-finalizar.
    END.


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-gera-planilha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-planilha Procedure 
PROCEDURE pi-gera-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN pi-inicializar IN h-acomp ('Gerando relat¢rio...').
i-linha = 2.

FOR EACH tt-dados.

    RUN pi-acompanhar IN h-acomp ('Linha: ' + STRING(i-linha) + "/" + STRING(n-linhas)).

    chworksheet:Range("a"  + string(i-linha)):VALUE = tt-dados.cod_empresa.
    chworksheet:Range("b"  + string(i-linha)):VALUE = tt-dados.cod_cta_pat.
    chworksheet:Range("c"  + string(i-linha)):VALUE = tt-dados.des_cta_pat.
    chworksheet:Range("d"  + string(i-linha)):VALUE = tt-dados.num_bem_pat.
    chworksheet:Range("e"  + string(i-linha)):VALUE = tt-dados.num_seq_bem_pat.
    chworksheet:Range("f"  + string(i-linha)):VALUE = tt-dados.cod_estab.
    chworksheet:Range("g"  + string(i-linha)):VALUE = tt-dados.dat_aquis_bem_pat.
    chworksheet:Range("h"  + string(i-linha)):VALUE = tt-dados.des_bem_pat.

    chworksheet:Range("i"  + string(i-linha)):VALUE = tt-param.cod_cenar_ctbl_ori.  
    chworksheet:Range("j"  + string(i-linha)):VALUE = tt-param.cod_finalid_econ_ori.
    chworksheet:Range("k"  + string(i-linha)):VALUE = tt-dados.valor_corrigido. 
    chworksheet:Range("l"  + string(i-linha)):VALUE = tt-dados.valor_depreciado.
    chworksheet:Range("m"  + string(i-linha)):VALUE = tt-dados.fator_recalc. 

    chworksheet:Range("n"  + string(i-linha)):VALUE = tt-dados.val_perc_anual_dpr.
    chworksheet:Range("o"  + string(i-linha)):VALUE = tt-dados.qtd_anos_vida_util.   

    chworksheet:Range("p"  + string(i-linha)):VALUE = tt-param.cod_cenar_ctbl_des.
    chworksheet:Range("q"  + string(i-linha)):VALUE = tt-param.cod_finalid_econ_cor_des.
    chworksheet:Range("r"  + string(i-linha)):VALUE = tt-dados.valor_original_des.    
    chworksheet:Range("s"  + string(i-linha)):VALUE = tt-dados.valor_depreciado_des.    

    chworksheet:Range("t"  + string(i-linha)):VALUE = tt-dados.valor_depreciado_des_cor.

    chworksheet:Range("u"  + string(i-linha)):VALUE = tt-dados.val_perc_anual_dpr_des.
    chworksheet:Range("v"  + string(i-linha)):VALUE = tt-dados.qtd_anos_vida_util_des. 

    chworksheet:Range("w"  + string(i-linha)):VALUE = tt-dados.num_seq_incorp_bem_pat. 
    chworksheet:Range("x"  + string(i-linha)):VALUE = tt-dados.num_seq_incorp_bem_pat_des. 

    i-linha = i-linha + 1.

END.

chWorksheet:Columns("a:z"):EntireColumn:AutoFit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-leitura-dados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leitura-dados Procedure 
PROCEDURE pi-leitura-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN pi-inicializar IN h-acomp ('Leitura Dados...').            

/* pesquisa por estabelecimento */
IF tt-param.cod_estab_ini <> '' AND 
   tt-param.dat_aquis_bem_pat_ini <> 01/01/0001 THEN DO:

    FOR EACH bem_pat NO-LOCK
        WHERE bem_pat.cod_estab       >= tt-param.cod_estab_ini
          AND bem_pat.cod_estab       <= tt-param.cod_estab_fim
          AND bem_pat.num_bem_pat     >= tt-param.num_bem_pat_ini
          AND bem_pat.num_bem_pat     >= tt-param.num_bem_pat_fim
          AND bem_pat.num_seq_bem_pat >= tt-param.num_seq_bem_pat_ini
          AND bem_pat.num_seq_bem_pat >= tt-param.num_seq_bem_pat_fim:

        RUN pi-acompanhar IN h-acomp ("Bem: " + bem_pat.cod_empresa             + "/" +
                                                bem_pat.cod_cta_pat             + "/" +
                                                string(bem_pat.num_bem_pat    ) + "/" +
                                                string(bem_pat.num_seq_bem_pat)).

        FIND FIRST cta_pat OF bem_pat NO-LOCK NO-ERROR.

        IF bem_pat.cod_cta_pat   > tt-param.cod_cta_pat_fim OR
           bem_pat.cod_cta_pat   < tt-param.cod_cta_pat_ini OR 
           bem_pat.dat_aquis_bem_pat > tt-param.dat_aquis_bem_pat_fim OR
           bem_pat.dat_aquis_bem_pat < tt-param.dat_aquis_bem_pat_ini OR
           bem_pat.cod_empresa       > tt-param.cod_empresa_fim OR
           bem_pat.cod_empresa       < tt-param.cod_empresa_ini THEN NEXT.

          RUN pi-cria-dados.

    END.

END.

/* pesquisa por data de aquisicao */
ELSE IF tt-param.dat_aquis_bem_pat_ini <> 01/01/0001 THEN DO:

    FOR EACH ems5.empresa NO-LOCK
        WHERE empresa.cod_empresa  >= tt-param.cod_empresa_ini
          AND empresa.cod_empresa  <= tt-param.cod_empresa_fim,
        EACH bem_pat NO-LOCK
        WHERE bem_pat.cod_empresa      = empresa.cod_empresa
          AND bem_pat.cod_cta_pat     >= tt-param.cod_cta_pat_ini
          AND bem_pat.cod_cta_pat     >= tt-param.cod_cta_pat_fim
          AND bem_pat.cod_estab       >= tt-param.cod_estab_ini
          AND bem_pat.cod_estab       <= tt-param.cod_estab_fim
          AND bem_pat.dat_aquis_bem_pat >= tt-param.dat_aquis_bem_pat_ini 
          AND bem_pat.dat_aquis_bem_pat <= tt-param.dat_aquis_bem_pat_fim:

          RUN pi-acompanhar IN h-acomp ("Bem: " + bem_pat.cod_empresa             + "/" +
                                                  bem_pat.cod_cta_pat             + "/" +
                                                  string(bem_pat.num_bem_pat    ) + "/" +
                                                  string(bem_pat.num_seq_bem_pat)).

          FIND FIRST cta_pat OF bem_pat NO-LOCK NO-ERROR.

          IF bem_pat.num_bem_pat     > tt-param.num_bem_pat_fim OR
             bem_pat.num_bem_pat     < tt-param.num_bem_pat_ini OR
             bem_pat.num_seq_bem_pat > tt-param.num_seq_bem_pat_fim OR
             bem_pat.num_seq_bem_pat < tt-param.num_seq_bem_pat_ini 
              THEN NEXT.

          RUN pi-cria-dados.

    END.

END.

/* pesquisa pela chave do bem */
ELSE DO:

    FOR EACH ems5.empresa NO-LOCK
        WHERE empresa.cod_empresa  >= tt-param.cod_empresa_ini
          AND empresa.cod_empresa  <= tt-param.cod_empresa_fim,
        EACH bem_pat NO-LOCK
        WHERE bem_pat.cod_empresa      = empresa.cod_empresa
          AND bem_pat.cod_cta_pat     >= tt-param.cod_cta_pat_ini
          AND bem_pat.cod_cta_pat     <= tt-param.cod_cta_pat_fim
          AND bem_pat.num_bem_pat     >= tt-param.num_bem_pat_ini
          AND bem_pat.num_bem_pat     <= tt-param.num_bem_pat_fim
          AND bem_pat.num_seq_bem_pat >= tt-param.num_seq_bem_pat_ini
          AND bem_pat.num_seq_bem_pat <= tt-param.num_seq_bem_pat_fim:

          FIND FIRST cta_pat OF bem_pat NO-LOCK NO-ERROR.

          RUN pi-acompanhar IN h-acomp ("Bem: " + bem_pat.cod_empresa             + "/" +
                                                  bem_pat.cod_cta_pat             + "/" +
                                                  string(bem_pat.num_bem_pat    ) + "/" +
                                                  string(bem_pat.num_seq_bem_pat)).

          RUN pi-cria-dados.

    END.

END.




/*
i-linha = 2.
DO WHILE chworksheet:Range("a" + string(i-linha)):text <> ? and
         chworksheet:Range("a" + string(i-linha)):text <> ' ':

    RUN pi-acompanhar IN h-acomp ("MI00 - " + STRING(i-linha)).

    CREATE tt-dados.
    ASSIGN tt-dados.n-linha = i-linha
           tt-dados.tipo    = 'MI00'.

    overlay(tt-dados.linha-txt,1,4  )  = fc-conv-dados(chworksheet:Range("a" + string(i-linha)):TEXT,4,0,'Caracter' ).

    IF chworksheet:Range("b"  + string(i-linha)):TEXT <> '' THEN
        overlay(tt-dados.linha-txt,11,16 )  = fc-conv-dados(chworksheet:Range("b"  + string(i-linha)):TEXT,16,0,'Caracter ').
    ELSE
        overlay(tt-dados.linha-txt,11,16 )  = fc-conv-dados(string(nr-contrato-aux),16,0,'caracter').

    overlay(tt-dados.linha-txt,21,20)  = fc-conv-dados(chworksheet:Range("c" + string(i-linha)):TEXT,20,0,'Caracter').
    overlay(tt-dados.linha-txt,41,20)  = fc-conv-dados(chworksheet:Range("d" + string(i-linha)):TEXT,20,0,'Caracter').
    overlay(tt-dados.linha-txt,61,5 )  = fc-conv-dados(string(dec(chworksheet:Range("e" + string(i-linha)):VALUE)),5,2,'Decimal'  ). 
    overlay(tt-dados.linha-txt,66,4 )  = fc-conv-dados(chworksheet:Range("f" + string(i-linha)):VALUE,4,0,'Inteiro'  ). 
    overlay(tt-dados.linha-txt,70,16)  = fc-conv-dados(chworksheet:Range("g" + string(i-linha)):TEXT,16,0,'Caracter'). 
    overlay(tt-dados.linha-txt,86,3 )  = fc-conv-dados(chworksheet:Range("h" + string(i-linha)):TEXT,3,0,'Caracter' ). 
    overlay(tt-dados.linha-txt,89,5 )  = fc-conv-dados(chworksheet:Range("i" + string(i-linha)):VALUE,5,0,'Inteiro'  ). 
                                                                             
    i-linha = i-linha + 1.                                                   

END.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/****************************************************************************
** Programa..............: fnc_exporta_lote_ctbl 
** Descricao.............: Envio de Lotes Cont beis Unificados
** Procedimento..........: fnc_exporta_lote_ctbl
** Nome Externo..........: esp/esfgl072rp.p
** Criado por............: Hilton Borba
** Alterado por .........: Rog‚rio Dias
** Criado em.............: 23/01/2012
*****************************************************************************/
/* -----------------[   Defini‡Æo de Temp-tables e Vari veis Globais    ]------------------- */
{esp\esfgl072tmp.i}

/* -----------------[   Defini‡Æo de Parƒmetros    ]------------------- */
/*DEFINE INPUT PARAMETER TABLE for tt_param.*/

/* -----------------[   Defini‡Æo de Vari veis    ]------------------- */
DEFINE VARIABLE v_cod_user         AS CHARACTER format "x(21)":U LABEL "Usu rio" column-label "Usu rio" NO-UNDO.
DEFINE VARIABLE c-resultado        AS CHARACTER FORMAT "x(10)"   LABEL "Narrativa"                      NO-UNDO.
DEFINE VARIABLE h-acomp            AS HANDLE                                                            NO-UNDO.
DEFINE VARIABLE v_num_mensagem     AS INTEGER                                                           NO-UNDO.
DEFINE VARIABLE v_cod_msg          AS CHARACTER                                                         NO-UNDO.
DEFINE VARIABLE v_des_ajuda_view   AS CHARACTER                                                         NO-UNDO.
DEFINE VARIABLE v_des_message_view AS CHARACTER                                                         NO-UNDO.
DEFINE VARIABLE v_ind_tip_msg      AS CHARACTER                                                         NO-UNDO.
define variable c-arquivo          AS CHARACTER FORMAT "x(30)"                                          NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF criter_distrib_cta_ctbl.

/* -----------------[   Defini‡Æo de Buffers    ]------------------- */
DEFINE BUFFER bf_es_cons_estab FOR es_cons_estab.
DEFINE BUFFER bf_criter_distrib_cta_ctbl  FOR criter_distrib_cta_ctbl.

/* -----------------[   Defini‡Æo de Streams    ]------------------- */
DEFINE STREAM str-rp.


/* -----------------------[   Main Block   ]------------------------ */

SESSION:SET-WAIT-STATE("General").

RUN pi_inicio.
FIND FIRST tt_param NO-LOCK NO-ERROR.

OUTPUT STREAM str-rp to value(c-arquivo) no-convert.

FOR EACH es_cons_estab_emp NO-LOCK 
    WHERE es_cons_estab_emp.cod_empresa     = tt_param.c_emp_dest_ini
      AND es_cons_estab_emp.cod_cenar_ctbl >= tt_param.c_cenario_ini 
      AND es_cons_estab_emp.cod_cenar_ctbl <= tt_param.c_cenario_fim 
      AND es_cons_estab_emp.dat_inic_valid <= TODAY
      AND es_cons_estab_emp.dat_fim_valid  >= TODAY:   

    PROCESS EVENTS.
    RUN pi_cria_lote_ctbl.    
END.

OUTPUT STREAM str-rp close.

RUN pi_show_report_2 (INPUT c-arquivo).

RUN pi-finalizar IN h-acomp.

SESSION:SET-WAIT-STATE("").

RETURN "OK".
/* -------------------------------------------------------------------- */



/* ----------------------------------------------------------------[   Procedures Internars   ]------------------------------------------------------- */
PROCEDURE pi_inicio:

    RUN utp/ut-acomp.p persistent set h-acomp.
    RUN pi-inicializar in h-acomp (INPUT "Localizando Lotes...").

    IF NUM-ENTRIES(v_cod_dwb_user,'_') > 1 THEN DO:
        ASSIGN v_cod_user = ENTRY(2,v_cod_dwb_user,'_').
    END.
    ELSE v_cod_user = v_cod_dwb_user.

    IF v_cod_dwb_program = '' THEN ASSIGN v_cod_dwb_program = 'esfgl072rp'.

    FIND dwb_rpt_param NO-LOCK WHERE dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
                                      AND dwb_rpt_param.cod_dwb_user    = v_cod_user      no-error.

    /* ------- Cria tt-param com tabela padrÆo do EMS. Necess rio para utiliza‡Æo por RPW ----------- */
    CREATE tt_param.
    ASSIGN tt_param.c_emp_orig_ini  =         ENTRY( 1,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.c_emp_orig_fim  =         ENTRY( 2,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.c_emp_dest_ini  =         ENTRY( 3,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.c_emp_dest_fim  =         ENTRY( 4,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.i_lote_ini      = INTEGER(ENTRY( 5,dwb_rpt_param.cod_dwb_parameters,'|'))
           tt_param.i_lote_fim      = INTEGER(ENTRY( 6,dwb_rpt_param.cod_dwb_parameters,'|'))
           tt_param.dt_lote_ini     = DATE   (ENTRY( 7,dwb_rpt_param.cod_dwb_parameters,'|'))
           tt_param.dt_lote_fim     = DATE   (ENTRY( 8,dwb_rpt_param.cod_dwb_parameters,'|'))
           tt_param.dt_lanca_ini    = DATE   (ENTRY( 9,dwb_rpt_param.cod_dwb_parameters,'|'))
           tt_param.dt_lanca_fim    = DATE   (ENTRY(10,dwb_rpt_param.cod_dwb_parameters,'|'))
           tt_param.c_cenario_ini   =         ENTRY(11,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.c_cenario_fim   =         ENTRY(12,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.c_modulo_ini    =         ENTRY(13,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.c_modulo_fim    =         ENTRY(14,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.class-1         =         ENTRY(15,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.class-2         =         ENTRY(16,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.class-3         =         ENTRY(17,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.rs_output       =         ENTRY(18,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.c_arquivo       =         ENTRY(19,dwb_rpt_param.cod_dwb_parameters,'|')
           tt_param.rs_tipo_log     = 'Detalhado'. 

EMPTY TEMP-TABLE tt_es_cons_lotes.
EMPTY TEMP-TABLE tt_msg_erro_es_lote.

if tt_param.rs_OUTPUT = "Terminal" THEN ASSIGN c-arquivo =  session:temp-directory + "log_lote.txt".
if  tt_param.rs_OUTPUT = "Arquivo" THEN ASSIGN c-arquivo = tt_param.c_arquivo.

END PROCEDURE.


PROCEDURE pi_cria_lote_ctbl :

    DEF VAR c_contas_erro AS CHARACTER NO-UNDO.
    DEF VAR c_ccusto_erro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE p_erro AS LOGICAL NO-UNDO.

    DEF BUFFER bf-lancto_ctbl FOR lancto_ctbl.

    FOR EACH modul_dtsul NO-LOCK WHERE cod_modul_dtsul >= tt_param.c_modulo_ini
                                          AND cod_modul_dtsul <= tt_param.c_modulo_fim:

        FOR EACH lote_ctbl NO-LOCK WHERE lote_ctbl.cod_empresa      >= tt_param.c_emp_orig_ini
                                     AND lote_ctbl.cod_empresa      <= tt_param.c_emp_orig_fim
                                     AND lote_ctbl.cod_modul_dtsul   = modul_dtsul.cod_modul_dtsul
                                     AND lote_ctbl.num_lote_ctbl    >= tt_param.i_lote_ini
                                     AND lote_ctbl.num_lote_ctbl    <= tt_param.i_lote_fim
                                     AND lote_ctbl.ind_sit_lote_ctb  = 'Ctbz'
                                     AND (CAN-FIND(FIRST lancto_ctbl OF lote_ctbl WHERE lancto_ctbl.dat_lancto_ctbl >= tt_param.dt_lanca_ini
                                                                                    AND lancto_ctbl.dat_lancto_ctbl <= tt_param.dt_lanca_fim )):

            EMPTY TEMP-TABLE tt_integr_lote_ctbl_1          .
            EMPTY TEMP-TABLE tt_integr_lancto_ctbl_1        .
            EMPTY TEMP-TABLE tt_integr_item_lanc_ctbl_1     .
            EMPTY TEMP-TABLE tt_integr_aprop_lanc_ctbl_1    .
            EMPTY TEMP-TABLE tt_integr_ctbl_valid_1         .
            EMPTY TEMP-TABLE tt_integr_lote_ctbl_1_aux      .
            EMPTY TEMP-TABLE tt_integr_lanc_ctbl_1_aux      .
            EMPTY TEMP-TABLE tt_integr_item_lanc_ctbl_1_aux .
            EMPTY TEMP-TABLE tt_integr_aprop_lancto_ctbl_aux.
            EMPTY TEMP-TABLE tt_integr_ctbl_valid_1_aux     .
            EMPTY TEMP-TABLE tt_lote_ctbl_orig              .

            FIND FIRST ems5.empresa WHERE empresa.cod_empresa = lote_ctbl.cod_empresa NO-LOCK NO-ERROR.
            IF NOT(CAN-FIND(FIRST estabelecimento WHERE estabelecimento.cod_empresa = empresa.cod_empresa
                                                    AND (CAN-FIND(FIRST es_cons_estab OF es_cons_estab_emp WHERE es_cons_estab.cod_estab = estabelecimento.cod_estab)))) THEN NEXT.

            IF NOT(CAN-FIND(FIRST lancto_ctbl OF lote_ctbl NO-LOCK WHERE lancto_ctbl.cod_cenar_ctbl  = es_cons_estab_emp.cod_cenar_ctbl )) THEN NEXT.

            PROCESS EVENTS.
            RUN pi-acompanhar in h-acomp ("Lote: " + STRING(lote_ctbl.num_lote_ctbl)).

            FOR EACH lancto_ctbl OF lote_ctbl NO-LOCK WHERE lancto_ctbl.cod_cenar_ctbl   = es_cons_estab_emp.cod_cenar_Ctbl
                                                        AND lancto_ctbl.cod_modul_dtsul  = lote_ctbl.cod_modul_dtsul
                                                        AND lancto_ctbl.dat_lancto_ctbl >= tt_param.dt_lanca_ini
                                                        AND lancto_ctbl.dat_lancto_ctbl <= tt_param.dt_lanca_fim:

                ASSIGN c_contas_erro = ''.
                FOR EACH ITEM_lancto_ctbl OF lancto_ctbl NO-LOCK:
                    IF (NOT CAN-FIND(FIRST es_cons_cta_ctbl WHERE es_cons_cta_ctbl.cod_cta_ctbl       = ITEM_lancto_ctbl.cod_cta_ctbl
                                                              AND es_cons_cta_ctbl.cod_plano_cta_ctbl = ITEM_lancto_ctbl.cod_plano_cta_ctbl )) THEN DO:
                        IF INDEX(c_contas_erro,ITEM_lancto_ctbl.cod_cta_ctbl) = 0 THEN
                            ASSIGN c_contas_erro = c_Contas_erro + ' ' + ITEM_lancto_ctbl.cod_cta_ctbl.
                    END.
                    IF (NOT CAN-FIND(FIRST es_cons_ccusto WHERE es_cons_ccusto.cod_ccusto       = ITEM_lancto_ctbl.cod_ccusto
                                                            AND es_cons_ccusto.cod_plano_ccusto = ITEM_lancto_ctbl.cod_plano_ccusto )) THEN DO:
                        IF INDEX(c_ccusto_erro,ITEM_lancto_ctbl.cod_ccusto) = 0 THEN
                            ASSIGN c_ccusto_erro  = c_ccusto_erro  + ' ' + ITEM_lancto_ctbl.cod_ccusto.
                    END.
                END.

                IF c_contas_erro <> '' THEN DO:
                    
                    CREATE tt_es_cons_lotes NO-ERROR.
                    ASSIGN tt_es_cons_lotes.cod_empresa_orig = lote_ctbl.cod_empresa
                           tt_es_cons_lotes.num_lote_orig    = lote_ctbl.num_lote_ctbl
                           tt_es_cons_lotes.des_lote_orig    = lote_ctbl.des_lote_ctbl     
                           tt_es_cons_lotes.dt_lote_orig     = lote_ctbl.dat_lote_ctbl
                           tt_es_cons_lotes.num_lancto_orig  = lancto_ctbl.num_lancto_ctbl
                           tt_es_cons_lotes.dt_lancto_orig   = lancto_ctbl.dat_lancto_ctbl
                           tt_es_cons_lotes.cod_mod_orig     = lote_ctbl.cod_modul_dts
                           tt_es_cons_lotes.cod_cenario_orig = lancto_ctbl.cod_cenar_ctbl
                           tt_es_cons_lotes.cod_empresa_dest = es_cons_estab_emp.cod_empresa
                           tt_es_cons_lotes.num_lote_dest    = 0
                           tt_es_cons_lotes.des_lote_dest    = ''
                           tt_es_cons_lotes.dt_lote_dest     = ?
                           tt_es_cons_lotes.dt_lancto_dest   = ?
                           tt_es_cons_lotes.resultado        = "As contas cont beis : " + c_contas_erro + " nÆo estÆo cadastradas no De-para de Conta Cont bil"
                           tt_es_cons_lotes.cria_reg         = YES
                           tt_es_cons_lotes.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).
                        
                    CREATE tt_msg_erro_es_lote.
                    ASSIGN tt_msg_erro_es_lote.des_msg_erro      = tt_es_cons_lotes.resultado
                           tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).
                    RUN pi-cria-tabela-es-lotes (INPUT ROWID(tt_es_cons_lotes)).                           
                    NEXT.                  
                END.

                IF c_ccusto_erro <> '' THEN DO:                    
                    CREATE tt_es_cons_lotes NO-ERROR.
                    ASSIGN tt_es_cons_lotes.cod_empresa_orig = lote_ctbl.cod_empresa
                           tt_es_cons_lotes.num_lote_orig    = lote_ctbl.num_lote_ctbl
                           tt_es_cons_lotes.des_lote_orig    = lote_ctbl.des_lote_ctbl     
                           tt_es_cons_lotes.dt_lote_orig     = lote_ctbl.dat_lote_ctbl
                           tt_es_cons_lotes.num_lancto_orig  = lancto_ctbl.num_lancto_ctbl
                           tt_es_cons_lotes.dt_lancto_orig   = lancto_ctbl.dat_lancto_ctbl
                           tt_es_cons_lotes.cod_mod_orig     = lote_ctbl.cod_modul_dts
                           tt_es_cons_lotes.cod_cenario_orig = lancto_ctbl.cod_cenar_ctbl
                           tt_es_cons_lotes.cod_empresa_dest = es_cons_estab_emp.cod_empresa
                           tt_es_cons_lotes.num_lote_dest    = 0
                           tt_es_cons_lotes.des_lote_dest    = ''
                           tt_es_cons_lotes.dt_lote_dest     = ?
                           tt_es_cons_lotes.dt_lancto_dest   = ?
                           tt_es_cons_lotes.resultado        = "Os Centros de Custos : " + c_ccusto_erro + " nÆo estÆo cadastrados no De-para de Centro de Custos"
                           tt_es_cons_lotes.cria_reg         = YES
                           tt_es_cons_lotes.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).
                        
                    CREATE tt_msg_erro_es_lote.
                    ASSIGN tt_msg_erro_es_lote.des_msg_erro      = tt_es_cons_lotes.resultado
                           tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).
                    RUN pi-cria-tabela-es-lotes (INPUT ROWID(tt_es_cons_lotes)).                           
                    NEXT.                  
                END.

                FIND FIRST es_cons_lotes WHERE es_cons_lotes.num_lote_orig     = lote_ctbl.num_lote_ctbl
                                           AND es_cons_lotes.cod_empresa_orig  = lote_ctbl.cod_empresa 
                                           AND es_cons_lotes.num_lancto_orig   = lancto_ctbl.num_lancto_ctbl NO-LOCK use-INDEX origem  NO-ERROR.
                IF AVAIL es_cons_lotes THEN DO:

                    /* ---------- Se encontrar um registro na tabela especifica com o lote origem e encontrar o lote destino, nÆo refaz o lote -------*/
                    FIND FIRST bf-lote_ctbl NO-LOCK WHERE bf-lote_ctbl.num_lote_ctbl = es_cons_lotes.num_lote_dest
                                                      AND (CAN-FIND(FIRST bf-lancto_ctbl OF bf-lote_ctbl WHERE bf-lancto_ctbl.num_lancto_ctbl = lancto_ctbl.num_lancto_ctbl)) NO-ERROR.
                    IF AVAIL bf-lote_ctbl THEN DO:
                        CREATE tt_es_cons_lotes NO-ERROR.
                        ASSIGN tt_es_cons_lotes.cod_empresa_orig = lote_ctbl.cod_empresa
                               tt_es_cons_lotes.num_lote_orig    = lote_ctbl.num_lote_ctbl
                               tt_es_cons_lotes.des_lote_orig    = lote_ctbl.des_lote_ctbl     
                               tt_es_cons_lotes.dt_lote_orig     = lote_ctbl.dat_lote_ctbl
                               tt_es_cons_lotes.num_lancto_orig  = lancto_ctbl.num_lancto_ctbl
                               tt_es_cons_lotes.dt_lancto_orig   = lancto_ctbl.dat_lancto_ctbl
                               tt_es_cons_lotes.cod_mod_orig     = lote_ctbl.cod_modul_dts
                               tt_es_cons_lotes.cod_cenario_orig = lancto_ctbl.cod_cenar_ctbl
                               tt_es_cons_lotes.cod_empresa_dest = es_cons_estab_emp.cod_empresa
                               tt_es_cons_lotes.num_lote_dest    = 0
                               tt_es_cons_lotes.des_lote_dest    = ''
                               tt_es_cons_lotes.dt_lote_dest     = ?
                               tt_es_cons_lotes.dt_lancto_dest   = ?
                               tt_es_cons_lotes.resultado        = "Lote " + string(bf-lote_ctbl.num_lote_ctbl) + " para a Empresa " + es_cons_lotes.cod_empresa_dest + " j  importado em " + string(es_cons_lotes.dt_lote_dest,"99/99/9999")
                               tt_es_cons_lotes.cria_reg         = NO
                               tt_es_cons_lotes.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).
                            
                        CREATE tt_msg_erro_es_lote.
                        ASSIGN tt_msg_erro_es_lote.des_msg_erro      = tt_es_cons_lotes.resultado
                               tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).                                                
                        NEXT.
                    END.
                    ELSE DO:
                        /* ---------- Caso nÆo encontro o lote destino deleta o registro na tabela espec¡fica -------*/
                        FIND CURRENT es_cons_lotes EXCLUSIVE-LOCK NO-ERROR.
                        DELETE es_cons_lotes.                        
                    END.
                END.
               
                IF NOT CAN-FIND(FIRST tt_integr_lote_ctbl_1_aux WHERE tt_integr_lote_ctbl_1_aux.tta_num_lote_ctbl = lote_ctbl.num_lote_ctbl) THEN DO:
                    
                    /* ---------- Cria Lote CTBL --------- */
                    CREATE tt_integr_lote_ctbl_1_aux NO-ERROR.
                    ASSIGN tt_integr_lote_ctbl_1_aux.tta_cod_modul_dtsul        = lote_ctbl.cod_modul_dtsul
                           tt_integr_lote_ctbl_1_aux.tta_num_lote_ctbl          = lote_ctbl.num_lote_ctbl
                           tt_integr_lote_ctbl_1_aux.tta_des_lote_ctbl          = lote_ctbl.des_lote_ctbl
                           tt_integr_lote_ctbl_1_aux.tta_cod_empresa            = es_cons_estab_emp.cod_empresa
                           tt_integr_lote_ctbl_1_aux.tta_dat_lote_ctbl          = TODAY
                           tt_integr_lote_ctbl_1_aux.tta_log_integr_ctbl_online = lote_ctbl.log_integr_ctbl_online
                           tt_integr_lote_ctbl_1_aux.ttv_rec_integr_lote_ctbl   = RECID(tt_integr_lote_ctbl_1_aux).                  
                END.

                /* ---------- Cria tabela tempor ria para amarrar o lote criado ao lote origem ---------- */
                IF NOT CAN-FIND(FIRST tt_lote_ctbl_orig WHERE tt_lote_ctbl_orig.num_lote_orig   = lote_ctbl.num_lote_ctbl
                                                          AND tt_lote_ctbl_orig.num_lancto_orig = lancto_ctbl.num_lancto_ctbl) THEN DO:
                    CREATE tt_lote_ctbl_orig NO-ERROR.
                    ASSIGN tt_lote_ctbl_orig.cod_empresa       = lote_ctbl.cod_empresa
                           tt_lote_ctbl_orig.num_lote_orig     = lote_ctbl.num_lote_ctbl
                           tt_lote_ctbl_orig.num_lancto_orig   = lancto_ctbl.num_lancto_ctbl
                           tt_lote_ctbl_orig.des_lote_orig     = lote_ctbl.des_lote_ctbl
                           tt_lote_ctbl_orig.dt_lote_orig      = lote_ctbl.dat_lote_ctbl
                           tt_lote_ctbl_orig.dt_lancto_orig    = lancto_ctbl.dat_lancto_ctbl
                           tt_lote_ctbl_orig.cod_mod_orig      = lancto_ctbl.cod_modul_dts
                           tt_lote_ctbl_orig.cod_cenario_orig  = lancto_ctbl.cod_cenar_ctbl
                           tt_lote_ctbl_orig.rec_lote          = RECID(tt_integr_lote_ctbl_1_aux).                    
                END.

                /* ---------- Cria Lancto CTBL --------- */
                RUN pi_cria_lancto_ctbl(INPUT lote_ctbl.num_lote_ctbl,
                                        INPUT lote_ctbl.cod_modul_dtsul,
                                        INPUT tt_integr_lote_ctbl_1_aux.ttv_rec_integr_lote_ctbl).                               
            END.            

            RUN pi-chama-api.
            RUN pi-imprime-log.
        END.
    END.    

END PROCEDURE.

PROCEDURE pi_cria_lancto_ctbl :

    DEFINE INPUT PARAMETER p_num_lote_orig        AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER p_cod_modul_dtsul      AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER p_rec_integr_lote_ctbl AS RECID      NO-UNDO.
    
    CREATE tt_integr_lanc_ctbl_1_aux NO-ERROR.
    ASSIGN tt_integr_lanc_ctbl_1_aux.tta_cod_cenar_ctbl           = lancto_ctbl.cod_cenar_ctbl
           tt_integr_lanc_ctbl_1_aux.tta_log_lancto_conver        = no
           tt_integr_lanc_ctbl_1_aux.tta_log_lancto_apurac_restdo = no
           tt_integr_lanc_ctbl_1_aux.tta_cod_rat_ctbl             = lancto_ctbl.cod_rat_ctbl
           tt_integr_lanc_ctbl_1_aux.ttv_rec_integr_lote_ctbl     = p_rec_integr_lote_ctbl
           tt_integr_lanc_ctbl_1_aux.tta_num_lancto_ctbl          = lancto_ctbl.num_lancto_ctbl
           tt_integr_lanc_ctbl_1_aux.tta_dat_lancto_ctbl          = lancto_ctbl.dat_lancto_ctbl
           tt_integr_lanc_ctbl_1_aux.ttv_rec_integr_lancto_ctbl   = RECID(tt_integr_lanc_ctbl_1_aux).

    /* ---------- Cria Item Lancto CTBL --------- */
    RUN pi_cria_item_lancto_ctbl(INPUT p_num_lote_orig,
                                 INPUT lancto_ctbl.num_lancto_ctbl,
                                 INPUT tt_integr_lanc_ctbl_1_aux.ttv_rec_integr_lancto_ctbl).        
    
END PROCEDURE.

PROCEDURE pi_cria_item_lancto_ctbl :

    DEFINE INPUT  PARAMETER p_num_lote_orig          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER p_num_lancto_orig        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER p_rec_integr_lancto_ctbl AS RECID     NO-UNDO.
    
    FOR EACH item_lancto_ctbl OF lancto_ctbl NO-LOCK:

        FIND FIRST es_cons_estab NO-LOCK WHERE es_cons_estab.cod_empresa = es_cons_estab_emp.cod_empresa
                                           AND es_cons_estab.cod_estab   = item_lancto_ctbl.cod_estab NO-ERROR.
        
        FIND FIRST es_cons_cta_ctbl NO-LOCK WHERE es_cons_cta_ctbl.cod_empresa  = es_cons_estab_emp.cod_empresa
                                              AND es_cons_cta_ctbl.cod_cta_ctbl = item_lancto_ctbl.cod_cta_ctbl NO-ERROR.
        
        FIND FIRST es_cons_ccusto NO-LOCK WHERE es_cons_ccusto.cod_empresa = es_cons_estab_emp.cod_empresa
                                            AND es_cons_ccusto.cod_ccusto  = item_lancto_ctbl.cod_ccusto NO-ERROR.
        
        IF NOT CAN-FIND(FIRST tt_integr_item_lanc_ctbl_1_aux WHERE tt_integr_item_lanc_ctbl_1_aux.ttv_rec_integr_lancto_ctbl = p_rec_integr_lancto_ctbl
                                                               AND tt_integr_item_lanc_ctbl_1_aux.tta_num_seq_lancto_ctbl    = item_lancto_ctbl.num_seq_lancto_ctbl) THEN DO:
            CREATE tt_integr_item_lanc_ctbl_1_aux NO-ERROR.
            ASSIGN tt_integr_item_lanc_ctbl_1_aux.ttv_rec_integr_lancto_ctbl      = p_rec_integr_lancto_ctbl
                   tt_integr_item_lanc_ctbl_1_aux.ttv_rec_integr_item_lancto_ctbl = RECID(tt_integr_item_lanc_ctbl_1_aux)
                   tt_integr_item_lanc_ctbl_1_aux.tta_num_seq_lancto_ctbl         = item_lancto_ctbl.num_seq_lancto_ctbl
                   tt_integr_item_lanc_ctbl_1_aux.tta_ind_natur_lancto_ctbl       = item_lancto_ctbl.ind_natur_lancto_ctbl
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_plano_cta_ctbl          = trim(es_cons_estab_emp.cod_plano_cta_ctbl)
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_cta_ctbl                = IF AVAIL es_cons_cta_ctbl THEN es_cons_cta_ctbl.cod_cta_ctbl_para ELSE TRIM(ITEM_lancto_ctbl.cod_cta_ctbl)
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_plano_ccusto            = IF AVAIL es_cons_ccusto THEN trim(es_cons_ccusto.cod_plano_ccusto_para) ELSE TRIM(ITEM_lancto_ctbl.cod_plano_ccusto)
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_ccusto                  = IF AVAIL es_cons_ccusto THEN trim(es_cons_ccusto.cod_ccusto_para) ELSE TRIM(ITEM_lancto_ctbl.cod_ccusto)
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_estab                   = IF AVAIL es_cons_estab THEN es_cons_estab.cod_estab_para ELSE ITEM_lancto_ctbl.cod_estab
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_unid_negoc              = item_lancto_ctbl.cod_unid_negoc
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_histor_padr             = ""
                   tt_integr_item_lanc_ctbl_1_aux.tta_des_histor_lancto_ctbl      = item_lancto_ctbl.des_histor_lancto_ctbl
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_espec_docto             = item_lancto_ctbl.cod_espec_docto          
                   tt_integr_item_lanc_ctbl_1_aux.tta_dat_docto                   = item_lancto_ctbl.dat_docto                
                   tt_integr_item_lanc_ctbl_1_aux.tta_des_docto                   = item_lancto_ctbl.des_docto                
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_imagem                  = item_lancto_ctbl.cod_imagem               
                   tt_integr_item_lanc_ctbl_1_aux.tta_cod_indic_econ              = item_lancto_ctbl.cod_indic_econ           
                   tt_integr_item_lanc_ctbl_1_aux.tta_dat_lancto_ctbl             = lancto_ctbl.dat_lancto_ctbl          
                   tt_integr_item_lanc_ctbl_1_aux.tta_qtd_unid_lancto_ctbl        = item_lancto_ctbl.qtd_unid_lancto_ctbl     
                   tt_integr_item_lanc_ctbl_1_aux.tta_val_lancto_ctbl             = item_lancto_ctbl.val_lancto_ctbl          
                   tt_integr_item_lanc_ctbl_1_aux.tta_num_seq_lancto_ctbl_cpart   = item_lancto_ctbl.num_seq_lancto_ctbl_cpart.            

            /* Altera‡Æo Realizada pois a base possue divergencias entre o item do lote e o criterio cadastrado */
            FIND FIRST criter_distrib_cta_ctbl NO-LOCK WHERE criter_distrib_cta_ctbl.cod_empresa        = es_cons_estab_emp.cod_empresa
                                                         AND criter_distrib_cta_ctbl.cod_estab          = tt_integr_item_lanc_ctbl_1_aux.tta_cod_estab
                                                         AND criter_distrib_cta_ctbl.dat_fim_valid     >= lancto_ctbl.dat_lancto_ctbl
                                                         AND criter_distrib_cta_ctbl.cod_plano_cta_ctbl = tt_integr_item_lanc_ctbl_1_aux.tta_cod_plano_cta_ctbl
                                                         AND criter_distrib_cta_ctbl.cod_cta_ctbl       = tt_integr_item_lanc_ctbl_1_aux.tta_cod_cta_ctbl NO-ERROR.
            IF AVAIL criter_distrib_cta_ctbl THEN DO:
                IF tt_integr_item_lanc_ctbl_1_aux.tta_cod_ccusto = '' THEN DO:
                   IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto <> 'NÆo Utiliza' THEN DO:
                       FIND CURRENT criter_distrib_cta_ctbl EXCLUSIVE-LOCK NO-ERROR.
                       ASSIGN criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = 'NÆo Utiliza'.
                   END.
                END.
                ELSE DO:
                   IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = 'NÆo Utiliza' THEN DO:
                       FIND CURRENT criter_distrib_cta_ctbl EXCLUSIVE-LOCK NO-ERROR.
                       ASSIGN criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = 'Utiliza Todos'.
                   END.
                END.                
            END.
            ELSE DO:
                find last bf_criter_distrib_cta_ctbl NO-LOCK where bf_criter_distrib_cta_ctbl.cod_plano_cta_ctbl = tt_integr_item_lanc_ctbl_1_aux.tta_cod_plano_cta_ctbl
                                                               and bf_criter_distrib_cta_ctbl.cod_cta_ctbl       = tt_integr_item_lanc_ctbl_1_aux.tta_cod_cta_ctbl NO-ERROR.
                
                CREATE criter_distrib_cta_ctbl.                             
                ASSIGN criter_distrib_cta_ctbl.cod_cta_ctbl                = tt_integr_item_lanc_ctbl_1_aux.tta_cod_cta_ctbl
                       criter_distrib_cta_ctbl.cod_empresa                 = es_cons_estab_emp.cod_empresa
                       criter_distrib_cta_ctbl.cod_estab                   = tt_integr_item_lanc_ctbl_1_aux.tta_cod_estab
                       criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto     = ''
                       criter_distrib_cta_ctbl.cod_plano_cta_ctbl          = tt_integr_item_lanc_ctbl_1_aux.tta_cod_plano_cta_ctbl
                       criter_distrib_cta_ctbl.dat_fim_valid               = 12/31/9999
                       criter_distrib_cta_ctbl.dat_inic_valid              = 01/01/1900
                       criter_distrib_cta_ctbl.ind_criter_distrib_ccusto   = (IF tt_integr_item_lanc_ctbl_1_aux.tta_cod_ccusto = '' THEN 'NÆo Utiliza' ELSE "Utiliza Todos")
                       criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl = (IF AVAIL bf_criter_distrib_cta_ctbl THEN bf_criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl + 10 ELSE 10 ).
            END.

            IF tt_integr_item_lanc_ctbl_1_aux.tta_cod_ccusto <> '' THEN DO:

                FIND FIRST ems5.ccusto WHERE ccusto.cod_ccusto       = tt_integr_item_lanc_ctbl_1_aux.tta_cod_ccusto
                                         AND ccusto.cod_plano_ccusto = tt_integr_item_lanc_ctbl_1_aux.tta_cod_plano_ccusto
                                         AND ccusto.cod_empresa      = es_cons_estab_emp.cod_empresa NO-LOCK NO-ERROR.
                IF AVAIL ccusto THEN DO:
                    IF NOT (CAN-FIND(FIRST ccusto_unid_negoc OF ccusto)) THEN DO:
                        CREATE ccusto_unid_negoc.
                        ASSIGN ccusto_unid_negoc.cod_ccusto       = ccusto.cod_ccusto      
                               ccusto_unid_negoc.cod_empresa      = ccusto.cod_empresa     
                               ccusto_unid_negoc.cod_plano_ccusto = ccusto.cod_plano_ccusto
                               ccusto_unid_negoc.cod_unid_negoc   = "00".
                    END.
                    IF NOT(CAN-FIND(FIRST segur_ccusto OF ccusto WHERE segur_ccusto.cod_grp_usuar = "*" )) THEN DO:
                        CREATE segur_ccusto.
                        ASSIGN segur_ccusto.cod_ccusto        = ccusto.cod_ccusto       
                               segur_ccusto.cod_empresa       = ccusto.cod_empresa      
                               segur_ccusto.cod_grp_usuar     = "*"   
                               segur_ccusto.cod_plano_ccusto  = ccusto.cod_plano_ccusto .
                    END.
                END.

            END.
            
            RUN pi_cria_aprop_lancto_ctbl(INPUT p_num_lote_orig,
                                          INPUT p_num_lancto_orig,
                                          INPUT tt_integr_item_lanc_ctbl_1_aux.tta_num_seq_lancto_ctbl,
                                          INPUT tt_integr_item_lanc_ctbl_1_aux.ttv_rec_integr_item_lancto_ctbl,
                                          INPUT trim(tt_integr_item_lanc_ctbl_1_aux.tta_cod_plano_ccusto),
                                          INPUT trim(tt_integr_item_lanc_ctbl_1_aux.tta_cod_ccusto),
                                          INPUT tt_integr_item_lanc_ctbl_1_aux.tta_cod_unid_negoc).
        END.
    END.

    
end PROCEDURE.

PROCEDURE pi_cria_aprop_lancto_ctbl :

    DEFINE INPUT PARAMETER p_num_lote_orig               AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p_num_lancto_orig             AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p_num_seq_lancto_orig         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p_rec_integr_item_lancto_ctbl AS RECID     NO-UNDO.
    DEFINE INPUT PARAMETER p_cod_plano_ccusto            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER p_cod_ccusto                  AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER p_cod_unid_negoc              AS CHARACTER  NO-UNDO.

    FOR EACH aprop_lancto_ctbl NO-LOCK WHERE aprop_lancto_ctbl.num_lote_ctbl       = p_num_lote_orig
                                         AND aprop_lancto_ctbl.num_lancto_ctbl     = p_num_lancto_orig
                                         AND aprop_lancto_ctbl.num_seq_lancto_ctbl = p_num_seq_lancto_orig:

        IF NOT CAN-FIND(FIRST tt_integr_aprop_lancto_ctbl_aux WHERE tt_integr_aprop_lancto_ctbl_aux.ttv_rec_integr_item_lancto_ctbl = p_rec_integr_item_lancto_ctbl
                                                                AND tt_integr_aprop_lancto_ctbl_aux.tta_cod_finalid_econ            = aprop_lancto_ctbl.cod_finalid_econ
                                                                AND tt_integr_aprop_lancto_ctbl_aux.tta_cod_unid_negoc              = p_cod_unid_negoc
                                                                AND tt_integr_aprop_lancto_ctbl_aux.tta_cod_plano_ccusto            = p_cod_plano_ccusto
                                                                AND tt_integr_aprop_lancto_ctbl_aux.tta_cod_ccusto                  = p_cod_ccusto) THEN DO:
            CREATE tt_integr_aprop_lancto_ctbl_aux NO-ERROR.
            ASSIGN tt_integr_aprop_lancto_ctbl_aux.ttv_rec_integr_item_lancto_ctbl  = p_rec_integr_item_lancto_ctbl
                   tt_integr_aprop_lancto_ctbl_aux.ttv_rec_integr_aprop_lancto_ctbl = RECID(tt_integr_aprop_lancto_ctbl_aux)
                   tt_integr_aprop_lancto_ctbl_aux.tta_cod_finalid_econ             = aprop_lancto_ctbl.cod_finalid_econ
                   tt_integr_aprop_lancto_ctbl_aux.tta_cod_unid_negoc               = p_cod_unid_negoc
                   tt_integr_aprop_lancto_ctbl_aux.tta_cod_plano_ccusto             = p_cod_plano_ccusto
                   tt_integr_aprop_lancto_ctbl_aux.tta_cod_ccusto                   = p_cod_ccusto
                   tt_integr_aprop_lancto_ctbl_aux.tta_qtd_unid_lancto_ctbl         = aprop_lancto_ctbl.qtd_unid_lancto_ctbl    
                   tt_integr_aprop_lancto_ctbl_aux.tta_val_lancto_ctbl              = aprop_lancto_ctbl.val_lancto_ctbl         
                   tt_integr_aprop_lancto_ctbl_aux.tta_num_id_aprop_lancto_ctbl     = aprop_lancto_ctbl.num_id_aprop_lancto_ctbl
                   tt_integr_aprop_lancto_ctbl_aux.tta_dat_cotac_indic_econ         = aprop_lancto_ctbl.dat_cotac_indic_econ    
                   tt_integr_aprop_lancto_ctbl_aux.tta_val_cotac_indic_econ         = aprop_lancto_ctbl.val_cotac_indic_econ    
                   tt_integr_aprop_lancto_ctbl_aux.tta_ind_orig_val_lancto_ctbl     = 'Integra‡Æo'.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-chama-api :

    DEF VAR i              AS INTEGER   NO-UNDO.
    DEF VAR v_des_mensagem AS CHARACTER NO-UNDO.
    DEF VAR v_des_ajuda    AS CHARACTER NO-UNDO.
    DEF VAR v_hdl_aux      AS HANDLE    NO-UNDO.

    FOR EACH tt_integr_lote_ctbl_1_aux NO-LOCK:

        EMPTY TEMP-TABLE tt_integr_lote_ctbl_1.
        EMPTY TEMP-TABLE tt_integr_lancto_ctbl_1.
        EMPTY TEMP-TABLE tt_integr_item_lanc_ctbl_1.
        EMPTY TEMP-TABLE tt_integr_aprop_lanc_ctbl_1.

        i = i + 1.

        PROCESS EVENTS.

        CREATE tt_integr_lote_ctbl_1 NO-ERROR.
        BUFFER-COPY tt_integr_lote_ctbl_1_aux EXCEPT tta_num_lote_ctbl TO tt_integr_lote_ctbl_1.
        ASSIGN tt_integr_lote_ctbl_1.tta_num_lote_ctbl = 0.

        FOR EACH tt_integr_lanc_ctbl_1_aux WHERE tt_integr_lanc_ctbl_1_aux.ttv_rec_integr_lote_ctbl = tt_integr_lote_ctbl_1_aux.ttv_rec_integr_lote_ctbl:
            
            CREATE tt_integr_lancto_ctbl_1 NO-ERROR.
            BUFFER-COPY tt_integr_lanc_ctbl_1_aux to tt_integr_lancto_ctbl_1.

            FOR EACH tt_integr_item_lanc_ctbl_1_aux WHERE tt_integr_item_lanc_ctbl_1_aux.ttv_rec_integr_lancto_ctbl = tt_integr_lanc_ctbl_1_aux.ttv_rec_integr_lancto_ctbl:
                CREATE tt_integr_item_lanc_ctbl_1 NO-ERROR.
                buffer-copy tt_integr_item_lanc_ctbl_1_aux to tt_integr_item_lanc_ctbl_1.

                FOR EACH tt_integr_aprop_lancto_ctbl_aux WHERE tt_integr_aprop_lancto_ctbl_aux.ttv_rec_integr_item_lancto_ctbl = tt_integr_item_lanc_ctbl_1_aux.ttv_rec_integr_item_lancto_ctbl:
                    CREATE tt_integr_aprop_lanc_ctbl_1 NO-ERROR.
                    buffer-copy tt_integr_aprop_lancto_ctbl_aux to tt_integr_aprop_lanc_ctbl_1.
                END.
            END.
        END.

        OUTPUT STREAM s_1 TO VALUE(SESSION:TEMP-DIRECTORY + "\cria_lotes.txt") NO-ECHO NO-CONVERT.

        RUN pi-acompanhar in h-acomp ("Criando Lote... ").

        ASSIGN v_cod_empres_usuar = es_cons_estab_emp.cod_empresa.

        RUN prgfin/fgl/fgl900zl.py (INPUT 3,
                                    INPUT "Aborta Lotes Errados",
                                    INPUT YES,
                                    INPUT 66,
                                    INPUT "Apropria‡Æo",
                                    INPUT "Com Erro",
                                    INPUT YES,
                                    INPUT YES,
                                    INPUT-OUTPUT TABLE tt_integr_lote_ctbl_1,
                                    INPUT-OUTPUT TABLE tt_integr_lancto_ctbl_1,
                                    INPUT-OUTPUT TABLE tt_integr_item_lanc_ctbl_1,
                                    INPUT-OUTPUT TABLE tt_integr_aprop_lanc_ctbl_1,
                                    INPUT-OUTPUT TABLE tt_integr_ctbl_valid_1).
        OUTPUT STREAM s_1 CLOSE.

        FOR EACH tt_lote_ctbl_orig NO-LOCK WHERE tt_lote_ctbl_orig.rec_lote = RECID(tt_integr_lote_ctbl_1_aux):
            IF CAN-FIND(FIRST tt_erro_integr_movto_ctbl) THEN DO:
                FOR EACH tt_erro_integr_movto_ctbl NO-LOCK WHERE tt_erro_integr_movto_ctbl.ttv_num_lancto_ctbl = tt_lote_ctbl_orig.num_lancto_orig
                                                           BREAK BY tt_erro_integr_movto_ctbl.ttv_des_msg_erro:

                    IF LAST-OF(tt_erro_integr_movto_ctbl.ttv_des_msg_erro) THEN DO:
                        FIND FIRST tt_integr_lanc_ctbl_1_aux NO-LOCK WHERE tt_integr_lanc_ctbl_1_aux.ttv_rec_integr_lote_ctbl = tt_integr_lote_ctbl_1_aux.ttv_rec_integr_lote_ctbl
                                                                       AND tt_integr_lanc_ctbl_1_aux.tta_num_lancto_ctbl      = tt_erro_integr_movto_ctbl.ttv_num_lancto_Ctbl NO-ERROR.
                        IF AVAIL tt_integr_lanc_ctbl_1_aux THEN DO:
                            FIND FIRST tt_es_cons_lotes EXCLUSIVE-LOCK WHERE tt_es_cons_lotes.num_lote_orig     = tt_lote_ctbl_orig.num_lote_orig
                                                                         AND tt_es_cons_lotes.cod_empresa_orig  = tt_lote_ctbl_orig.cod_empresa 
                                                                         AND tt_es_cons_lotes.num_lancto_orig   = tt_lote_ctbl_orig.num_lancto_orig NO-ERROR.
                            IF NOT AVAIL tt_es_cons_lotes THEN DO:
                                CREATE tt_es_cons_lotes NO-ERROR.
                                ASSIGN tt_es_cons_lotes.cod_empresa_orig = tt_lote_ctbl_orig.cod_empresa
                                       tt_es_cons_lotes.num_lote_orig    = tt_lote_ctbl_orig.num_lote_orig
                                       tt_es_cons_lotes.des_lote_orig    = tt_lote_ctbl_orig.des_lote_orig
                                       tt_es_cons_lotes.dt_lote_orig     = tt_lote_ctbl_orig.dt_lote_orig
                                       tt_es_cons_lotes.num_lancto_orig  = tt_lote_ctbl_orig.num_lancto_orig
                                       tt_es_cons_lotes.dt_lancto_orig   = tt_lote_ctbl_orig.dt_lancto_orig
                                       tt_es_cons_lotes.cod_mod_orig     = tt_lote_ctbl_orig.cod_mod_orig
                                       tt_es_cons_lotes.cod_cenario_orig = tt_lote_ctbl_orig.cod_cenario_orig
                                       tt_es_cons_lotes.cod_empresa_dest = ""
                                       tt_es_cons_lotes.num_lote_dest    = 0
                                       tt_es_cons_lotes.des_lote_dest    = ""
                                       tt_es_cons_lotes.dt_lote_dest     = ?
                                       tt_es_cons_lotes.dt_lancto_dest   = ?
                                       tt_es_cons_lotes.resultado        = STRING(tt_erro_integr_movto_ctbl.ttv_num_cod_erro) + '#' /* trim(tt_erro_integr_movto_ctbl.ttv_des_msg_erro)*/
                                       tt_es_cons_lotes.cria_reg         = YES
                                       tt_es_cons_lotes.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).
                                
                                CREATE tt_msg_erro_es_lote.
                                ASSIGN tt_msg_erro_es_lote.des_msg_erro      = tt_erro_integr_movto_ctbl.ttv_des_msg_erro
                                       tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).                                
                            END.
                            ELSE DO :
                                /*IF LENGTH(TRIM(tt_es_cons_lotes.resultado)) < 500 THEN*/
                                ASSIGN tt_es_cons_lotes.resultado = tt_es_cons_lotes.resultado + STRING(tt_erro_integr_movto_ctbl.ttv_num_cod_erro) + '#'.

                                CREATE tt_msg_erro_es_lote.
                                ASSIGN tt_msg_erro_es_lote.des_msg_erro      = tt_erro_integr_movto_ctbl.ttv_des_msg_erro
                                       tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).                                
                            END.
                            IF AVAIL tt_es_cons_lotes THEN RUN pi-cria-tabela-es-lotes (INPUT ROWID(tt_es_cons_lotes)).
                        END.                            
                    END.
                END.
            END.

            IF CAN-FIND(FIRST tt_integr_ctbl_valid_1) THEN DO:
                FOR EACH tt_integr_ctbl_valid_1 NO-LOCK BREAK BY tt_integr_ctbl_valid_1.ttv_num_mensagem:
                    IF LAST-OF(tt_integr_ctbl_valid_1.ttv_num_mensagem) THEN DO:

                        RUN pi_msg_lote_ctbl_recebto_1 (INPUT tt_integr_ctbl_valid_1.ttv_num_mensagem,
                                                        OUTPUT v_des_mensagem,
                                                        OUTPUT v_des_ajuda).

                        FIND FIRST tt_es_cons_lotes EXCLUSIVE-LOCK WHERE tt_es_cons_lotes.num_lote_orig    = tt_lote_ctbl_orig.num_lote_orig
                                                                     AND tt_es_cons_lotes.cod_empresa_orig = tt_lote_ctbl_orig.cod_empresa
                                                                     AND tt_es_Cons_lotes.num_lancto_orig  = tt_lote_ctbl_orig.num_lancto_orig NO-ERROR.
                        IF NOT AVAIL tt_es_cons_lotes THEN DO:
                            CREATE tt_es_cons_lotes NO-ERROR.
                            ASSIGN tt_es_cons_lotes.cod_empresa_orig = tt_lote_ctbl_orig.cod_empresa
                                   tt_es_cons_lotes.num_lote_orig    = tt_lote_ctbl_orig.num_lote_orig     
                                   tt_es_cons_lotes.des_lote_orig    = tt_lote_ctbl_orig.des_lote_orig     
                                   tt_es_cons_lotes.dt_lote_orig     = tt_lote_ctbl_orig.dt_lote_orig      
                                   tt_es_cons_lotes.dt_lancto_orig   = tt_lote_ctbl_orig.dt_lancto_orig    
                                   tt_es_cons_lotes.num_lancto_orig  = tt_lote_ctbl_orig.num_lancto_orig
                                   tt_es_cons_lotes.cod_mod_orig     = tt_lote_ctbl_orig.cod_mod_orig      
                                   tt_es_cons_lotes.cod_cenario_orig = tt_lote_ctbl_orig.cod_cenario_orig  
                                   tt_es_cons_lotes.cod_empresa_dest = ""
                                   tt_es_cons_lotes.num_lote_dest    = 0
                                   tt_es_cons_lotes.des_lote_dest    = ""
                                   tt_es_cons_lotes.dt_lote_dest     = ?
                                   tt_es_cons_lotes.dt_lancto_dest   = ?
                                   tt_es_cons_lotes.cria_reg         = YES
                                   tt_es_cons_lotes.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).

                            ASSIGN tt_es_cons_lotes.resultado        = trim(STRING(tt_integr_ctbl_valid_1.ttv_num_mensagem)) + '#'.
                            
                            CREATE tt_msg_erro_es_lote.
                            ASSIGN tt_msg_erro_es_lote.des_msg_erro      = "Erro " + trim(STRING(tt_integr_ctbl_valid_1.ttv_num_mensagem)  + ' ' + v_des_mensagem + ' - ' +  v_des_ajuda )
                                   tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).                            
                        END.
                        ELSE DO: 
                            /*IF LENGTH(TRIM(tt_es_cons_lotes.resultado)) < 500 THEN*/
                            ASSIGN tt_es_cons_lotes.resultado        = tt_es_cons_lotes.resultado + trim(string(tt_integr_ctbl_valid_1.ttv_num_mensagem)) + '#'.
                            CREATE tt_msg_erro_es_lote.
                            ASSIGN tt_msg_erro_es_lote.des_msg_erro      = "Erro " + trim(string(tt_integr_ctbl_valid_1.ttv_num_mensagem)  +  ' ' + v_des_mensagem + ' - ' +  v_des_ajuda )
                                   tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).                            
                        END.                        
                        IF AVAIL tt_es_cons_lotes THEN RUN pi-cria-tabela-es-lotes (INPUT ROWID(tt_es_cons_lotes)).
                    END.                    
                END.
            END.

            FIND FIRST tt_integr_lote_ctbl_1 NO-LOCK WHERE tt_integr_lote_ctbl_1.tta_num_lote_ctbl <> 0 NO-ERROR.
            IF AVAIL tt_integr_lote_ctbl_1 THEN DO:

                FIND FIRST tt_integr_lancto_ctbl_1 NO-LOCK WHERE tt_integr_lancto_ctbl_1.ttv_rec_integr_lote_ctbl = tt_integr_lote_ctbl_1.ttv_rec_integr_lote_ctbl
                                                             AND tt_integr_lancto_ctbl_1.tta_num_lancto_ctbl      = tt_lote_ctbl_orig.num_lancto_orig NO-ERROR.
                IF AVAIL tt_integr_lancto_ctbl_1 THEN DO:
                    FIND FIRST tt_es_cons_lotes WHERE tt_es_cons_lotes.num_lote_orig    = tt_lote_ctbl_orig.num_lote_orig
                                                  AND tt_es_cons_lotes.cod_empresa_orig = tt_lote_ctbl_orig.cod_empresa
                                                  AND tt_es_cons_lotes.num_lancto_orig  = tt_lote_ctbl_orig.num_lancto_orig NO-ERROR.
                    IF NOT AVAIL tt_es_cons_lotes THEN DO:
                        CREATE tt_es_cons_lotes NO-ERROR.
                        ASSIGN tt_es_cons_lotes.cod_empresa_orig = tt_lote_ctbl_orig.cod_empresa
                               tt_es_cons_lotes.num_lote_orig    = tt_lote_ctbl_orig.num_lote_orig     
                               tt_es_cons_lotes.num_lancto_orig  = tt_lote_ctbl_orig.num_lancto_orig
                               tt_es_cons_lotes.des_lote_orig    = tt_lote_ctbl_orig.des_lote_orig     
                               tt_es_cons_lotes.dt_lote_orig     = tt_lote_ctbl_orig.dt_lote_orig      
                               tt_es_cons_lotes.dt_lancto_orig   = tt_lote_ctbl_orig.dt_lancto_orig    
                               tt_es_cons_lotes.cod_mod_orig     = tt_lote_ctbl_orig.cod_mod_orig      
                               tt_es_cons_lotes.cod_cenario_orig = tt_lote_ctbl_orig.cod_cenario_orig  
                               tt_es_cons_lotes.cod_empresa_dest = tt_integr_lote_ctbl_1.tta_cod_empresa
                               tt_es_cons_lotes.num_lote_dest    = tt_integr_lote_ctbl_1.tta_num_lote_ctbl 
                               tt_es_cons_lotes.num_lancto_dest  = tt_integr_lancto_ctbl_1.tta_num_lancto_ctbl
                               tt_es_cons_lotes.des_lote_dest    = tt_integr_lote_ctbl_1.tta_des_lote_ctbl 
                               tt_es_cons_lotes.dt_lote_dest     = tt_integr_lote_ctbl_1.tta_dat_lote_ctbl 
                               tt_es_cons_lotes.dt_lancto_dest   = tt_lote_ctbl_orig.dt_lancto_orig
                               tt_es_cons_lotes.resultado        = "Lote Criado com Sucesso."
                               tt_es_cons_lotes.cria_reg         = YES
                               tt_es_cons_lotes.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).

                        CREATE tt_msg_erro_es_lote.
                        ASSIGN tt_msg_erro_es_lote.des_msg_erro      = "Lote Criado com Sucesso."
                               tt_msg_erro_es_lote.ROW_es_cons_lotes = ROWID(tt_es_cons_lotes).                    

                        RUN pi-cria-tabela-es-lotes (INPUT ROWID(tt_es_cons_lotes)).

                    END.
                END.               
            END.                        
        END.
        EMPTY TEMP-TABLE tt_integr_ctbl_valid_1.
        FOR EACH tt_erro_integr_movto_ctbl:
            DELETE tt_erro_integr_movto_ctbl.
        END.
    END.

end PROCEDURE.

PROCEDURE pi-imprime-log :

    
    IF tt_param.class-1 = "Lote Cont bil"           AND tt_param.class-2 = "Data Lote Cont bil" THEN DO:
        {esp/esfgl072.i tt_es_cons_lotes.num_lote_dest tt_es_cons_lotes.dt_lote_dest}
    END.
    ELSE IF tt_param.class-1 = "Lote Cont bil"      AND tt_param.class-2 = "Data Lan‡amento"    THEN DO:
        {esp/esfgl072.i tt_es_cons_lotes.num_lote_dest tt_es_cons_lotes.dt_lancto_orig}
    END.
    ELSE IF tt_param.class-1 = "Data Lote Cont bil" AND tt_param.class-2 = "Lote Cont bil"      THEN DO:
        {esp/esfgl072.i tt_es_cons_lotes.dt_lote_dest tt_es_cons_lotes.num_lote_dest}
    END.
    ELSE IF tt_param.class-1 = "Data Lote Cont bil" AND tt_param.class-2 = "Data Lan‡amento"    THEN DO:
        {esp/esfgl072.i tt_es_cons_lotes.dt_lote_dest tt_es_cons_lotes.dt_lancto_orig}
    END.
    ELSE IF tt_param.class-1 = "Data Lan‡amento"    AND tt_param.class-2 = "Data Lote Cont bil" THEN DO:
        {esp/esfgl072.i tt_es_cons_lotes.dt_lancto_orig tt_es_cons_lotes.dt_lote_dest}
    END.
    ELSE IF tt_param.class-1 = "Data Lan‡amento"    AND tt_param.class-2 = "Lote Cont bil"      THEN DO:
        {esp/esfgl072.i tt_es_cons_lotes.dt_lancto_orig tt_es_cons_lotes.num_lote_dest}
    END.        
    

END PROCEDURE.

PROCEDURE pi-cria-tabela-es-lotes :

    DEFINE INPUT PARAMETER p_rowid_cons_lotes AS ROWID NO-UNDO.

    DEF BUFFER bf_tt_es_Cons_lotes FOR tt_es_cons_lotes.

    FIND FIRST bf_tt_es_cons_lotes WHERE rowid(bf_tt_es_cons_lotes)           = p_rowid_cons_lotes 
                                     AND bf_tt_es_cons_lotes.cria_reg         = YES NO-LOCK NO-ERROR.
    IF AVAIL bf_tt_es_cons_lotes THEN DO:
        
        FIND FIRST es_cons_lotes EXCLUSIVE-LOCK WHERE es_cons_lotes.cod_empresa_orig = bf_tt_es_cons_lotes.cod_empresa_orig
                                                  AND es_cons_lotes.num_lote_orig    = bf_tt_es_cons_lotes.num_lote_orig
                                                  AND es_cons_lotes.num_lancto_orig  = bf_tt_es_cons_lotes.num_lancto_orig NO-ERROR.
        IF NOT AVAIL es_cons_lotes THEN DO:
            CREATE es_cons_lotes NO-ERROR.
            ASSIGN es_cons_lotes.cod_empresa_orig = bf_tt_es_cons_lotes.cod_empresa_orig
                   es_cons_lotes.num_lote_orig    = bf_tt_es_cons_lotes.num_lote_orig
                   es_cons_lotes.num_lancto_orig  = bf_tt_es_cons_lotes.num_lancto_orig.
        END.
        ASSIGN es_cons_lotes.des_lote_orig    = bf_tt_es_cons_lotes.des_lote_orig
               es_cons_lotes.dt_lote_orig     = bf_tt_es_cons_lotes.dt_lote_orig
               es_cons_lotes.dt_lancto_orig   = bf_tt_es_cons_lotes.dt_lancto_orig
               es_cons_lotes.cod_mod_orig     = bf_tt_es_cons_lotes.cod_mod_orig
               es_cons_lotes.cod_cenario_orig = bf_tt_es_cons_lotes.cod_cenario_orig
               es_cons_lotes.cod_empresa_dest = bf_tt_es_cons_lotes.cod_empresa_dest
               es_cons_lotes.num_lote_dest    = bf_tt_es_cons_lotes.num_lote_dest
               es_cons_lotes.num_lancto_dest  = bf_tt_es_cons_lotes.num_lancto_dest
               es_cons_lotes.des_lote_dest    = bf_tt_es_cons_lotes.des_lote_dest
               es_cons_lotes.dt_lancto_dest   = bf_tt_es_cons_lotes.dt_lancto_dest
               es_cons_lotes.dt_lote_dest     = bf_tt_es_cons_lotes.dt_lote_dest
               es_cons_lotes.resultado        = bf_tt_es_cons_lotes.resultado.
    END.     

    
end PROCEDURE.
/*****************************************************************************
** PROCEDURE Interna.....: pi_show_report_2
** Descricao.............: pi_show_report_2
*****************************************************************************/
PROCEDURE pi_show_report_2:

    /************************ PARAMETEReter Definition Begin ************************/

    DEFINE INPUT PARAMETER p_cod_dwb_file
        AS CHARACTER
        FORMAT "x(40)"
        NO-UNDO.

    /************************* PARAMETEReter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_key_value
        AS CHARACTER
        FORMAT "x(8)":U
        NO-UNDO.

    /************************** Variable Definition End *************************/

    get-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value.
    if  v_cod_key_value = ""
    or   v_cod_key_value = ? THEN
        ASSIGN v_cod_key_value = 'notepad.exe'.
        put-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value NO-ERROR.

    RUN winexec (INPUT v_cod_key_value + chr(32) + p_cod_dwb_file, INPUT 1).

    end PROCEDURE.

    PROCEDURE WinExec EXTERNAL 'kernel32.dll':
      DEF INPUT  PARAMETER prg_name                          AS CHARACTER.
      DEF INPUT  PARAMETER prg_style                         AS SHORT.
end PROCEDURE. /* pi_show_report_2 */

/************************** Internal PROCEDURE End **************************/

/***********************  End of esfgl072rp ***********************/



PROCEDURE pi_msg_lote_ctbl_recebto_1:

    def Input param p_num_mensagem  as INTEGER   format ">>>>,>>9" no-undo.
    def output param p_des_mensagem as CHARACTER format "x(50)"    no-undo.
    def output param p_des_ajuda    as CHARACTER format "x(50)"    no-undo.

    ASSIGN v_num_mensagem = p_num_mensagem.
    RUN pi_view_message.
    ASSIGN p_des_mensagem = v_des_message_view
           p_des_ajuda    = v_des_ajuda_view. 

    
END PROCEDURE. /* pi_msg_lote_ctbl_recebto_1 */

PROCEDURE pi_messages: 

    def input param c_action    as char    no-undo. 
    def input param i_msg       as integer no-undo. 
    def input param c_param     as char    no-undo. 

    def var c_prg_msg           as char    no-undo. 

    assign c_prg_msg = "messages/" 
                     + string(trunc(i_msg / 1000,0),"99") 
                     + "/msg" 
                     + string(i_msg, "99999"). 

    if search(c_prg_msg + ".r") = ? and search(c_prg_msg + ".p") = ? then do: 
        message "Mensagem nr. " i_msg "!!!" skip 
                "Programa Mensagem" c_prg_msg "n’o encontrado." 
                view-as alert-box error. 
        return error. 
    end. 

    run value(c_prg_msg + ".p") (input c_action, input c_param). 
    return return-value. 
END PROCEDURE.

PROCEDURE pi_view_message: 

    assign v_cod_msg = 'messages/' 
                     + string(trunc(v_num_mensagem / 1000,0),'99') 
                     + '/msg' 
                     + string(v_num_mensagem, '99999'). 

    if  search(v_cod_msg + '.r') = ? and search(v_cod_msg + '.p') = ? 
    then do: 
        /* A mensagem &1 n’o estÿ dispon­vel em seu ambiente ! */ 
        run pi_messages (input "show", 
                         input 3530, 
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9", 
                                           v_num_mensagem)) /*msg_3530*/. 
        assign v_des_ajuda_view = ''
               v_des_message_view = ''
               v_ind_tip_msg = ''.
        return 'nok'. 
    end /* if */. 

    run pi_messages (input "type", 
                     input v_num_mensagem, 
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")). 
    assign v_ind_tip_msg = return-value. 
    run pi_messages (input "msg", 
                     input v_num_mensagem, 
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")). 
    assign v_des_message_view = return-value. 
    run pi_messages (input "help", 
                     input v_num_mensagem, 
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")). 
    assign v_des_ajuda_view = return-value. 
END PROCEDURE.





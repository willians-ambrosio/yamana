
DEF NEW SHARED TEMP-TABLE tt_integr_aprop_lanc_ctbl_1 NO-UNDO
    FIELD tta_cod_finalid_econ             AS CHARACTER  FORMAT "x(10)" LABEL "Finalidade" COLUMN-LABEL "Finalidade"
    FIELD tta_cod_unid_negoc               AS CHARACTER  FORMAT "x(03)" LABEL "Unid Neg¢cio" COLUMN-LABEL "Un Neg"
    FIELD tta_cod_plano_ccusto             AS CHARACTER  FORMAT "x(08)" LABEL "Plano Centros Custo" COLUMN-LABEL "Plano Centros Custo"
    FIELD tta_qtd_unid_lancto_ctbl         AS DECIMAL    FORMAT ">>,>>>,>>9.99" decimals 2 initial 0 LABEL "Quantidade" COLUMN-LABEL "Quantidade"
    FIELD tta_val_lancto_ctbl              AS DECIMAL    FORMAT ">>>>>,>>>,>>9.99" decimals 2 initial 0 LABEL "Valor Lan‡amento" COLUMN-LABEL "Valor Lan‡amento"
    FIELD tta_num_id_aprop_lancto_ctbl     AS INTEGER    FORMAT "9999999999" initial 0 LABEL "Apropriacao Lan‡to" COLUMN-LABEL "Apropriacao Lan‡to"
    FIELD ttv_rec_integr_item_lancto_ctbl  AS RECID      FORMAT ">>>>>>9"
    FIELD tta_dat_cotac_indic_econ         AS DATE       FORMAT "99/99/9999" initial ? LABEL "Data Cota‡Æo" COLUMN-LABEL "Data Cota‡Æo"
    FIELD tta_val_cotac_indic_econ         AS DECIMAL    FORMAT ">>>>,>>9.9999999999" decimals 10 initial 0 LABEL "Cota‡Æo" COLUMN-LABEL "Cota‡Æo"
    FIELD ttv_ind_erro_valid               AS CHARACTER  FORMAT "x(08)" initial "NÆo"
    FIELD tta_ind_orig_val_lancto_ctbl     AS CHARACTER  FORMAT "x(10)" initial "Informado" LABEL "Origem Valor" COLUMN-LABEL "Origem Valor"
    FIELD tta_cod_ccusto                   AS CHARACTER  FORMAT "x(11)" LABEL "Centro Custo" COLUMN-LABEL "Centro Custo"
    FIELD ttv_rec_integr_aprop_lancto_ctbl AS RECID      FORMAT ">>>>>>9"
    INDEX tt_id                            IS PRIMARY UNIQUE
          ttv_rec_integr_item_lancto_ctbl
          tta_cod_finalid_econ
          tta_cod_unid_negoc
          tta_cod_plano_ccusto
          tta_cod_ccusto
    INDEX tt_RECID
          ttv_rec_integr_aprop_lancto_ctbl.


DEF NEW SHARED TEMP-TABLE tt_integr_aprop_lancto_ctbl_aux NO-UNDO like tt_integr_aprop_lanc_ctbl_1.

DEF NEW SHARED TEMP-TABLE tt_integr_ctbl_valid_1 NO-UNDO
    FIELD ttv_rec_integr_ctbl AS RECID FORMAT ">>>>>>9"
    FIELD ttv_num_mensagem    AS INTEGER    FORMAT ">>>>,>>9" LABEL "N£mero" COLUMN-LABEL "N£mero Mensagem"
    FIELD ttv_ind_pos_erro    AS CHARACTER  FORMAT "x(08)"    LABEL "Posi‡Æo"
    INDEX tt_id               IS PRIMARY UNIQUE
          ttv_rec_integr_ctbl
          ttv_num_mensagem.

/* aparentemente a tabela "tt_integr_ctbl_valid_1_aux" nÆo est  sendo usada */
DEF NEW SHARED TEMP-TABLE tt_integr_ctbl_valid_1_aux NO-UNDO like tt_integr_ctbl_valid_1.

DEF NEW SHARED TEMP-TABLE tt_integr_item_lanc_ctbl_1 NO-UNDO
    FIELD ttv_rec_integr_lancto_ctbl      AS RECID      FORMAT ">>>>>>9"            
    FIELD tta_num_seq_lancto_ctbl         AS INTEGER    FORMAT ">>>>9"              LABEL "Sequˆncia Lan‡to" COLUMN-LABEL "Sequˆncia Lan‡to"   initial 0    
    FIELD tta_ind_natur_lancto_ctbl       AS CHARACTER  FORMAT "x(02)"              LABEL "Natureza" COLUMN-LABEL "Natureza"                   initial "DB" 
    FIELD tta_cod_plano_cta_ctbl          AS CHARACTER  FORMAT "x(8)"               LABEL "Plano Contas" COLUMN-LABEL "Plano Contas"
    FIELD tta_cod_cta_ctbl                AS CHARACTER  FORMAT "x(20)"              LABEL "Conta Cont bil" COLUMN-LABEL "Conta Cont bil"
    FIELD tta_cod_plano_ccusto            AS CHARACTER  FORMAT "x(8)"               LABEL "Plano Centros Custo" COLUMN-LABEL "Plano Centros Custo"
    FIELD tta_cod_estab                   AS CHARACTER  FORMAT "x(3)"               LABEL "Estabelecimento" COLUMN-LABEL "Estab"
    FIELD tta_cod_unid_negoc              AS CHARACTER  FORMAT "x(3)"               LABEL "Unid Neg¢cio" COLUMN-LABEL "Un Neg"
    FIELD tta_cod_histor_padr             AS CHARACTER  FORMAT "x(8)"               LABEL "Hist¢rico PadrÆo" COLUMN-LABEL "Hist¢rico PadrÆo"
    FIELD tta_des_histor_lancto_ctbl      AS CHARACTER  FORMAT "x(2000)"            LABEL "Hist¢rico Cont bil" COLUMN-LABEL "Hist¢rico Cont bil"
    FIELD tta_cod_espec_docto             AS CHARACTER  FORMAT "x(3)"               LABEL "Esp‚cie Documento" COLUMN-LABEL "Esp‚cie"
    FIELD tta_dat_docto                   AS DATE       FORMAT "99/99/9999"         LABEL "Data Documento" COLUMN-LABEL "Data Documento" initial ? 
    FIELD tta_des_docto                   AS CHARACTER  FORMAT "x(25)"              LABEL "N£mero Documento" COLUMN-LABEL "N£mero Documento"
    FIELD tta_cod_imagem                  AS CHARACTER  FORMAT "x(30)"              LABEL "Imagem" COLUMN-LABEL "Imagem"
    FIELD tta_cod_indic_econ              AS CHARACTER  FORMAT "x(8)"               LABEL "Moeda" COLUMN-LABEL "Moeda"
    FIELD tta_dat_lancto_ctbl             AS DATE       FORMAT "99/99/9999"         LABEL "Data Lan‡amento" COLUMN-LABEL "Data Lan‡to" initial ? 
    FIELD tta_qtd_unid_lancto_ctbl        AS DECIMAL    FORMAT ">>,>>>,>>9.99"      LABEL "Quantidade" COLUMN-LABEL "Quantidade" decimals 2 initial 0 
    FIELD tta_val_lancto_ctbl             AS DECIMAL    FORMAT ">>>>>,>>>,>>9.99"   LABEL "Valor Lan‡amento" COLUMN-LABEL "Valor Lan‡amento" decimals 2 initial 0 
    FIELD tta_num_seq_lancto_ctbl_cpart   AS INTEGER    FORMAT ">>>9"               LABEL "Sequˆncia CPartida" COLUMN-LABEL "Sequˆncia CP" initial 0 
    FIELD ttv_ind_erro_valid              AS CHARACTER  FORMAT "x(08)" initial "NÆo"
    FIELD tta_cod_ccusto                  AS CHARACTER  FORMAT "x(11)"              LABEL "Centro Custo" COLUMN-LABEL "Centro Custo"
    FIELD tta_cod_proj_financ             AS CHARACTER  FORMAT "x(20)"              LABEL "Projeto" COLUMN-LABEL "Projeto"
    FIELD ttv_rec_integr_item_lancto_ctbl AS RECID FORMAT ">>>>>>9"
    INDEX tt_id                           IS PRIMARY UNIQUE
          ttv_rec_integr_lancto_ctbl
          tta_num_seq_lancto_ctbl
    INDEX tt_RECID
          ttv_rec_integr_item_lancto_ctbl.

DEF NEW SHARED TEMP-TABLE tt_integr_item_lanc_ctbl_1_aux NO-UNDO like tt_integr_item_lanc_ctbl_1.

def TEMP-TABLE tt_integr_lancto_ctbl_1 NO-UNDO
    FIELD tta_cod_cenar_ctbl           AS CHARACTER  FORMAT "x(8)" LABEL "Cen rio Cont bil" COLUMN-LABEL "Cen rio Cont bil"
    FIELD tta_log_lancto_conver        as log   FORMAT "Sim/NÆo" initial no LABEL "Lan‡amento ConversÆo" COLUMN-LABEL "Lan‡to Conv"
    FIELD tta_log_lancto_apurac_restdo as log   FORMAT "Sim/NÆo" initial no LABEL "Lan‡amento Apura‡Æo" COLUMN-LABEL "Lancto Apura‡Æo"
    FIELD tta_cod_rat_ctbl             AS CHARACTER  FORMAT "x(8)" LABEL "Rateio Cont bil" COLUMN-LABEL "Rateio"
    FIELD ttv_rec_integr_lote_ctbl     AS RECID FORMAT ">>>>>>9"
    FIELD tta_num_lancto_ctbl          AS INTEGER   FORMAT ">>,>>>,>>9" initial 10 LABEL "Lan‡amento Cont bil" COLUMN-LABEL "Lan‡amento Cont bil"
    FIELD ttv_ind_erro_valid           AS CHARACTER  FORMAT "x(08)" initial "NÆo"
    FIELD tta_dat_lancto_ctbl          AS DATE  FORMAT "99/99/9999" initial ? LABEL "Data Lan‡amento" COLUMN-LABEL "Data Lan‡to"
    FIELD ttv_rec_integr_lancto_ctbl   AS RECID FORMAT ">>>>>>9"
    INDEX tt_id                        IS PRIMARY UNIQUE
          ttv_rec_integr_lote_ctbl
          tta_num_lancto_ctbl
    INDEX tt_RECID
          ttv_rec_integr_lancto_ctbl.

DEF NEW SHARED TEMP-TABLE tt_integr_lanc_ctbl_1_aux NO-UNDO like tt_integr_lancto_ctbl_1.
    
DEF NEW SHARED TEMP-TABLE tt_integr_lote_ctbl_1 NO-UNDO
    FIELD tta_cod_modul_dtsul        AS CHARACTER  FORMAT "x(3)" LABEL "M¢dulo" COLUMN-LABEL "M¢dulo"
    FIELD tta_num_lote_ctbl          AS INTEGER   FORMAT ">>>,>>>,>>9" initial 1 LABEL "Lote Cont bil" COLUMN-LABEL "Lote Cont bil"
    FIELD tta_des_lote_ctbl          AS CHARACTER  FORMAT "x(40)" LABEL "Descri‡Æo Lote" COLUMN-LABEL "Descri‡Æo Lote"
    FIELD tta_cod_empresa            AS CHARACTER  FORMAT "x(3)" LABEL "Empresa" COLUMN-LABEL "Empresa"
    FIELD tta_dat_lote_ctbl          AS DATE  FORMAT "99/99/9999" initial today LABEL "Data Lote Cont bil" COLUMN-LABEL "Data Lote Cont bil"
    FIELD ttv_ind_erro_valid         AS CHARACTER  FORMAT "x(08)" initial "NÆo"
    FIELD tta_log_integr_ctbl_online as log   FORMAT "Sim/NÆo" initial no LABEL "Integra‡Æo Online" COLUMN-LABEL "Integr Online"
    FIELD ttv_rec_integr_lote_ctbl   AS RECID FORMAT ">>>>>>9"
    INDEX tt_RECID
          ttv_rec_integr_lote_ctbl.

DEF NEW SHARED TEMP-TABLE tt_integr_lote_ctbl_1_aux NO-UNDO like tt_integr_lote_ctbl_1.

DEF NEW SHARED TEMP-TABLE tt_lote_ctbl_orig NO-UNDO
    FIELD cod_empresa   LIKE lote_ctbl.cod_empresa
    FIELD num_lote_orig like lote_ctbl.num_lote_ctbl
    FIELD num_lancto_orig LIKE lancto_ctbl.num_lancto_ctbl
    FIELD des_lote_orig like lote_ctbl.des_lote_ctbl
    FIELD dt_lote_orig  like lote_ctbl.dat_lote_ctbl
    FIELD dt_lancto_orig  LIKE lancto_ctbl.dat_lancto_ctbl
    FIELD cod_mod_orig    LIKE lancto_ctbl.cod_modul_dtsul
    FIELD cod_cenario_orig LIKE lancto_ctbl.cod_cenar_ctbl
    FIELD rec_lote      AS RECID
    INDEX tt_lote       IS PRIMARY UNIQUE
          num_lote_orig
          num_lancto_orig.

/* aparentemente a tabela "tt_erro_integr_movto_ctbl" nÆo est  sendo usada */
def TEMP-TABLE tt_erro_integr_movto_ctbl
    FIELD ttv_cod_empresa     AS CHARACTER FORMAT "x(3)" LABEL "Empresa" COLUMN-LABEL "Empresa"
    FIELD ttv_num_lote_ctbl   AS INTEGER  FORMAT ">>>,>>>,>>9" LABEL "Lote Cont bil" COLUMN-LABEL "Lote Cont bil"
    FIELD ttv_num_lancto_ctbl AS INTEGER  FORMAT ">>,>>>,>>9" LABEL "Lan‡amento Cont bil" COLUMN-LABEL "Lan‡amento Cont bil"
    FIELD ttv_num_cod_erro    AS INTEGER  FORMAT ">>>>,>>9" LABEL "N£mero" COLUMN-LABEL "N£mero"
    FIELD ttv_des_msg_erro    AS CHARACTER FORMAT "x(60)" LABEL "Mensagem Erro" COLUMN-LABEL "Inconsistˆncia".

/* aparentemente a tabela "tt_erro_integr_movto_ctbl_aux" nÆo est  sendo usada */
def TEMP-TABLE tt_erro_integr_movto_ctbl_aux like tt_erro_integr_movto_ctbl.

def TEMP-TABLE tt_es_cons_lotes like es_cons_lotes
    FIELD cria_reg          AS LOGICAL
    FIELD ROW_es_cons_lotes AS ROWID
    INDEX destino cod_empresa_dest
                  num_lote_dest
    INDEX origem IS PRIMARY UNIQUE cod_empresa_orig
                                   num_lote_orig
                                   num_lancto_orig.

DEF TEMP-TABLE tt_msg_erro_es_lote NO-UNDO
    FIELD des_msg_erro            AS CHARACTER 
    FIELD ROW_es_cons_lotes       AS ROWID.

/************************** STREAM Definition Begin *************************/
DEF NEW SHARED STREAM s_1.

def TEMP-TABLE tt_param NO-UNDO
    FIELD c_emp_orig_ini AS CHARACTER FORMAT "x(03)"
    FIELD c_emp_orig_fim AS CHARACTER FORMAT "x(03)"
    FIELD c_emp_dest_ini AS CHARACTER FORMAT "x(03)"
    FIELD c_emp_dest_fim AS CHARACTER FORMAT "x(03)"
    FIELD c_estab_ini    AS CHARACTER FORMAT "x(03)"
    FIELD c_estab_fim    AS CHARACTER FORMAT "x(03)"
    FIELD i_lote_ini     AS INTEGER  FORMAT ">>>>>>>>9"
    FIELD i_lote_fim     AS INTEGER  FORMAT ">>>>>>>>9"
    FIELD dt_lote_ini    AS DATE FORMAT 99/99/9999
    FIELD dt_lote_fim    AS DATE FORMAT 99/99/9999
    FIELD dt_lanca_ini   AS DATE FORMAT 99/99/9999
    FIELD dt_lanca_fim   AS DATE FORMAT 99/99/9999
    FIELD c_cenario_ini  AS CHARACTER FORMAT "x(09)"
    FIELD c_cenario_fim  AS CHARACTER FORMAT "x(09)"
    FIELD c_modulo_ini   AS CHARACTER FORMAT "x(03)"
    FIELD c_modulo_fim   AS CHARACTER FORMAT "x(03)"
    FIELD class-1        AS CHARACTER FORMAT "x(15)"
    FIELD class-2        AS CHARACTER FORMAT "x(15)"
    FIELD class-3        AS CHARACTER FORMAT "x(15)"
    FIELD rs_tipo_log    AS CHARACTER
    FIELD rs_OUTPUT      AS CHARACTER
    FIELD c_arquivo      AS CHARACTER FORMAT "x(30)".

def TEMP-TABLE tt-editor NO-UNDO
    FIELD linha    AS INTEGER
    FIELD conteudo AS CHARACTER FORMAT "x(80)"
    INDEX editor-id IS PRIMARY UNIQUE linha.

def buffer bf-lote_ctbl for lote_ctbl.

def new global shared var v_cod_empres_usuar AS CHARACTER FORMAT "x(3)":U LABEL "Empresa" COLUMN-LABEL "Empresa" NO-UNDO.
def new shared var v_cod_dwb_program as CHARACTER format "x(32)":U label "Programa" column-label "Programa" no-undo.
def new global shared var v_cod_dwb_user as CHARACTER format "x(21)":U label "Usu rio" column-label "Usu rio" no-undo.

/* -----------------[   Defini‡Æo de Forms    ]------------------- */
form tt_es_cons_lotes.cod_empresa_dest  at 1 COLUMN-LABEL "E.D."           space(1)
     tt_es_cons_lotes.num_lote_orig          COLUMN-LABEL "L.O."           space(0)
     tt_es_cons_lotes.des_lote_orig          COLUMN-LABEL "Descri‡Æo L.O." space(0)
     tt_es_cons_lotes.num_lancto_orig        COLUMN-LABEL "Lanc Orig"      SPACE(0)
     tt_es_cons_lotes.num_lote_dest          COLUMN-LABEL "L.D."           space(0)
     tt_es_cons_lotes.des_lote_dest          COLUMN-LABEL "Descri‡Æo L.D." space(0)
     tt_es_cons_lotes.num_lancto_dest        COLUMN-LABEL "Lanc Dest"      SPACE(0)
     tt_es_cons_lotes.cod_mod_orig           COLUMN-LABEL "Mod. O."        space(0)     
     with no-box width 164 attr-space 64 down frame f-lote STREAM-io.

form skip(1)
     tt_es_cons_lotes.dt_lote_dest   at 1 LABEL "Data do Lote"       space(2)
     tt_es_cons_lotes.dt_lancto_orig at 1 LABEL "Data do Lan‡amento" space(2)
     tt_msg_erro_es_lote.des_msg_erro  AT 01  FORMAT '(200)'  LABEL "Resultado"          space(2)
     skip(1)
     with no-box width 132 side-LABELs attr-space frame f-detalhe-lote STREAM-io.

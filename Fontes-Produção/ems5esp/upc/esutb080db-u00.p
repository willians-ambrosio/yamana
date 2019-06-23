/**************************************************************************
 ** Programa: upc/esutb080db-u00.p (add_cta_ctbl)
 ** Objetivo: Controle cria‡Æo de contas para integra‡Æo JDE
 ** Autor   : Bruno Bertulli - DSC
 ** Data    : 07/07/2014
 ** Cliente : Yamana
 **************************************************************************/
 
{tools/fc-handle-obj.i}
{tools/fc-falso.i}

DEF NEW GLOBAL SHARED var v_cod_usuar_corren as CHARACTER format "x(12)":U label "Usuÿrio Corrente" column-label "Usuÿrio Corrente" no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

define input parameter p_ind_event    as character     no-undo.
define input parameter p_ind_object   as character     no-undo.
define input parameter p_wgh_object   as widget-handle no-undo.
define input parameter p_wgh_frame    as widget-handle no-undo.
define input parameter p_cod_table    as character     no-undo.
define input parameter p_rec_table    as recid         no-undo.

def new global shared var wh-utb080db-bt_ok                         as widget-handle no-undo.
def new global shared var wh-utb080db-bt_ok-f                       as widget-handle no-undo.
def new global shared var wh-utb080db-bt_sav                        as widget-handle no-undo.
def new global shared var wh-utb080db-bt_sav-f                      as widget-handle no-undo.
def new global shared var wh-utb080db-cod_cta_ctbl                  as widget-handle no-undo.
def new global shared var wh-utb080db-log_cta_ctbl_exclus_analit    as widget-handle no-undo. 

DEF VAR c-handle-obj AS CHAR NO-UNDO.
DEF VAR cConta       AS CHAR NO-UNDO.
def var l-resposta   as log  no-undo.

DEFINE BUFFER bf-criter_distrib_cta_ctbl FOR criter_distrib_cta_ctbl.
DEFINE BUFFER bf-cta_ctbl FOR cta_ctbl.

/* ===> Main Block <=== */

/* MESSAGE 'p_ind_event  ' p_ind_event   skip */
/*         'p_ind_object ' p_ind_object  skip */
/*         'p_wgh_object ' p_wgh_object  skip */
/*         'p_wgh_frame  ' p_wgh_frame   skip */
/*         'p_cod_table  ' p_cod_table   skip */
/*         'p_rec_table  ' p_rec_table   skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
  
IF p_ind_event = "INITIALIZE" then do:
    c-handle-obj               = fc-handle-obj("cod_cta_ctbl,bt_ok,bt_sav,log_cta_ctbl_exclus_analit",p_wgh_frame).
    wh-utb080db-cod_cta_ctbl   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    wh-utb080db-bt_ok          = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.
    wh-utb080db-bt_sav         = WIDGET-HANDLE(ENTRY(3,c-handle-obj)) NO-ERROR.
    wh-utb080db-log_cta_ctbl_exclus_analit = WIDGET-HANDLE(ENTRY(4,c-handle-obj)) NO-ERROR.

    wh-utb080db-bt_ok-f        = fc-falso(wh-utb080db-bt_ok, wh-utb080db-bt_ok:FRAME, "").
    wh-utb080db-bt_ok-f:LOAD-IMAGE-UP(wh-utb080db-bt_ok:IMAGE-UP).
    wh-utb080db-bt_ok-f:MOVE-TO-TOP().
    wh-utb080db-bt_ok:VISIBLE  = NO.
    wh-utb080db-bt_sav-f       = fc-falso(wh-utb080db-bt_sav, wh-utb080db-bt_sav:FRAME, "").
    wh-utb080db-bt_sav-f:LOAD-IMAGE-UP(wh-utb080db-bt_sav:IMAGE-UP).
    wh-utb080db-bt_sav-f:MOVE-TO-TOP().
    wh-utb080db-bt_sav:VISIBLE = NO.
  
    ON 'CHOOSE':U OF wh-utb080db-bt_ok-f PERSISTENT RUN upc\esutb080db-u00.p
       ("choose",
        "bt_ok-f",
        p_wgh_object,
        p_wgh_frame,
        p_cod_table,
        p_rec_table).

    ON 'CHOOSE':U OF wh-utb080db-bt_sav-f PERSISTENT RUN upc\esutb080db-u00.p
       ("choose",
        "bt_sav-f",
        p_wgh_object,
        p_wgh_frame,
        p_cod_table,
        p_rec_table).

END.

IF  p_ind_event  = "choose" AND
   (p_ind_object = "bt_ok-f" OR p_ind_object = "bt_sav-f") THEN DO:

    ASSIGN cConta = REPLACE (wh-utb080db-cod_cta_ctbl:SCREEN-VALUE,".","").

    IF SUBSTRING(cConta,7,2) <> '00' AND 
       wh-utb080db-log_cta_ctbl_exclus_analit:SCREEN-VALUE = "NÆo" THEN DO:

        run utp/ut-msgs.p (input "show",
                           input 27100,
                           input "Conta Anal¡tica Exclusiva?~~A Conta informada nÆo termina com 00 e nÆo foi marcado como Anal¡tica Exclusiva. Deseja continuar?").
        IF LOGICAL (RETURN-VALUE) = YES THEN DO:
            IF p_ind_object = "bt_ok-f" THEN DO:
                APPLY "choose" TO wh-utb080db-bt_ok.
                RETURN "OK":U.
            END.
            ELSE DO:
                APPLY "choose" TO wh-utb080db-bt_sav.
                RETURN "OK":U.
            END.
        END.
    END.
    ELSE DO:
        IF p_ind_object = "bt_ok-f" THEN DO:
            APPLY "choose" TO wh-utb080db-bt_ok.
            RETURN "OK":U.
        END.
        ELSE DO:
            APPLY "choose" TO wh-utb080db-bt_sav.
            RETURN "OK":U.
        END.
    END.
END.

IF  p_ind_event  = 'VALIDATE' AND
    p_ind_object = 'viewer' THEN DO:
    assign l-resposta = ?.

    FIND FIRST bf-cta_ctbl NO-LOCK
        WHERE RECID(bf-cta_ctbl) = p_rec_table NO-ERROR.
    IF AVAILABLE bf-cta_ctbl THEN DO:
        FOR EACH es-cross-reference-jde EXCLUSIVE-LOCK
            WHERE es-cross-reference-jde.cod_cta_ctbl = bf-cta_ctbl.cod_cta_ctbl:
            ASSIGN es-cross-reference-jde.log-erro = 0.
        END.

        IF bf-cta_ctbl.cod_plano_cta   = "CONTSOC"  AND
           bf-cta_ctbl.cod_cta_ctbl   >= '30000000' AND
           bf-cta_ctbl.cod_cta_ctbl   <= '79999999' AND 
           bf-cta_ctbl.dat_inic_valid <= TODAY      AND
           bf-cta_ctbl.dat_fim_valid  >= TODAY      AND
           bf-cta_ctbl.log_cta_ctbl_exclus_analit THEN DO:

            /* Despresar faixa de contas que nÆo utiliza CCUSTO */
            IF  TODAY >= 11/01/2014 AND
                bf-cta_ctbl.cod_cta_ctbl >= '54100000' AND
                bf-cta_ctbl.cod_cta_ctbl <= '54299999' THEN NEXT.

            RUN esp/esjd002a.p (INPUT bf-cta_ctbl.cod_cta_ctbl,
                                INPUT bf-cta_ctbl.cod_cta_ctbl).

            FOR EACH estabelecimento NO-LOCK
                WHERE estabelecimento.cod_empresa <> "CAN":
                FIND LAST criter_distrib_cta_ctbl EXCLUSIVE-LOCK
                     where criter_distrib_cta_ctbl.cod_empresa        = estabelecimento.cod_empresa
                       AND criter_distrib_cta_ctbl.cod_plano_cta_ctbl = bf-cta_ctbl.cod_plano_cta_ctbl
                       and criter_distrib_cta_ctbl.cod_cta_ctbl       = bf-cta_ctbl.cod_cta_ctbl
                       and criter_distrib_cta_ctbl.cod_estab          = estabelecimento.cod_estab NO-ERROR.
                IF NOT AVAILABLE criter_distrib_cta_ctbl THEN DO:
                    IF  l-resposta = ? THEN DO:
                        run utp/ut-msgs.p(input "show":U, input 27100,
                                          input 'Cria‡Æo de Crit‚rio~~Ser  criado crit‚rio como definidos. Deseja passar para "NÆo Utiliza"?').
                        assign l-resposta = logical(return-value).
                    END.

                    RUN pi-cria-criterio (INPUT DATE('01/' + STRING(MONTH(TODAY)) + '/' + STRING(YEAR(TODAY))),
                                          INPUT DATE("31/12/9999"),
                                          INPUT 10,
                                          INPUT bf-cta_ctbl.cod_cta_ctbl).
                END.
            END.
        END.
    END.
END.

/* ===> Procedures <=== */

PROCEDURE pi-cria-criterio:
    DEFINE INPUT PARAMETER p-dat_inic_valid LIKE criter_distrib_cta_ctbl.dat_inic_valid NO-UNDO.
    DEFINE INPUT PARAMETER p-dat_fim_valid  LIKE criter_distrib_cta_ctbl.dat_fim_valid  NO-UNDO.
    DEFINE INPUT PARAMETER p-seq            AS   INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-mapa           LIKE criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF bf-criter_distrib_cta_ctbl.
    DISABLE TRIGGERS FOR DUMP OF bf-criter_distrib_cta_ctbl.

    CREATE bf-criter_distrib_cta_ctbl.
    ASSIGN bf-criter_distrib_cta_ctbl.cod_empresa                 = estabelecimento.cod_empresa
           bf-criter_distrib_cta_ctbl.cod_estab                   = estabelecimento.cod_estab
           bf-criter_distrib_cta_ctbl.cod_plano_cta_ctbl          = bf-cta_ctbl.cod_plano_cta_ctbl
           bf-criter_distrib_cta_ctbl.cod_cta_ctbl                = bf-cta_ctbl.cod_cta_ctbl
           bf-criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl = p-seq
           bf-criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto     =(if  l-resposta then "" else p-mapa)
           bf-criter_distrib_cta_ctbl.ind_criter_distrib_ccusto   =(if  l-resposta then "NÆo Utiliza" else "Definidos")
           bf-criter_distrib_cta_ctbl.dat_inic_valid              = p-dat_inic_valid
           bf-criter_distrib_cta_ctbl.dat_fim_valid               = p-dat_fim_valid
           .

END PROCEDURE.

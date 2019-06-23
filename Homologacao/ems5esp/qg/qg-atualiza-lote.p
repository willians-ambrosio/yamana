{utp/ut-glob.i}
{utp/utapi019.i}
{include/i-prgvrs.i apb001-w01 1.00.00.000}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE c-mess-top         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess-base        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mot-reprov       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-confirma-titulos AS LOGICAL     NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario          AS CHARACTER                          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_lote_impl_tit_ap AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-prg-aprov-movto      AS LOGICAL NO-UNDO. 

def new shared temp-table tt_log_erros_atualiz no-undo
    field tta_cod_estab                         as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                         as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_num_seq_refer                     as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field ttv_num_mensagem                      as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero Mensagem"
    field ttv_des_msg_erro                      as character format "x(60)" label "Mensagem Erro" column-label "Inconsistˆncia"
    field ttv_des_msg_ajuda                     as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_ind_tip_relacto                   as character format "X(15)" label "Tipo Relacionamento" column-label "Tipo Relac"
    field ttv_num_relacto                       as integer format ">>>>,>>9" label "Relacionamento" column-label "Relacionamento".

DEFINE TEMP-TABLE tt_esp_pend_lote_ap           LIKE esp_pend_lote_ap
    FIELD cdn_fornecedor                        LIKE item_lote_impl_ap.cdn_fornecedor   
    FIELD cod_espec_docto                       LIKE item_lote_impl_ap.cod_espec_docto  
    FIELD cod_ser_docto                         LIKE item_lote_impl_ap.cod_ser_docto    
    FIELD cod_tit_ap                            LIKE item_lote_impl_ap.cod_tit_ap       
    FIELD cod_parcela                           LIKE item_lote_impl_ap.cod_parcela      
    FIELD dat_emis_docto                        LIKE item_lote_impl_ap.dat_emis_docto   
    FIELD dat_vencto_tit_ap                     LIKE item_lote_impl_ap.dat_vencto_tit_ap
    FIELD val_tit_ap                            LIKE item_lote_impl_ap.val_tit_ap
    FIELD des_text_histor                       LIKE item_lote_impl_ap.des_text_histor.

DEFINE TEMP-TABLE tt_esp_pend_movto_cta_corren  LIKE esp_pend_movto_cta_corren
    FIELD val_movto_cta_corren                  LIKE movto_cta_corren.val_movto_cta_corren
    FIELD dat_transacao                         LIKE movto_cta_corren.dat_transacao
    FIELD des_histor_movto_cta_corren           LIKE movto_cta_corren.des_histor_movto_cta_corren
    FIELD cod_usuar_ult_atualiz                 LIKE movto_cta_corren.cod_usuar_ult_atualiz
    FIELD ind_fluxo_movto_cta_corren            LIKE movto_cta_corren.ind_fluxo_movto_cta_corren.

DEFINE TEMP-TABLE tt-tit
    FIELD cod_estab                             LIKE esp_pend_lote_ap.cod_estab
    FIELD cod_refer                             LIKE esp_pend_lote_ap.cod_refer
    FIELD dt_aprovacao1                         LIKE esp_pend_lote.dt_aprovacao1
    FIELD dt_aprovacao2                         LIKE esp_pend_lote.dt_aprovacao2
    FIELD cod_usuario                           LIKE esp_pend_lote.cod_usuario.

DEFINE TEMP-TABLE tt_tit_ap_par
    FIELD cod_estab         LIKE tit_ap.cod_estab                
    FIELD cdn_fornecedor    LIKE tit_ap.cdn_fornecedor
    FIELD cod_espec_docto   LIKE tit_ap.cod_espec_docto          
    FIELD cod_ser_docto     LIKE tit_ap.cod_ser_docto            
    FIELD cod_tit_ap        LIKE tit_ap.cod_tit_ap               
    FIELD val_origin_tit_ap LIKE tit_ap.val_origin_tit_ap
    FIELD dat_emis_docto    LIKE tit_ap.dat_emis_docto.

DEFINE TEMP-TABLE tt_esp_hist_aprov_tit         LIKE esp_hist_aprov_tit.

DEFINE BUFFER   b_esp_aprovador FOR esp_aprovador.
DEFINE BUFFER   b-item_lote_impl_ap FOR item_lote_impl_ap.


    def new global shared var v_ind_tip_usuar_cor
    as character
    format "X(13)":U
    view-as radio-set Horizontal
    radio-buttons "Super", "Super","Admin", "Admin","Supervisor", "Supervisor","Comum", "Comum"
     /*l_super*/ /*l_super*/ /*l_administrador*/ /*l_administrador*/ /*l_supervisor*/ /*l_supervisor*/ /*l_comum*/ /*l_comum*/
    bgcolor 8 
    label "Tipo Usu rio"
    column-label "Tipo Usu rio"
    no-undo.
def new global shared var v_cod_produt_corren
    as character
    format 'x(50)'
    no-undo.
def new global shared var v2_cod_usuar_corren_criptog
    as character
    format "x(16)"
    no-undo.
def new global shared var v2_cod_usuar_corren
        as character
        format "x(12)"
        label "Usu rio Corrente"
    column-label "Usu rio Corrente"
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)":U
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg¢cio"
    column-label "Unid Neg¢cio"
    no-undo.
def new global shared var v_cod_empresa
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.

/* ========================================================================= */

FOR EACH esp_pend_lote_ap NO-LOCK
   WHERE esp_pend_lote_ap.dt_reprovacao = ?
     AND (esp_pend_lote_ap.dt_aprovacao1 = ?
      OR esp_pend_lote_ap.dt_aprovacao2 = ?):

    ASSIGN c-seg-usuario = esp_pend_lote_ap.cod_usuario .
    

    FIND FIRST usuar_mestre WHERE 
               usuar_mestre.cod_usuario =  c-seg-usuario NO-LOCK NO-ERROR.
    FIND FIRST usuar_univ WHERE
               usuar_univ.cod_usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.

    assign v_cod_usuar_corren          = c-seg-usuario
           v_cod_usuar_corren_criptog  = encode(v_cod_usuar_corren)
           v_ind_tip_usuar_cor         = usuar_mestre.ind_tip_usuar
           v_cod_produt_corren         = 'EMS5'
           l-achou-prog                = YES
           v2_cod_usuar_corren_criptog = v_cod_usuar_corren_criptog
           c-seg-usuario               = v_cod_usuar_corren 
           v2_cod_usuar_corren         = v_cod_usuar_corren
    /* ----------------------------------------------------- */
           v_cod_ccusto_corren         = usuar_univ.cod_ccusto 
           v_cod_unid_negoc_usuar      = usuar_univ.cod_unid_negoc
           v_cod_empresa               = usuar_univ.cod_empresa 
           v_cod_estab_usuar           = usuar_univ.cod_estab
           v_cod_plano_ccusto_corren   = usuar_univ.cod_plano_ccusto.

    RUN pi-processar.
END.

/* ========================================================================= */
PROCEDURE pi-processar:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-assunto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-aprov-atu AS DATETIME  NO-UNDO.

FIND FIRST lote_impl_tit_ap NO-LOCK 
     WHERE lote_impl_tit_ap.cod_estab = esp_pend_lote_ap.cod_estab
       AND lote_impl_tit_ap.cod_refer = esp_pend_lote_ap.cod_refer NO-ERROR.
IF AVAIL lote_impl_tit_ap THEN
    ASSIGN v_rec_lote_impl_tit_ap = RECID(lote_impl_tit_ap).
ELSE
DO:
    ASSIGN v_rec_lote_impl_tit_ap = ?.
    RETURN NO-APPLY.
END.

EMPTY TEMP-TABLE tt_esp_hist_aprov_tit.
c-mess = "".



FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.
CREATE tt_esp_hist_aprov_tit.
ASSIGN
    tt_esp_hist_aprov_tit.cod_estab          = esp_pend_lote_ap.cod_estab    
    tt_esp_hist_aprov_tit.cod_refer          = esp_pend_lote_ap.cod_refer    
    tt_esp_hist_aprov_tit.num_seq_refer      = esp_pend_lote_ap.num_seq_refer
    tt_esp_hist_aprov_tit.dt_aprovacao1      = esp_pend_lote_ap.dt_aprovacao1     
    tt_esp_hist_aprov_tit.cod_usuario_aprov1 = c-seg-usuario
    tt_esp_hist_aprov_tit.dt_aprovacao2      = NOW          
    tt_esp_hist_aprov_tit.cod_usuario_aprov2 = c-seg-usuario
    tt_esp_hist_aprov_tit.cod_usuario_reprov = esp_pend_lote_ap.cod_usuario_reprov
    tt_esp_hist_aprov_tit.dt_reprovacao      = esp_pend_lote_ap.dt_reprovacao     
    tt_esp_hist_aprov_tit.mot-reprov         = esp_pend_lote_ap.mot-reprov        
    tt_esp_hist_aprov_tit.cod_usuario        = esp_pend_lote_ap.cod_usuario       
    tt_esp_hist_aprov_tit.dt_digitacao       = esp_pend_lote_ap.dt_digitacao.

FIND ems5.fornecedor OF item_lote_impl_ap NO-LOCK NO-ERROR.
IF AVAIL item_lote_impl_ap THEN
ASSIGN
    tt_esp_hist_aprov_tit.cdn_fornecedor    = item_lote_impl_ap.cdn_fornecedor   
    tt_esp_hist_aprov_tit.cod_espec_docto   = item_lote_impl_ap.cod_espec_docto  
    tt_esp_hist_aprov_tit.cod_ser_docto     = item_lote_impl_ap.cod_ser_docto    
    tt_esp_hist_aprov_tit.cod_tit_ap        = item_lote_impl_ap.cod_tit_ap       
    tt_esp_hist_aprov_tit.cod_parcela       = item_lote_impl_ap.cod_parcela      
    tt_esp_hist_aprov_tit.dat_emis_docto    = item_lote_impl_ap.dat_emis_docto   
    tt_esp_hist_aprov_tit.dat_vencto_tit_ap = item_lote_impl_ap.dat_vencto_tit_ap
    tt_esp_hist_aprov_tit.val_tit_ap        = item_lote_impl_ap.val_tit_ap       
    tt_esp_hist_aprov_tit.des_text_histor   = item_lote_impl_ap.des_text_histor. 



OUTPUT TO "c:\temp\lotes-atualizados-capa-20022019.txt" APPEND.
PUT UNFORMATTED tt_esp_hist_aprov_tit.cod_estab             skip
                tt_esp_hist_aprov_tit.cod_refer             skip
                tt_esp_hist_aprov_tit.num_seq_refer         skip
                tt_esp_hist_aprov_tit.dt_aprovacao1         skip
                tt_esp_hist_aprov_tit.cod_usuario_aprov1    skip
                tt_esp_hist_aprov_tit.dt_aprovacao2         skip
                tt_esp_hist_aprov_tit.cod_usuario_aprov2    skip
                tt_esp_hist_aprov_tit.cod_usuario_reprov    skip
                tt_esp_hist_aprov_tit.dt_reprovacao         skip
                tt_esp_hist_aprov_tit.mot-reprov            skip
                tt_esp_hist_aprov_tit.cod_usuario           skip
                tt_esp_hist_aprov_tit.dt_digitacao          SKIP.
OUTPUT CLOSE.

IF AVAIL tt_esp_hist_aprov_tit THEN
DO:
    OUTPUT TO "c:\temp\lotes-atualizados-item-20022019.txt" APPEND.
    PUT UNFORMATTED tt_esp_hist_aprov_tit.cdn_fornecedor       skip
                    tt_esp_hist_aprov_tit.cod_espec_docto      skip
                    tt_esp_hist_aprov_tit.cod_ser_docto        skip
                    tt_esp_hist_aprov_tit.cod_tit_ap           skip
                    tt_esp_hist_aprov_tit.cod_parcela          skip
                    tt_esp_hist_aprov_tit.dat_emis_docto       skip
                    tt_esp_hist_aprov_tit.dat_vencto_tit_ap    skip
                    tt_esp_hist_aprov_tit.val_tit_ap           skip
                    tt_esp_hist_aprov_tit.des_text_histor      skip.
    OUTPUT CLOSE.
END.



c-mess = c-mess + "<tr>"
    + "<td>" + item_lote_impl_ap.cod_empresa +                                           "</td>"
    + "<td>" + item_lote_impl_ap.cod_estab +                                             "</td>"
    + "<td>" + STRING(item_lote_impl_ap.cdn_fornecedor) + " - " + ems5.fornecedor.nom_abrev + "</td>"
    + "<td>" + item_lote_impl_ap.cod_tit_ap +                                            "</td>"
    + "<td>" + item_lote_impl_ap.cod_ser_docto +                                         "</td>"
    + "<td>" + item_lote_impl_ap.cod_espec_docto +                                       "</td>"
    + "<td>" + item_lote_impl_ap.cod_parcela +                                           "</td>"
    + "<td>" + STRING(item_lote_impl_ap.val_tit_ap, "->>>>,>>>,>>9.99") +                "</td>"
    + "<td>" + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999") +                  "</td>"
    + "<td>" + STRING(item_lote_impl_ap.dat_vencto_tit_ap, "99/99/9999") +               "</td>"
    + "<td>" + item_lote_impl_ap.des_text_histor +                                       "</td>"
    + "<td>" + esp_pend_lote_ap.cod_usuario +                                            "</td>"
    + "<td>" + STRING(esp_pend_lote_ap.dt_digitacao, "99/99/9999 HH:MM") +                     "</td>"
    + "</tr>".

l-prg-aprov-movto = YES.
RUN prgfin/apb/apb739za.py (Input "On-Line").
l-prg-aprov-movto = NO.

IF RETURN-VALUE = "OK" THEN DO:
    FOR EACH tt_esp_hist_aprov_tit:
        FIND esp_hist_aprov_tit OF tt_esp_hist_aprov_tit NO-LOCK NO-ERROR.
        IF NOT AVAIL esp_hist_aprov_tit THEN
        DO:
            CREATE esp_hist_aprov_tit.
            BUFFER-COPY tt_esp_hist_aprov_tit TO esp_hist_aprov_tit NO-ERROR.
        END.

        FOR EACH esp_pend_lote_ap
           WHERE esp_pend_lote_ap.cod_estab = tt_esp_hist_aprov_tit.cod_estab
             AND esp_pend_lote_ap.cod_refer = tt_esp_hist_aprov_tit.cod_refer EXCLUSIVE-LOCK:
    
            DELETE esp_pend_lote_ap.
        END.
        ASSIGN c-assunto = "Aprova‡Æo de T¡tulo no Contas a Pagar".
    
        /*manda e-mail para o usuario que cadastrou o titulo*/
        RUN pi-envia-email (INPUT TRIM(tt_esp_hist_aprov_tit.cod_usuario) + "@yamana.com",
                            INPUT c-assunto,
                            INPUT c-mess-top + c-mess + c-mess-base,
                            INPUT "APB").
    END.
END.



FOR EACH tt_log_erros_atualiz:
    MESSAGE tta_cod_estab         skip
            tta_cod_refer         skip
            tta_num_seq_refer     skip
            ttv_num_mensagem      skip
            ttv_des_msg_erro      skip
            ttv_des_msg_ajuda     skip
            ttv_ind_tip_relacto   skip
            ttv_num_relacto       skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

END PROCEDURE.

/* PROCEDURE pi-aprova-tit:                                                                                                                                                      */
/* /*------------------------------------------------------------------------------                                                                                              */
/*   Purpose:                                                                                                                                                                    */
/*   Parameters:  <none>                                                                                                                                                         */
/*   Notes:                                                                                                                                                                      */
/* ------------------------------------------------------------------------------*/                                                                                              */
/* DEFINE VARIABLE c-assunto AS CHARACTER   NO-UNDO.                                                                                                                             */
/* DEFINE VARIABLE dt-aprov-atu AS DATETIME  NO-UNDO.                                                                                                                            */
/* FOR EACH tt-tit:                                                                                                                                                              */
/*                                                                                                                                                                               */
/*     dt-aprov-atu = tt-tit.dt_aprovacao1.                                                                                                                                      */
/*                                                                                                                                                                               */
/*     IF tt-tit.dt_aprovacao1 = ? THEN                                                                                                                                          */
/*     DO:                                                                                                                                                                       */
/*         /*primeira aprova‡Æo*/                                                                                                                                                */
/*         {prgfin\apb\apb001-i03.i}                                                                                                                                             */
/*         IF esp_aprovador.nivel_aprovador <> 1 THEN                                                                                                                            */
/*         DO:                                                                                                                                                                   */
/*             RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Aprova‡Æo~~Somente aprovador de N¡vel 1 pode fazer a primeira a prova‡Æo do(s) t¡tulo(s).").                  */
/*             RETURN NO-APPLY.                                                                                                                                                  */
/*         END.                                                                                                                                                                  */
/*                                                                                                                                                                               */
/*         FOR EACH esp_pend_lote_ap                                                                                                                                             */
/*            WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab                                                                                                                */
/*              AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer EXCLUSIVE-LOCK:                                                                                                */
/*                                                                                                                                                                               */
/*             ASSIGN esp_pend_lote_ap.dt_aprovacao1 = NOW                                                                                                                       */
/*                    c-seg-usuario = c-seg-usuario.                                                                                                       */
/*                                                                                                                                                                               */
/*             FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.                                                                                                      */
/*             FIND ems5.fornecedor OF item_lote_impl_ap NO-LOCK NO-ERROR.                                                                                                       */
/*                                                                                                                                                                               */
/*             c-mess = c-mess + "<tr>"                                                                                                                                          */
/*                 + "<td>" + item_lote_impl_ap.cod_empresa +                                           "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_estab +                                             "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.cdn_fornecedor) + " - " + ems5.fornecedor.nom_abrev + "</td>"                                                             */
/*                 + "<td>" + item_lote_impl_ap.cod_tit_ap +                                            "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_ser_docto +                                         "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_espec_docto +                                       "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_parcela +                                           "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.val_tit_ap, "->>>>,>>>,>>9.99") +                "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999") +                  "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.dat_vencto_tit_ap, "99/99/9999") +               "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.des_text_histor +                                       "</td>"                                                                  */
/*                 + "<td>" + esp_pend_lote_ap.cod_usuario +                                            "</td>"                                                                  */
/*                 + "<td>" + STRING(esp_pend_lote_ap.dt_digitacao, "99/99/9999 HH:MM") +                     "</td>"                                                            */
/*                 + "</tr>".                                                                                                                                                    */
/*         END.                                                                                                                                                                  */
/*         ASSIGN c-assunto = "Pendˆncia de T¡tulos para Aprova‡Æo".                                                                                                             */
/*         FOR EACH b_esp_aprovador NO-LOCK                                                                                                                                      */
/*            WHERE b_esp_aprovador.aprov_contas_pagar                                                                                                                           */
/*              AND b_esp_aprovador.nivel_aprovador = 2:                                                                                                                         */
/*                                                                                                                                                                               */
/*             /*manda e-mail para aprovador de nivel 2*/                                                                                                                        */
/*             RUN pi-envia-email (INPUT TRIM(b_esp_aprovador.email),                                                                                                            */
/*                                 INPUT c-assunto,                                                                                                                              */
/*                                 INPUT c-mess-top + c-mess + c-mess-base,                                                                                                      */
/*                                 INPUT "APB").                                                                                                                                 */
/*         END.                                                                                                                                                                  */
/*     END.                                                                                                                                                                      */
/*     IF tt-tit.dt_aprovacao2 = ? AND dt-aprov-atu <> ? THEN                                                                                                                    */
/*     DO:                                                                                                                                                                       */
/*         /*segunda aprova‡Æo*/                                                                                                                                                 */
/*         {prgfin\apb\apb001-i05.i}                                                                                                                                             */
/*         IF esp_aprovador.nivel_aprovador <> 2 THEN                                                                                                                            */
/*         DO:                                                                                                                                                                   */
/*             RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Atualiza‡Æo~~Somente aprovador de N¡vel 2 pode fazer a segunda a prova‡Æo e a atualiza‡Æo do(s) t¡tulo(s)."). */
/*             RETURN NO-APPLY.                                                                                                                                                  */
/*         END.                                                                                                                                                                  */
/*                                                                                                                                                                               */
/*         FIND FIRST lote_impl_tit_ap NO-LOCK                                                                                                                                   */
/*              WHERE lote_impl_tit_ap.cod_estab = tt-tit.cod_estab                                                                                                              */
/*                AND lote_impl_tit_ap.cod_refer = tt-tit.cod_refer NO-ERROR.                                                                                                    */
/*         IF AVAIL lote_impl_tit_ap THEN                                                                                                                                        */
/*             ASSIGN v_rec_lote_impl_tit_ap = RECID(lote_impl_tit_ap).                                                                                                          */
/*         ELSE                                                                                                                                                                  */
/*         DO:                                                                                                                                                                   */
/*             ASSIGN v_rec_lote_impl_tit_ap = ?.                                                                                                                                */
/*             RETURN NO-APPLY.                                                                                                                                                  */
/*         END.                                                                                                                                                                  */
/*                                                                                                                                                                               */
/*         EMPTY TEMP-TABLE tt_esp_hist_aprov_tit.                                                                                                                               */
/*         c-mess = "".                                                                                                                                                          */
/*         FOR EACH esp_pend_lote_ap                                                                                                                                             */
/*            WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab                                                                                                                */
/*              AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer NO-LOCK:                                                                                                       */
/*                                                                                                                                                                               */
/*             FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.                                                                                                      */
/*             CREATE tt_esp_hist_aprov_tit.                                                                                                                                     */
/*             ASSIGN                                                                                                                                                            */
/*                 tt_esp_hist_aprov_tit.cod_estab          = esp_pend_lote_ap.cod_estab                                                                                         */
/*                 tt_esp_hist_aprov_tit.cod_refer          = esp_pend_lote_ap.cod_refer                                                                                         */
/*                 tt_esp_hist_aprov_tit.num_seq_refer      = esp_pend_lote_ap.num_seq_refer                                                                                     */
/*                 tt_esp_hist_aprov_tit.dt_aprovacao1      = esp_pend_lote_ap.dt_aprovacao1                                                                                     */
/*                 tt_esp_hist_aprov_tit.cod_usuario_aprov1 = c-seg-usuario                                                                                */
/*                 tt_esp_hist_aprov_tit.dt_aprovacao2      = NOW                                                                                                                */
/*                 tt_esp_hist_aprov_tit.cod_usuario_aprov2 = c-seg-usuario                                                                                                      */
/*                 tt_esp_hist_aprov_tit.cod_usuario_reprov = esp_pend_lote_ap.cod_usuario_reprov                                                                                */
/*                 tt_esp_hist_aprov_tit.dt_reprovacao      = esp_pend_lote_ap.dt_reprovacao                                                                                     */
/*                 tt_esp_hist_aprov_tit.mot-reprov         = esp_pend_lote_ap.mot-reprov                                                                                        */
/*                 tt_esp_hist_aprov_tit.cod_usuario        = esp_pend_lote_ap.cod_usuario                                                                                       */
/*                 tt_esp_hist_aprov_tit.dt_digitacao       = esp_pend_lote_ap.dt_digitacao.                                                                                     */
/*                                                                                                                                                                               */
/*             FIND ems5.fornecedor OF item_lote_impl_ap NO-LOCK NO-ERROR.                                                                                                       */
/*             IF AVAIL item_lote_impl_ap THEN                                                                                                                                   */
/*             ASSIGN                                                                                                                                                            */
/*                 tt_esp_hist_aprov_tit.cdn_fornecedor    = item_lote_impl_ap.cdn_fornecedor                                                                                    */
/*                 tt_esp_hist_aprov_tit.cod_espec_docto   = item_lote_impl_ap.cod_espec_docto                                                                                   */
/*                 tt_esp_hist_aprov_tit.cod_ser_docto     = item_lote_impl_ap.cod_ser_docto                                                                                     */
/*                 tt_esp_hist_aprov_tit.cod_tit_ap        = item_lote_impl_ap.cod_tit_ap                                                                                        */
/*                 tt_esp_hist_aprov_tit.cod_parcela       = item_lote_impl_ap.cod_parcela                                                                                       */
/*                 tt_esp_hist_aprov_tit.dat_emis_docto    = item_lote_impl_ap.dat_emis_docto                                                                                    */
/*                 tt_esp_hist_aprov_tit.dat_vencto_tit_ap = item_lote_impl_ap.dat_vencto_tit_ap                                                                                 */
/*                 tt_esp_hist_aprov_tit.val_tit_ap        = item_lote_impl_ap.val_tit_ap                                                                                        */
/*                 tt_esp_hist_aprov_tit.des_text_histor   = item_lote_impl_ap.des_text_histor.                                                                                  */
/*                                                                                                                                                                               */
/*             c-mess = c-mess + "<tr>"                                                                                                                                          */
/*                 + "<td>" + item_lote_impl_ap.cod_empresa +                                           "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_estab +                                             "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.cdn_fornecedor) + " - " + ems5.fornecedor.nom_abrev + "</td>"                                                             */
/*                 + "<td>" + item_lote_impl_ap.cod_tit_ap +                                            "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_ser_docto +                                         "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_espec_docto +                                       "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.cod_parcela +                                           "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.val_tit_ap, "->>>>,>>>,>>9.99") +                "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999") +                  "</td>"                                                                  */
/*                 + "<td>" + STRING(item_lote_impl_ap.dat_vencto_tit_ap, "99/99/9999") +               "</td>"                                                                  */
/*                 + "<td>" + item_lote_impl_ap.des_text_histor +                                       "</td>"                                                                  */
/*                 + "<td>" + esp_pend_lote_ap.cod_usuario +                                            "</td>"                                                                  */
/*                 + "<td>" + STRING(esp_pend_lote_ap.dt_digitacao, "99/99/9999 HH:MM") +                     "</td>"                                                            */
/*                 + "</tr>".                                                                                                                                                    */
/*         END.                                                                                                                                                                  */
/*                                                                                                                                                                               */
/*         l-prg-aprov-movto = YES.                                                                                                                                              */
/*         RUN prgfin/apb/apb739za.py (Input "On-Line").                                                                                                                         */
/*         l-prg-aprov-movto = NO.                                                                                                                                               */
/*                                                                                                                                                                               */
/*         IF RETURN-VALUE = "OK" THEN DO:                                                                                                                                       */
/*             FOR EACH tt_esp_hist_aprov_tit:                                                                                                                                   */
/*                 FIND esp_hist_aprov_tit OF tt_esp_hist_aprov_tit NO-LOCK NO-ERROR.                                                                                            */
/*                 IF NOT AVAIL esp_hist_aprov_tit THEN                                                                                                                          */
/*                 DO:                                                                                                                                                           */
/*                     CREATE esp_hist_aprov_tit.                                                                                                                                */
/*                     BUFFER-COPY tt_esp_hist_aprov_tit TO esp_hist_aprov_tit NO-ERROR.                                                                                         */
/*                 END.                                                                                                                                                          */
/*             END.                                                                                                                                                              */
/*             FOR EACH esp_pend_lote_ap                                                                                                                                         */
/*                WHERE esp_pend_lote_ap.cod_estab = tt-tit.cod_estab                                                                                                            */
/*                  AND esp_pend_lote_ap.cod_refer = tt-tit.cod_refer EXCLUSIVE-LOCK:                                                                                            */
/*                                                                                                                                                                               */
/*                 DELETE esp_pend_lote_ap.                                                                                                                                      */
/*             END.                                                                                                                                                              */
/*             ASSIGN c-assunto = "Aprova‡Æo de T¡tulo no Contas a Pagar".                                                                                                       */
/*                                                                                                                                                                               */
/*             /*manda e-mail para o usuario que cadastrou o titulo*/                                                                                                            */
/*             RUN pi-envia-email (INPUT TRIM(tt-tit.cod_usuario) + "@yamana.com",                                                                                               */
/*                                 INPUT c-assunto,                                                                                                                              */
/*                                 INPUT c-mess-top + c-mess + c-mess-base,                                                                                                      */
/*                                 INPUT "APB").                                                                                                                                 */
/*         END.                                                                                                                                                                  */
/*         ELSE                                                                                                                                                                  */
/*         DO:                                                                                                                                                                   */
/*             RUN utp/ut-msgs.p ("show",17006,"Apresentou Erro na Atualiza‡Æo~~Apresentou o erro " + RETURN-VALUE + " na atualiza‡Æo do(s) t¡tulo(s).").                        */
/*         END.                                                                                                                                                                  */
/*     END.                                                                                                                                                                      */
/* END.                                                                                                                                                                          */
/*                                                                                                                                                                               */
/* END PROCEDURE.                                                                                                                                                                */




PROCEDURE pi-envia-email:

    DEFINE INPUT PARAMETER p-dest      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-assunto   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-mess      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER p-modulo    AS CHARACTER NO-UNDO.

    RUN utp/utapi019.p PERSISTENT SET h-utapi019.
    FIND FIRST param_email NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.
    EMPTY TEMP-TABLE tt-erros.

    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = param_email.log_servid_exchange
           tt-envio2.servidor          = param_email.cod_servid_e_mail
           tt-envio2.porta             = param_email.num_porta
           tt-envio2.destino           = "v-rpaulucci@yamana.com; daniel.lima@yamana.com; aurelio.alves@yamana.com; v-sbaggio@yamana.com; v-psantos@yamana.com; v-wambrosio@yamana.com"
           tt-envio2.assunto           = p-assunto
           tt-envio2.remetente         = "SustencaoYamana@yamana.com"
           tt-envio2.copia             = ""
           tt-envio2.importancia       = 1
           tt-envio2.log-enviada       = NO
           tt-envio2.log-lida          = NO
           tt-envio2.acomp             = NO
           tt-envio2.formato           = "html".

    CREATE tt-mensagem.                                       
    ASSIGN tt-mensagem.seq-mensagem = 1                         
           tt-mensagem.mensagem     = p-mess. 

    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,
                                   OUTPUT TABLE tt-erros).
    IF TEMP-TABLE tt-erros:HAS-RECORDS THEN
    DO:
        OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erro-email-" + p-modulo + "-" + STRING(TODAY, "99999999") + "-" + STRING(ETIME, "99999999") + ".txt").
        FOR EACH tt-erros:
            DISP tt-erros WITH SCROLLABLE.
        END.
        OUTPUT CLOSE.
    END.

DELETE PROCEDURE h-utapi019.

END PROCEDURE.



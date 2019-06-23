/*****************************************************************************
**       Programa: esbtb008rp.p
**       Data....: 22/02/11
**       Autor...: Daniel P. de Lima
**       Objetivo: Exporta Cadastro de Fornecedor
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "ESBTB008RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.06.00.000").

{cdp/cdcfgdis.i}

/****************** Definição de Tabelas Temporárias do Relatório **********************/

define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var h-FunctionLibrary    as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
def var v-cont-registro      as int    no-undo.
def var v-des-retorno        as char   no-undo.
def var v-des-local-layout   as char   no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.
define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.
define new shared stream str-rp.

assign c-programa     = "ESBTB008RP"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "Exporta Cadastro de Fornecedor"
       c-sistema      = "".


find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

if  tt-param.formato = 1 then do:


form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    fill("-", 60) format "x(58)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabec-80.
        
form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    "Per¡odo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 22) format "x(20)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabper-80.
run grapi/gr2005.p.
form header
    c-rodape format "x(80)"
    with stream-io width 80 no-labels no-box page-bottom frame f-rodape-80.

end. /* tt-param.formato = 1 */ 

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.
form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabper.
run grapi/gr2004.p.
form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.


end. /* tt-param.formato = 2 */


run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
.

def var l-imprime as logical no-undo.

assign l-imprime = no.
if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020a.p */

FIND FIRST es_fornecedor NO-LOCK NO-ERROR.
    IF AVAIL es_fornecedor THEN DO:
        RUN esp/esidelete.i 'es_fornecedor'.
    END.



FOR EACH emitente WHERE identific = 2 
                    OR  identific = 3 
                    USE-INDEX codigo 
                                NO-LOCK:
    CREATE es_fornecedor NO-ERROR.
    ASSIGN  es_fornecedor.agencia                                = emitente.agencia                         
            es_fornecedor.agente_retencao                        = emitente.agente-retencao                 
            es_fornecedor.atividade                              = emitente.atividade                       
            es_fornecedor.bairro                                 = emitente.bairro                          
            es_fornecedor.bairro_cob                             = emitente.bairro-cob                      
            es_fornecedor.bonificacao                            = emitente.bonificacao                     
            es_fornecedor.bx_acatada                             = emitente.bx-acatada                      
            es_fornecedor.caixa_postal                           = emitente.caixa-postal                    
            es_fornecedor.calcula_multa                          = emitente.calcula-multa                   
            es_fornecedor.categoria                              = emitente.categoria                       
            es_fornecedor.cep                                    = emitente.cep                             
            es_fornecedor.cep_cob                                = emitente.cep-cob                         
            es_fornecedor.cgc                                    = emitente.cgc                             
            es_fornecedor.cgc_cob                                = emitente.cgc-cob                         
/*             es_fornecedor.char_1                                 = emitente.char-1 */
/*             es_fornecedor.char_2                                 = emitente.char-2 */
            es_fornecedor.check_sum                              = emitente.check-sum                       
            es_fornecedor.cidade                                 = emitente.cidade                          
            es_fornecedor.cidade_cob                             = emitente.cidade-cob                      
            es_fornecedor.cn_codigo                              = emitente.cn-codigo                       
            es_fornecedor.cod_banco                              = emitente.cod-banco                       
            es_fornecedor.cod_cacex                              = emitente.cod-cacex                       
            es_fornecedor.cod_canal_venda                        = emitente.cod-canal-venda                 
            es_fornecedor.cod_classif_cliente                    = emitente.cod-classif-cliente             
            es_fornecedor.cod_classif_fornec                     = emitente.cod-classif-fornec              
            es_fornecedor.cod_cond_pag                           = emitente.cod-cond-pag                    
            es_fornecedor.cod_emitente                           = emitente.cod-emitente                  
            es_fornecedor.cod_entrega                            = emitente.cod-entrega                     
            es_fornecedor.cod_gr_cli                             = emitente.cod-gr-cli                      
            es_fornecedor.cod_gr_forn                            = emitente.cod-gr-forn                     
            es_fornecedor.cod_isencao                            = emitente.cod-isencao                     
            es_fornecedor.cod_mensagem                           = emitente.cod-mensagem                    
            es_fornecedor.cod_parceiro_edi                       = emitente.cod-parceiro-edi                
            es_fornecedor.cod_rep                                = emitente.cod-rep                         
            es_fornecedor.cod_repres_imp                         = emitente.cod-repres-imp                  
            es_fornecedor.cod_suframa                            = emitente.cod-suframa                     
            es_fornecedor.cod_tax                                = emitente.cod-tax                         
            es_fornecedor.cod_tip_ent                            = emitente.cod-tip-ent                     
            es_fornecedor.cod_transp                             = emitente.cod-transp                      
            es_fornecedor.compr_period                           = emitente.compr-period                    
            es_fornecedor.conta_corren                           = emitente.conta-corren                    
/*             es_fornecedor.contato[1]                             = emitente.contato[1] 
            es_fornecedor.contato[2]                             = emitente.contato[2] 
            es_fornecedor.contrib_icms                           = emitente.contrib-icms                    
            es_fornecedor.cx_post_cob                            = emitente.cx-post-cob                     
            es_fornecedor.data_1                                 = &IF "{&BF_DIS_VERSAO_EMS}" < "2.071" &THEN
                                                                   emitente.data-1
                                                                   &else
                                                                   emitente.dt-atualiza
                                                                   &endif
             es_fornecedor.data_2                                 = emitente.data-2 */
/*              es_fornecedor.data_implant                           = emitente.data-implant                    
            es_fornecedor.data_taxa                              = emitente.data-taxa                       
           es_fornecedor.dec_1                                  = emitente.dec-1 
             es_fornecedor.dec_2                                  = emitente.dec-2 
            es_fornecedor.dias_comp                              = emitente.dias-comp                       
            es_fornecedor.dt_fim_cred                            = emitente.dt-fim-cred                     
            es_fornecedor.dt_lim_cred                            = emitente.dt-lim-cred                     
            es_fornecedor.dt_ult_venda                           = emitente.dt-ult-venda*/                    
            es_fornecedor.e_mail                                 = emitente.e-mail                          
            es_fornecedor.emissao_ped                            = emitente.emissao-ped                     
            es_fornecedor.emite_bloq                             = emitente.emite-bloq                      
            es_fornecedor.emite_etiq                             = emitente.emite-etiq                      
            es_fornecedor.end_cobranca                           = emitente.end-cobranca                    
            es_fornecedor.endereco                               = emitente.endereco                        
            es_fornecedor.endereco_cob                           = emitente.endereco-cob                    
            es_fornecedor.esp_pd_venda                           = emitente.esp-pd-venda                    
            es_fornecedor.estado                                 = emitente.estado                          
            es_fornecedor.estado_cob                             = emitente.estado-cob                      
            es_fornecedor.estoque                                = emitente.estoque                         
           /* es_fornecedor.flag_pag                               = emitente.flag-pag                        
            es_fornecedor.forn_exp                               = emitente.forn-exp                        
            es_fornecedor.gera_ad                                = emitente.gera-ad                         
            es_fornecedor.gera_difer                             = emitente.gera-difer                      
            es_fornecedor.home_page                              = emitente.home-page                       
            es_fornecedor.hora_fim                               = emitente.hora-fim                        
            es_fornecedor.hora_ini                               = emitente.hora-ini   */                     
            /*es_fornecedor.identific                              = emitente.identific                       */
            /*es_fornecedor.ind_abrange_aval                       = emitente.ind-abrange-aval                
            es_fornecedor.ind_apr_cred                           = emitente.ind-apr-cred                    
            es_fornecedor.ind_atraso                             = emitente.ind-atraso                      
            es_fornecedor.ind_aval                               = emitente.ind-aval                        
            es_fornecedor.ind_aval_embarque                      = emitente.ind-aval-embarque               
            es_fornecedor.ind_cre_cli                            = emitente.ind-cre-cli                     
            es_fornecedor.ind_cred_abat                          = emitente.ind-cred-abat                   
            es_fornecedor.ind_dif_atrs_1                         = emitente.ind-dif-atrs-1                  
            es_fornecedor.ind_dif_atrs_2                         = emitente.ind-dif-atrs-2                  
            es_fornecedor.ind_div_atraso                         = emitente.ind-div-atraso                  
            es_fornecedor.ind_emit_retencao                      = emitente.ind-emit-retencao               
            es_fornecedor.ind_fat_par                            = emitente.ind-fat-par                     
            es_fornecedor.ind_lib_estoque                        = emitente.ind-lib-estoque                 
            es_fornecedor.ind_licenciador                        = emitente.ind-licenciador                 
            es_fornecedor.ind_moeda_tit                          = emitente.ind-moeda-tit                   
            es_fornecedor.ind_rendiment                          = emitente.ind-rendiment                   
            es_fornecedor.ind_sit_emitente                       = emitente.ind-sit-emitente              
            es_fornecedor.inf_complementar                       = emitente.inf-complementar                */
/*             es_fornecedor.ins_banc[1]                            = emitente.ins-banc[1] */
/*             es_fornecedor.ins_banc[2]                            = emitente.ins-banc[2] */
     /*       es_fornecedor.ins_est_cob                            = emitente.ins-est-cob                     
            es_fornecedor.ins_estadual                           = emitente.ins-estadual                    
            es_fornecedor.ins_municipal                          = emitente.ins-municipal                   
            es_fornecedor.insc_subs_trib                         = emitente.insc-subs-trib                  
            es_fornecedor.int_1                                  = emitente.int-1                           
            es_fornecedor.int_2                                  = emitente.int-2                           
            es_fornecedor.istr                                   = emitente.istr                            
            es_fornecedor.item_cli                               = emitente.item-cli                        
            es_fornecedor.lim_adicional                          = emitente.lim-adicional                   
            es_fornecedor.lim_credito                            = emitente.lim-credito                     
            es_fornecedor.linha_produt                           = emitente.linha-produt      */              
/*             es_fornecedor.log_1                                  = emitente.log-1 */
/*             es_fornecedor.log_2                                  = emitente.log-2 */
           /* es_fornecedor.mod_prefer                             = emitente.mod-prefer                      */
            es_fornecedor.modalidade                             = emitente.modalidade                      
            es_fornecedor.moeda_libcre                           = emitente.moeda-libcre                    
            es_fornecedor.nat_ope_ext                            = emitente.nat-ope-ext                     
            es_fornecedor.nat_operacao                           = emitente.nat-operacao                    
            es_fornecedor.natureza                               = emitente.natureza                        
            es_fornecedor.nome_abrev                             = emitente.nome-abrev                      
            es_fornecedor.nome_emit                              = emitente.nome-emit                       
            es_fornecedor.nome_matriz                            = emitente.nome-matriz                     
            es_fornecedor.nome_mic_reg                           = emitente.nome-mic-reg                    
            es_fornecedor.nr_cheque_devol                        = emitente.nr-cheque-devol                 
            es_fornecedor.nr_copias_ped                          = emitente.nr-copias-ped                   
            es_fornecedor.nr_dias                                = emitente.nr-dias                         
            es_fornecedor.nr_dias_atraso                         = emitente.nr-dias-atraso                  
            es_fornecedor.nr_dias_taxa                           = emitente.nr-dias-taxa                    
            es_fornecedor.nr_mesina                              = emitente.nr-mesina                       
            es_fornecedor.nr_peratr                              = emitente.nr-peratr                       
            es_fornecedor.nr_tab_progr                           = emitente.nr-tab-progr                    
            es_fornecedor.nr_tabpre                              = emitente.nr-tabpre                       
            es_fornecedor.nr_titulo                              = emitente.nr-titulo                       
/*             es_fornecedor.obs_entrega                            = emitente.obs-entrega */
/*             es_fornecedor.observacoes                            = emitente.observacoes */
            es_fornecedor.pais                                   = emitente.pais                            
            es_fornecedor.pais_cob                               = emitente.pais-cob                        
            es_fornecedor.per_max_canc                           = emitente.per-max-canc                    
            es_fornecedor.per_minfat                             = emitente.per-minfat                      
            es_fornecedor.perc_fat_ped                           = emitente.perc-fat-ped                    
            es_fornecedor.percent_verba                          = emitente.percent-verba                   
            es_fornecedor.percepcao                              = emitente.percepcao                       
            es_fornecedor.periodo_devol                          = emitente.periodo-devol                   
            es_fornecedor.port_prefer                            = emitente.port-prefer                     
            es_fornecedor.portador                               = emitente.portador                        
          /*  es_fornecedor.prog_emit                              = emitente.prog-emit                       
            es_fornecedor.prox_ad                                = emitente.prox-ad                         
            es_fornecedor.ramal_fac                              = emitente.ramal-fac                       
            es_fornecedor.ramal_fax                              = emitente.ramal-fax                       
            es_fornecedor.ramal_modem                            = emitente.ramal-modem     */                
/*             es_fornecedor.ramal[1]                               = emitente.ramal[1] 
            es_fornecedor.ramal[2]                               = emitente.ramal[2] */
    /*        es_fornecedor.recebe_inf_sci                         = emitente.recebe-inf-sci                  
            es_fornecedor.rend_tribut                            = emitente.rend-tribut                     
            es_fornecedor.resumo_mp                              = emitente.resumo-mp                       
            es_fornecedor.taxa_financ                            = emitente.taxa-financ                     
            es_fornecedor.telef_fac                              = emitente.telef-fac                       
            es_fornecedor.telef_modem                            = emitente.telef-modem                     
            es_fornecedor.telefax                                = emitente.telefax                         */
            /*es_fornecedor.telefone[1]                            = emitente.telefone[1]
            es_fornecedor.telefone[2]                            = emitente.telefone[2]*/
            /*es_fornecedor.telex                                  = emitente.telex                           
            es_fornecedor.tip_cob_desp                           = emitente.tip-cob-desp                    
            es_fornecedor.tp_desp_padrao                         = emitente.tp-desp-padrao                  
            es_fornecedor.tp_inspecao                            = emitente.tp-inspecao                     
            es_fornecedor.tp_pagto                               = emitente.tp-pagto                        
            es_fornecedor.tp_qt_prg                              = emitente.tp-qt-prg                       
            es_fornecedor.tp_rec_padrao                          = emitente.tp-rec-padrao                   
            es_fornecedor.tr_ar_valor                            = emitente.tr-ar-valor                     
            es_fornecedor.user_libcre                            = emitente.user-libcre                     
            es_fornecedor.utiliza_verba                          = emitente.utiliza-verba                   
            es_fornecedor.val_quota_media                        = emitente.val-quota-media                 
            es_fornecedor.valor_minimo                           = emitente.valor-minimo                    
            es_fornecedor.ven_domingo                            = emitente.ven-domingo                     
            es_fornecedor.ven_feriado                            = emitente.ven-feriado                     
            es_fornecedor.ven_sabado                             = emitente.ven-sabado                      
            es_fornecedor.vencto_dia_nao_util                    = emitente.vencto-dia-nao-util             
            es_fornecedor.vl_max_devol                           = emitente.vl-max-devol                    
            es_fornecedor.vl_min_ad                              = emitente.vl-min-ad */
            es_fornecedor.zip_cob_code                           = emitente.zip-cob-code                    
            es_fornecedor.zip_code                               = emitente.zip-code.
            


    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    /***  CàDIGO PARA SAÖDA EM 80 COLUNAS ***/

    if  tt-param.formato = 1 then do:

        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
        assign l-imprime = yes.
    end.

    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/

    if  tt-param.formato = 2 then do:

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
    end.
    
end.


if  l-imprime = no then do:
    if  tt-param.formato = 1 then do:
        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
    end.

    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.

if  tt-param.destino <> 1 then

    page stream str-rp.

else do:

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

if  tt-param.parametro then do:


   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Exporta Cadastro de Fornecedor"
        with stream-io side-labels overlay row 040 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.

end.

    output stream str-rp close.

procedure pi-print-editor:

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */

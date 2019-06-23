/*****************************************************************************
**       PROGRAMA: esap001rp.p
**       DATA....: Abril/2012
**       OBJETIVO: Altera Data Vencimento Lote
**       EMPRESA:  CSX Solution - Thiago Coutinho
**       VERSAO..: 2.06.00.000
******************************************************************************/
{include/i-prgvrs.i ESAP001RP 2.06.00.001}

/*----Definiá∆o de variaveis locais--------------- */
def var h-acomp as handle no-undo.

def temp-table tt-raw-digita no-undo
    field raw-digita as raw.

/* definiá∆o das temp-tables para recebimento de parÉmetros */
def temp-table tt-param no-undo
    field destino           as int
    field arquivo           as char format "x(35)"
    field usuario           as char format "x(12)"
    field data-exec         as date
    field hora-exec         as int
    field classifica        as int
    field desc-classifica   as char format "x(40)"
    field c-cod-estabel-ini like tit_ap.cod_estab
    field c-cod-estabel-fim like tit_ap.cod_estab
    field c-cod-esp-ini     like tit_ap.cod_espec_docto
    field c-cod-esp-fim     like tit_ap.cod_espec_docto
    field i-portador-ini    like tit_ap.cod_portador
    field i-portador-fim    like tit_ap.cod_portador
    field i-cod-fornec-ini  like tit_ap.cdn_fornecedor
    field i-cod-fornec-fim  like tit_ap.cdn_fornecedor
    field d-dt-vencimen-ini like tit_ap.dat_vencto_tit_ap
    field d-dt-vencimen-fim like tit_ap.dat_vencto_tit_ap
/**
    field d-dt-prev-pag-ini like tit-ap.dt-prev-pag
    field d-dt-prev-pag-fim like tit-ap.dt-prev-pag
**/
    .

def temp-table tt-digita no-undo
    field id            as int
    field sel           as char format "x(3)" label "Selec"
    field cod-estabel   like tit_ap.cod_estab
    field cod-fornec    like tit_ap.cdn_fornecedor
    field nome-abrev    like ems5.fornecedor.nom_abrev
    field cod-esp       like tit_ap.cod_espec_docto
    field serie         like tit_ap.cod_ser_docto
    field nr-docto      like tit_ap.cod_tit_ap
    field parcela       like tit_ap.cod_parcela
    field portador      like tit_ap.cod_portador
    field dt-vencimen   like tit_ap.dat_vencto_tit_ap
/*     field dt-prev-pag   like tit-ap.dt-prev-pag */
    field dt-emissao    like tit_ap.dat_emis_docto
    field valor-saldo   like tit_ap.val_sdo_tit_ap
    field vl-original   like tit_ap.val_origin_tit_ap
    field dt-modifica   like tit_ap.dat_vencto_tit_ap
    .

def temp-table tt_log_erros_tit_ap_alteracao no-undo
    field tta_cod_estab       as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cdn_fornecedor  as integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto as character format "x(3)" label "Esp≤cie Documento" column-label "Esp≤cie"
    field tta_cod_ser_docto   as character format "x(3)" label "S≤rie Documento" column-label "S≤rie"
    field tta_cod_tit_ap      as character format "x(10)" label "T≠tulo" column-label "T≠tulo"
    field tta_cod_parcela     as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_num_id_tit_ap   as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_num_mensagem    as integer format ">>>>,>>9" label "Número" column-label "Número Mensagem"
    field ttv_cod_tip_msg_dwb as character format "x(12)" label "Tipo Mensagem" column-label "Tipo Mensagem"
    field ttv_des_msg_erro    as character format "x(60)" label "Mensagem Erro" column-label "Inconsistºncia"
    field ttv_des_msg_ajuda_1 as character format "x(360)"
    field ttv_wgh_focus       as widget-handle format ">>>>>>9"
    .

/*----- DEFINICAO DE PARAMETROS -----*/
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.
def input param table for tt_log_erros_tit_ap_alteracao.

{utp/ut-glob.i}
{include/i-rpvar.i}

/*----- ACERTO NOS PARAMETROS -----*/
create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*----- DEFINE SAIDA PARA IMPRESSORA/ARQUIVO -----*/
{include/i-rpout.i}. 

assign c-titulo-relat = 'Relatorio de Alteraá∆o Data Vencimento - Lote'
       c-programa     = 'ESAP001'.

{include/i-rpcab.i}

/* executando de forma persistente utilit†rio de acompanhamento */
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp ('Imprimindo...').

/*----- L‡GICA -----*/
view frame f-cabec.
view frame f-rodape. 

/* corpo do relat¢rio */
run pi-acompanhar in h-acomp (input "Alteraá∆o Data Vencimento - Lote").

/*----- L‡GICA -----*/
run pi-imprimeDados.

/*----- FINALIZAÄ«O -----*/
{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return 'OK':U.

procedure pi-imprimeDados :
    find first tt-digita no-lock no-error.
    if  avail tt-digita then do:
        put "Dt Vcto Ant. Dt Vcto Atual    Fornec Nome Abreviado Est Esp SÇrie Documento        /P  Port Emiss∆o        Vl original     Valor Saldo" skip
            "------------ ------------- --------- -------------- --- --- ----- ---------------- -- ----- ---------- --------------- ---------------" skip.

        for each tt-digita no-lock:
            put tt-digita.dt-vencimen '   '
                tt-digita.dt-modifica '    '
                tt-digita.cod-fornec  ' ' 
                tt-digita.nome-abrev  '   '
                tt-digita.cod-estabel ' '
                tt-digita.cod-esp     ' '
                tt-digita.serie       '  '
                tt-digita.nr-docto    ' '
                tt-digita.parcela     ' '
                tt-digita.portador    ' '
                tt-digita.dt-emissao  ' '
                tt-digita.vl-original ' '
                tt-digita.valor-saldo skip.

            for each tt_log_erros_tit_ap_alteracao no-lock
               where tt_log_erros_tit_ap_alteracao.tta_cod_estab       = tt-digita.cod-estabel
                 and tt_log_erros_tit_ap_alteracao.tta_cdn_fornecedor  = tt-digita.cod-fornec
                 and tt_log_erros_tit_ap_alteracao.tta_cod_espec_docto = tt-digita.cod-esp
                 and tt_log_erros_tit_ap_alteracao.tta_cod_ser_docto   = tt-digita.serie
                 and tt_log_erros_tit_ap_alteracao.tta_cod_tit_ap      = tt-digita.nr-docto
                 and tt_log_erros_tit_ap_alteracao.tta_cod_parcela     = tt-digita.parcela:
        
                put skip(2).
        
                put tt_log_erros_tit_ap_alteracao.ttv_num_mensagem     ' - '
                    tt_log_erros_tit_ap_alteracao.ttv_des_msg_erro     
                    skip.
            end.
        end.      
    end. /* if avail tt-digita then */
end procedure.

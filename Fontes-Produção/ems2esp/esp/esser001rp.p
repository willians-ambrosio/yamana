/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSER001RP 0.12.00.000 } /*** 010002 ***/
{utp/utapi019.i}

{btb/btb912zb.i}
/*****************************************************************************
**
**       Programa: ESSER001RP.p
**
**       Data....: 12/07/00
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Relat¢rio Usu rio Mestre
**
**       VersÆo..: 1.00.000 - sec
**
**       OBS.....: Este fonte (nÆo) foi gerado pelo Data Viewer
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "ESSER001RP".


/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/
DEF STREAM str-csv.

def temp-table tt-raw-digita NO-UNDO
    field raw-digita as raw.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field formato          as integer
    field cod_usuario_ini  as char
    field cod_usuario_fim  as char
    field nom_usuario_ini  as char
    field nom_usuario_fim  as char
    field log_ativos       as logical
    field log_inativos     as LOGICAL
    FIELD rs-tipo-execucao AS INTEGER
    FIELD l-csv            AS LOGICAL.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

{utp/ut-glob.i}
{include/i_fnctrad.i}

/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/

def new shared var v-log-ativo as logical format "sim/nao" label "Ativos" view-as toggle-box.
def new shared var v-log-inativo as logical format "sim/nao" label "Inativos" view-as toggle-box.

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/

def new shared var c-cod_usuario-ini like usuar_mestre.cod_usuario format "x(12)" initial "" no-undo.
def new shared var c-cod_usuario-fim like usuar_mestre.cod_usuario format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var c-nom_usuario-ini like usuar_mestre.nom_usuario format "x(20)" initial "" no-undo.
def new shared var c-nom_usuario-fim like usuar_mestre.nom_usuario format "x(20)" initial "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" no-undo.

DEFINE VARIABLE c-destino  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-arquivo  AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-ambiente AS CHARACTER NO-UNDO.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/****************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
Def Var v-Ltr                As Char   No-undo.
Def Var v-Ltr1               As Char   No-undo.
DEF VAR c-ativo              AS CHAR   NO-UNDO.
DEFINE VARIABLE c-email AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-csv AS CHARACTER   NO-UNDO.

/****************** Defini‡ao de Forms do Relat¢rio 172 Colunas ***************************************/

form usuar_mestre.cod_usuario       column-label "Usu rio"          format "x(12)"
     usuar_mestre.nom_usuario       column-label "Nome"             format "x(32)" 
     usuar_mestre.cod_idiom_orig    column-label "Idioma Usu rio"   format "x(8)" 
     usuar_mestre.ind_tip_usuar     column-label "Tipo Usu rio"     format "X(13)" 
     c-email                        column-label "E-Mail Local"     format "x(40)" 
     usuar_mestre.dat_inic_valid    column-label "Inic Validade"    format "99/99/9999" 
     usuar_mestre.dat_fim_valid     column-label "Fim Validade"     format "99/99/9999" 
     c-ativo                        column-label "Status"           format "x(8)" 
     usuar_mestre.ind_tip_aces_usuar column-label "Tipo Acesso Usu rio"
     with down width 250 no-box stream-io frame f-relat-09-172.


 &IF "{&FNC_MULTI_IDIOMA}" = "Yes" &THEN
     Run utp/ut-trfrrp.p (input Frame f-relat-09-172:Handle).
&ENDIF

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


{include/i-rpvar.i}

{utp/ut-liter.i Relat¢rio_Usu rio_Mestre *}
assign c-programa     = "ESSER001RP"
       c-versao       = "0.12"
       c-revisao      = "00.000"
       c-titulo-relat = Return-value
       c-sistema      = "".

if  tt-param.formato = 2 then do:


{include/i-rpcab.i &stream="str-rp"}

end. /* tt-param.formato = 2 */

IF SEARCH(tt-param.arquivo) <> ? THEN
    OS-DELETE VALUE(tt-param.arquivo) NO-ERROR.


{include/i-rpout.i &stream="stream str-rp"}

assign i-ep-codigo-usuario = v_cdn_empres_usuar
       v_cdn_empres_usuar  = i-ep-codigo-usuario.

assign v-log-ativo = tt-param.log_ativos
       v-log-inativo = tt-param.log_inativos
       c-cod_usuario-ini = tt-param.cod_usuario_ini
       c-cod_usuario-fim = tt-param.cod_usuario_fim
       c-nom_usuario-ini = tt-param.nom_usuario_ini
       c-nom_usuario-fim = tt-param.nom_usuario_fim.

{varinc/var00002.i}

run utp/ut-acomp.p persistent set h-acomp.

/*Run getVariable In hSPContexto(Input "i-ep-codigo-usuario",     Output i-ep-codigo-usuario).
Run getVariable In hSPContexto(Input "v_nom_razao_social",      Output c-empresa).*/

ASSIGN c-ambiente = IF CONNECTED("hresp") THEN "HCM" ELSE "TOTVS 12".


def var l-imprime as logical no-undo.

assign l-imprime = no.
if  tt-param.destino = 1
then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2
    then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".
Run utp/ut-liter.p (input v-cod-destino-impres,"*","").
Assign v-cod-destino-impres = Return-value.

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Acompanhamento_Relat¢rio *}
run pi-inicializar in h-acomp(input Return-value).

assign v-num-reg-lidos = 0.


IF tt-param.l-csv = YES THEN DO:
  
END.

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

IF tt-param.l-csv THEN DO:

    ASSIGN c-csv = SUBSTR(tt-param.arquivo,1,INDEX(tt-param.arquivo,".") - 1) + ".csv".

    OUTPUT STREAM str-csv TO VALUE(c-csv) NO-CONVERT.

     PUT STREAM str-csv
         "Usu rio;Nome;Idioma Usu rio;Tipo Usu rio;E-Mail Local;Inic Validade;Fim Validade;Status;Tipo Acesso Usu rio" SKIP.
END.

FOR EACH usuar_mestre NO-LOCK 
     where usuar_mestre.cod_usuario >= c-cod_usuario-ini AND 
           usuar_mestre.cod_usuario <= c-cod_usuario-fim AND 
           usuar_mestre.nom_usuario >= c-nom_usuario-ini AND 
           usuar_mestre.nom_usuario <= c-nom_usuario-fim AND 
           ((usuar_mestre.dat_inic_valid  <= TODAY AND 
           usuar_mestre.dat_fim_valid  >= TODAY AND 
           v-log-ativo  = yes) or
           (usuar_mestre.dat_inic_valid  > today OR 
           (usuar_mestre.dat_fim_valid  < today AND 
           v-log-inativo  = yes)))
    break by usuar_mestre.cod_usuario
          by usuar_mestre.nom_usuario:

    ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1.

    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    /***  CàDIGO PARA SAÖDA EM 172 COLUNAS ***/
    IF (usuar_mestre.dat_inic_valid  <= today AND usuar_mestre.dat_fim_valid  >= today ) THEN
       ASSIGN c-ativo = "ENABLE".
    ELSE 
       ASSIGN c-ativo = "DISABLE".

    ASSIGN l-imprime = YES
           c-email = usuar_mestre.cod_e_mail_local.

    DO i-cont = 1 TO 32:

        ASSIGN c-email = REPLACE(c-email,CHR(i-cont),"").

    END.
    
    DISPLAY STREAM str-rp 
             usuar_mestre.cod_usuario       
             usuar_mestre.nom_usuario       
             usuar_mestre.cod_idiom_orig    
             usuar_mestre.ind_tip_usuar     
             c-email                        
             usuar_mestre.dat_inic_valid    
             usuar_mestre.dat_fim_valid     
             c-ativo                        
             usuar_mestre.ind_tip_aces_usuar
        with stream-io frame f-relat-09-172 WIDTH 250.
        down stream str-rp with frame f-relat-09-172.
        
    IF tt-param.l-csv THEN 
      PUT STREAM str-csv 
         UNFORMATTED
                 usuar_mestre.cod_usuario                  ";"
                 usuar_mestre.nom_usuario                  ";"
                 CAPS(usuar_mestre.cod_idiom_orig)         ";"
                 CAPS(usuar_mestre.ind_tip_usuar)          ";"
                 c-email /*usuar_mestre.cod_e_mail_local*/ ";"
                 usuar_mestre.dat_inic_valid               ";"
                 usuar_mestre.dat_fim_valid                ";" 
                 CAPS(c-ativo)                             ";"
                 usuar_mestre.ind_tip_aces_usuar SKIP.
    
end.

IF tt-param.l-csv THEN 
    OUTPUT STREAM str-csv CLOSE.

if  l-imprime = no AND tt-param.l-csv = NO then do:
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.

run pi-finalizar in h-acomp.

page stream str-rp.

{include/i-rpclo.i &stream="stream str-rp"}

find first param_email no-lock no-error.

/* ASSIGN c-destino = "AHR-SOXReportDatasul@Yamana.com". */
/*                                                       */
/* ASSIGN c-destino = "sergio.silveira@dsc.com.br".      */

FOR EACH es_parametro
         WHERE es_parametro.cod_prog_dtsul = "ESSER001" AND
               es_parametro.cod_referencia = "E-MAIL"   
         NO-LOCK:
   ASSIGN c-destino = es_parametro.cod_parametro.
END.

/*ASSIGN c-destino = "daniel.lima@yamana.com.br;sergio.silveira@dsc.com.br".*/

ASSIGN c-arquivo = tt-param.arquivo.

IF i-num-ped-exec-rpw <> 0 Then 
   Assign c-arquivo = c-dir-spool-servid-exec + "~/" + tt-param.arquivo.

create tt-envio2.
assign tt-envio2.versao-integracao = 1
       tt-envio2.exchange          = param_email.log_servid_exchange
       tt-envio2.servidor          = param_email.cod_servid_e_mail
       tt-envio2.porta             = param_email.num_porta
       tt-envio2.destino           = c-destino
       tt-envio2.assunto           = "Relat¢rio Usu rio Mestre - ESSER001"
       tt-envio2.remetente         = "adm@yamana.com"
       tt-envio2.copia             = ""
       tt-envio2.mensagem          = ""
       tt-envio2.importancia       = 1
       tt-envio2.log-enviada       = no
       tt-envio2.log-lida          = no
       tt-envio2.acomp             = no
       tt-envio2.arq-anexo         = c-arquivo.

RUN utp/utapi019.p PERSISTENT SET h-utapi019.

IF CONNECTED("hresp") THEN
   ASSIGN c-ambiente = "HCM-TOTVS 12".
ELSE
   ASSIGN c-ambiente = "ERP-TOTVS 12".

CREATE tt-mensagem.
ASSIGN tt-mensagem.seq-mensagem = 1
       tt-mensagem.mensagem     = "Envio automatizado de reportes Datasul - Controles Mensais SOX" + CHR(10) + CHR(13).

CREATE tt-mensagem.
ASSIGN tt-mensagem.seq-mensagem = 2
       tt-mensagem.mensagem     = "Controle : GC AC KY15" + CHR(10) + CHR(13).

CREATE tt-mensagem.
ASSIGN tt-mensagem.seq-mensagem = 3
       tt-mensagem.mensagem     = "Ambiente : " + c-ambiente + CHR(10) + CHR(13).

CREATE tt-mensagem.
ASSIGN tt-mensagem.seq-mensagem = 4
       tt-mensagem.mensagem     = "Vocˆ est  recebendo este email pois foi executada a rotina ESSER001 do produto Totvs 12".
  
FOR EACH tt-envio2:
   RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                  INPUT  TABLE tt-mensagem,
                                  OUTPUT TABLE tt-erros).
END.

DELETE PROCEDURE h-utapi019.  

if tt-param.rs-tipo-execucao = 2 then
   run agendarExecucaoAutomatica.

return 'OK'.



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




procedure agendarExecucaoAutomatica private:
    /*******************************************************************************************************************
     Gera nova chamada RPW levando em consideracao a hora atual mais o intervalo em segundos gravado nos Parametros da 
     Engenharia
    *******************************************************************************************************************/
    DEFINE VARIABLE i-tempo-rpw AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-hra-rpw   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE d-dat-rpw   AS DATE        NO-UNDO.

    EMPTY TEMP-TABLE tt_param_segur.
    EMPTY TEMP-TABLE tt_ped_exec.
    EMPTY TEMP-TABLE tt_ped_exec_param.

    ASSIGN i-tempo-rpw          = TIME
           c-hra-rpw            = REPLACE (STRING (i-tempo-rpw, "HH:MM:SS"),":","")
           tt-param.data-exec   = TODAY.
    
    ASSIGN tt-param.data-exec = IF MONTH(TODAY) = 12 THEN ( DATE(01,01,YEAR(TODAY) + 1)) ELSE (DATE(MONTH(TODAY) + 1,01,YEAR(TODAY))).

    ASSIGN d-dat-rpw = tt-param.data-exec.

    FIND FIRST ped_exec NO-LOCK WHERE ped_exec.num_ped_exec = i-num-ped-exec-rpw NO-ERROR.
    IF NOT AVAIL ped_exec THEN RETURN.

    FIND FIRST ped_exec_param NO-LOCK WHERE ped_exec_param.num_ped_exec = ped_exec.num_ped_exec NO-ERROR.
    IF NOT AVAIL ped_exec_param THEN RETURN.

    CREATE tt_param_segur.
    ASSIGN tt_param_segur.tta_num_vers_integr_api      = 3
           tt_param_segur.tta_cod_aplicat_dtsul_corren = ped_exec_param.cod_aplicat_dtsul
           tt_param_segur.tta_cod_empres_usuar         = ped_exec_param.cod_empresa
           tt_param_segur.tta_cod_modul_dtsul_corren   = ped_exec_param.cod_modul_dtsul
           tt_param_segur.tta_cod_pais_empres_usuar    = ped_exec_param.cod_pais
           tt_param_segur.tta_cod_usuar_corren         = ped_exec.cod_usuario
           tt_param_segur.tta_cod_usuar_corren_criptog = ped_exec_param.cod_usuar_criptog.
    
    CREATE tt_ped_exec.
    ASSIGN tt_ped_exec.tta_num_seq                     = 1
           tt_ped_exec.tta_cod_usuario                 = ped_exec.cod_usuario
           tt_ped_exec.tta_cod_prog_dtsul              = "ESSER001"
           tt_ped_exec.tta_cod_prog_dtsul_rp           = "esp/esser001rp.p"
           tt_ped_exec.tta_cod_release_prog_dtsul      = "0.12.00.000"
           tt_ped_exec.tta_dat_exec_ped_exec           = d-dat-rpw
           tt_ped_exec.tta_hra_exec_ped_exec           = c-hra-rpw
           tt_ped_exec.tta_cod_servid_exec             = ped_exec.cod_servid_exec
           tt_ped_exec.tta_cdn_estil_dwb               = 97.

    ASSIGN tt_ped_exec.tta_log_exec_prog_depend = YES
           tt_ped_exec.tta_num_ped_exec_pai     = i-num-ped-exec-rpw.
           
    CREATE tt_ped_exec_param.
    ASSIGN tt_ped_exec_param.tta_num_seq               = 1
           tt_ped_exec_param.tta_cod_dwb_file          = "esp/esser001rp.p"
           tt_ped_exec_param.tta_cod_dwb_output        = (IF tt-param.destino = 1 THEN "Impressora" ELSE "Arquivo")
           tt_ped_exec_param.tta_nom_dwb_printer       = tt-param.arquivo
           tt_ped_exec_param.tta_cod_dwb_print_layout  = tt-param.arquivo.

    RAW-TRANSFER tt-param TO tt_ped_exec_param.tta_raw_param_ped_exec.

    RUN btb/btb912zb.p (INPUT-OUTPUT TABLE tt_param_segur,
                        INPUT-OUTPUT TABLE tt_ped_exec,
                        INPUT TABLE tt_ped_exec_param,
                        INPUT TABLE tt_ped_exec_param_aux,
                        INPUT TABLE tt_ped_exec_sel).
end procedure.

/* fim do programa */

/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSER002RP 0.12.00.000 } /*** 010002 ***/
{utp/utapi019.i}

{btb/btb912zb.i}
/*****************************************************************************
**
**       Programa: ESSER002RP.p
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

define variable c-prog-gerado as character no-undo initial "ESSER002RP".


/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/
def temp-table tt-raw-digita NO-UNDO
    field raw-digita as raw.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as DATE 
    field hora-exec        as INTEGER 
    field classifica       as INTEGER 
    field desc-classifica  AS CHAR format "x(40)"
    field formato          as INTEGER 
    field cod_usuario_ini  as CHAR 
    field cod_usuario_fim  as CHAR 
    field nom_usuario_ini  as CHAR 
    field nom_usuario_fim  as CHAR 
    field log_ativos       as LOGICAL 
    field log_inativos     as LOGICAL
    FIELD dt_ini           AS DATE
    FIELD dt_fim           AS DATE
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
DEFINE VARIABLE c-arq-csv    AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-dados        
    field cod_usuario          like usuar_mestre.cod_usuario        
    field nom_usuario          like usuar_mestre.nom_usuario        
    field dat_inic_valid       like usuar_mestre.dat_inic_valid     
    field dat_fim_valid        like usuar_mestre.dat_fim_valid      
    field sit                  AS CHAR 
    field ind_tip_aces_usuar   like usuar_mestre.ind_tip_aces_usuar 
    field idi_tip_login        like usuar_mestre.idi_tip_login      
    field nom_dir_spool        like usuar_mestre.nom_dir_spool      
    field log_full_determ      like usuar_mestre.log_full_determ
    FIELD tipo_acesso          AS CHAR 
    FIELD cdn_empresa          LIKE usuar_aplicat_rh.cdn_empresa                                                          
    FIELD cdn_estab            LIKE usuar_aplicat_rh.cdn_estab                                                            
    FIELD cdn_funcionario      LIKE usuar_aplicat_rh.cdn_funcionario                                                      
    FIELD num_pessoa_fisic     LIKE usuar_aplicat_rh.num_pessoa_fisic                                                     
    FIELD acess_turno          AS CHAR 
    FIELD tip_usuar            AS CHAR 
    FIELD log_usuar_epm        LIKE usuar_aplicat_rh.log_usuar_epm
    field cod_domin_so         AS CHAR
    field cod_usuar_so         AS CHAR
    field cod_empresa          AS CHAR.

DEF BUFFER b-tt-dados FOR tt-dados.

/****************** Defini‡ao de Forms do Relat¢rio 172 Colunas ***************************************/

form usuar_mestre.cod_usuario       column-label "Usu rio"          format "x(12)" at 001
     usuar_mestre.nom_usuario       column-label "Nome"             format "x(32)" at 014
     usuar_mestre.cod_idiom_orig    column-label "Idioma Usu rio"   format "x(8)"  at 047
     usuar_mestre.ind_tip_usuar     column-label "Tipo Usu rio"     format "X(13)" at 062
     usuar_mestre.cod_e_mail_local  column-label "E-Mail Local"     format "x(30)" at 076
     usuar_mestre.dat_inic_valid    column-label "Inic Validade"    format "99/99/9999" at 107
     usuar_mestre.dat_fim_valid     column-label "Fim Validade"     format "99/99/9999" at 121
     c-ativo                        column-label "Status" format "x(8)" AT 134
     usuar_mestre.ind_tip_aces_usuar column-label "Tipo Acesso Usu rio"
     with down width 172 no-box stream-io frame f-relat-09-172.
&IF "{&FNC_MULTI_IDIOMA}" = "Yes" &THEN
     Run utp/ut-trfrrp.p (input Frame f-relat-09-172:Handle).
&ENDIF

create tt-param.
raw-transfer raw-param to tt-param.


IF tt-param.l-csv = YES THEN 
    ASSIGN c-arq-csv = replace(REPLACE(tt-param.arquivo,'txt','csv'),'lst','csv').

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

{include/i-rpvar.i}

{utp/ut-liter.i Relat¢rio_Usu rio_Mestre *}
assign c-programa     = "ESSER002RP"
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

RUN utp/ut-liter.p (INPUT v-cod-destino-impres,"*","").
ASSIGN v-cod-destino-impres = RETURN-VALUE.

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Acompanhamento_Relat¢rio *}
run pi-inicializar in h-acomp(input Return-value).

assign v-num-reg-lidos = 0.

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

ASSIGN tt-param.l-csv = YES.

for each usuar_mestre no-lock
         where usuar_mestre.cod_usuario >= c-cod_usuario-ini AND 
               usuar_mestre.cod_usuario <= c-cod_usuario-fim AND 
               usuar_mestre.nom_usuario >= c-nom_usuario-ini AND 
               usuar_mestre.nom_usuario <= c-nom_usuario-fim AND 
               ((usuar_mestre.dat_inic_valid  <= tt-param.dt_ini  AND 
                 usuar_mestre.dat_fim_valid   >= tt-param.dt_fim  AND 
               v-log-ativo  = YES) OR 
               (usuar_mestre.dat_inic_valid  > tt-param.dt_ini  OR 
               (usuar_mestre.dat_fim_valid   < tt-param.dt_fim  AND 
               v-log-inativo  = YES)))
    break by usuar_mestre.cod_usuario
          by usuar_mestre.nom_usuario:

      ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1.
      RUN pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

      /***  CàDIGO PARA SAÖDA EM 172 COLUNAS ***/
      IF (usuar_mestre.dat_inic_valid  <= tt-param.dt_ini AND usuar_mestre.dat_fim_valid  >= tt-param.dt_fim ) THEN
         ASSIGN c-ativo = "ATIVO".
      ELSE 
         ASSIGN c-ativo = "INATIVO".

      RUN utp/ut-liter.p (input usuar_mestre.ind_tip_usuar,"*","").

      DISPLAY STREAM str-rp usuar_mestre.cod_usuario
               usuar_mestre.nom_usuario
               usuar_mestre.cod_idiom_orig
               RETURN-VALUE @ usuar_mestre.ind_tip_usuar
               usuar_mestre.cod_e_mail_local
               usuar_mestre.dat_inic_valid
               usuar_mestre.dat_fim_valid
               c-ativo
               usuar_mestre.ind_tip_aces_usuar
       with stream-io frame f-relat-09-172.
       down stream str-rp with frame f-relat-09-172.
              
      CREATE tt-dados.
      BUFFER-COPY usuar_mestre TO tt-dados
          ASSIGN tt-dados.sit = c-ativo.

      IF c-ambiente = "HCM" THEN DO:

         FIND FIRST usuar_aplicat_rh OF usuar_mestre NO-LOCK NO-ERROR.
         IF AVAIL usuar_aplicat_rh THEN
             ASSIGN tt-dados.tipo_acesso      = entry(usuar_aplicat_rh.idi_tip_aces_inform_func,{database/inpy/i01py288.i 03})
                    tt-dados.cdn_empresa      = usuar_aplicat_rh.cdn_empresa
                    tt-dados.cdn_estab        = usuar_aplicat_rh.cdn_estab
                    tt-dados.cdn_funcionario  = usuar_aplicat_rh.cdn_funcionario
                    tt-dados.num_pessoa_fisic = usuar_aplicat_rh.num_pessoa_fisic
                    tt-dados.acess_turno      = entry(usuar_aplicat_rh.idi_tip_permis_aces_turno_trab,{database/inpy/i02py288.i 03})
                    tt-dados.tip_usuar        = entry(usuar_aplicat_rh.idi_tip_usuar_rh,{database/inpy/i03py288.i 03})
                    tt-dados.log_usuar_epm    = usuar_aplicat_rh.log_usuar_epm.
      END.

      FOR EACH usuar_mestre_ext OF usuar_mestre NO-LOCK:

            IF tt-dados.cod_domin_so = ""
               THEN ASSIGN tt-dados.cod_domin_so = usuar_mestre_ext.cod_domin_so 
                           tt-dados.cod_usuar_so = usuar_mestre_ext.cod_usuar_so.

            FIND b-tt-dados WHERE
                 b-tt-dados.cod_usuario = usuar_mestre_ext.cod_usuario AND 
                 b-tt-dados.cod_domin_so = usuar_mestre_ext.cod_domin_so AND 
                 b-tt-dados.cod_usuar_so = usuar_mestre_ext.cod_usuar_so NO-ERROR.
            IF NOT AVAIL b-tt-dados THEN DO:
                CREATE b-tt-dados.
                BUFFER-COPY tt-dados TO b-tt-dados.
            END.
            ASSIGN b-tt-dados.cod_domin_so = usuar_mestre_ext.cod_domin_so
                   b-tt-dados.cod_usuar_so = usuar_mestre_ext.cod_usuar_so.
      END.

      FOR EACH segur_empres_usuar OF usuar_mestre NO-LOCK:

         IF tt-dados.cod_empresa = ""
              THEN ASSIGN tt-dados.cod_empresa = segur_empres_usuar.cod_empresa.

         FIND FIRST b-tt-dados WHERE
                    b-tt-dados.cod_empresa = "" AND 
                    b-tt-dados.cod_usuario = segur_empres_usuar.cod_usuario NO-ERROR.
         IF AVAIL b-tt-dados THEN
                 ASSIGN b-tt-dados.cod_empresa = segur_empres_usuar.cod_empresa.

          FIND FIRST b-tt-dados WHERE
                    b-tt-dados.cod_empresa = segur_empres_usuar.cod_empresa AND
                    b-tt-dados.cod_usuario = segur_empres_usuar.cod_usuario NO-ERROR.
          IF NOT AVAIL b-tt-dados THEN DO:
            CREATE b-tt-dados.
            BUFFER-COPY tt-dados TO b-tt-dados
                ASSIGN b-tt-dados.cod_empresa = segur_empres_usuar.cod_empresa.

          END.
      END.
end.


OUTPUT TO VALUE(c-arq-csv) NO-CONVERT.

IF NOT c-ambiente = "HCM" THEN
PUT UNFORMATTED
    "Usu rio;Nome;Dt Ini Valid;Dt Fim Valid;Situa‡Æo;Tipo Acesso;Tipo Login;Diret¢rio Spool;Full Determinado;Dom¡nio;Usu rio SO;Empresa" SKIP.

IF c-ambiente = "HCM" THEN
    PUT "Usu rio;Nome;Dt Ini Valid;Dt Fim Valid;Situa‡Æo;Tipo Acesso;Tipo Login;Diret¢rio Spool;Full Determinado;Dom¡nio;Usu rio SO;Empresa;Tipo Acesso Usu rio;Empresa;Estab;Matr¡cula;Pessoa Fisica;Acesso Turno;Tipo Usu rio;Perm. Atualiz" SKIP.


FOR EACH tt-dados:

      PUT UNFORMATTED
            tt-dados.cod_usuario                  ";"          
            tt-dados.nom_usuario                  ";"          
            tt-dados.dat_inic_valid               ";"          
            tt-dados.dat_fim_valid                ";"          
            tt-dados.sit                          ";"      
            tt-dados.ind_tip_aces_usuar           ';'          
           (IF tt-dados.idi_tip_login = 1 THEN "Menu" ELSE "Desktop") ';'          
            tt-dados.nom_dir_spool                ';'          
            tt-dados.log_full_determ FORMAT "Sim/NÆo"  ';'
            tt-dados.cod_domin_so                 ';'
            tt-dados.cod_usuar_so                 ';'
            tt-dados.cod_empresa                  ';'.
    
      IF c-ambiente = "HCM" THEN
          PUT UNFORMATTED
            tt-dados.tipo_acesso                  ";"     
            tt-dados.cdn_empresa                  ";"
            tt-dados.cdn_estab                    ";"
            tt-dados.cdn_funcionario              ";"
            tt-dados.num_pessoa_fisic             ";"
            tt-dados.acess_turno                  ";"
            tt-dados.tip_usuar                    ";"
            tt-dados.log_usuar_epm FORMAT "Sim/NÆo" ";".
    
      PUT SKIP.
END.

OUTPUT CLOSE.

MESSAGE "Arquivo gerado" skip
        c-arq-csv
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

if  l-imprime = no AND tt-param.l-csv = NO then do:
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.

run pi-finalizar in h-acomp.

page stream str-rp.

{include/i-rpclo.i &STREAM="STREAM str-rp"}


RETURN 'OK'.






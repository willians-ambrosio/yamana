/*****************************************************************************
**       Programa: aval0254.p
**       Data....: 16/03/10
**       Autor...: DATASUL S.A.
**       Objetivo: Avalia‡äes HCM
**       VersÆo..: 1.00.000 - 30100979
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "AVAL0254".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

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
    field i-ferra-abre         as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field v-cod-tipo-grafico-dv203 as character
    field num_avpes_emitid as integer
    field i-num_pessoa_fisic_avaldor-ini like avpes_emitid.num_pessoa_fisic_avaldor
    field i-num_pessoa_fisic_avaldor-fim like avpes_emitid.num_pessoa_fisic_avaldor
    field da-dat_emis_avpes-ini like avpes_emitid.dat_emis_avpes
    field da-dat_emis_avpes-fim like avpes_emitid.dat_emis_avpes
    field da-dat_final_period_avpes-ini like avpes_emitid.dat_final_period_avpes
    field da-dat_final_period_avpes-fim like avpes_emitid.dat_final_period_avpes
    field da-dat_previs_conclus_meta-ini like avpes_meta_indual.dat_previs_conclus_meta
    field da-dat_previs_conclus_meta-fim like avpes_meta_indual.dat_previs_conclus_meta
    field c-cod_rh_ccusto-ini like funcionario.cod_rh_ccusto
    field c-cod_rh_ccusto-fim like funcionario.cod_rh_ccusto
    field i-cdn_empresa-ini like avpes_emitid.cdn_empresa
    field i-cdn_empresa-fim like avpes_emitid.cdn_empresa
    field l-log_avpes_finaliz-ini like avpes_emitid.log_avpes_finaliz
    field l-log_avpes_finaliz-fim like avpes_emitid.log_avpes_finaliz
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

def var v-cod-tipo-grafico-dv203-tmp as char no-undo.
def new shared var v-cod-tipo-grafico-dv203 as character format "x(30)" label "Tipo Gr fico" view-as combo-box list-items "<<Nenhum>>","Colunas Agrupadas","Colunas Agrupadas 3D","Colunas 3D","Barras  Agrupadas","Barras  Agrupadas 3D","Linhas","Linhas  Com Marcadores","Linhas  3D","Pizza","Pizza   Explodida","Pizza   3D","Pizza   Explodida 3D","Colunas Cilindricas Agrupadas","Barras  Cilindricas Agrupadas","Colunas Cilindricas 3D","Colunas C“nicas Agrupadas","Barras  C“nicas Agrupadas","Colunas C“nicas 3D","Colunas Piramidais Agrupadas","Barras  Piramidais Agrupadas","Colunas Piramidais 3D" .
def new shared var num_avpes_emitid as integer format ">>>>>>>>9" label "N§ avalia‡Æo emitida".

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var i-num_pessoa_fisic_avaldor-ini like avpes_emitid.num_pessoa_fisic_avaldor format ">>>,>>>,>>9" initial 0 no-undo.
def new shared var i-num_pessoa_fisic_avaldor-fim like avpes_emitid.num_pessoa_fisic_avaldor format ">>>,>>>,>>9" initial 999999999 no-undo.
def new shared var da-dat_emis_avpes-ini like avpes_emitid.dat_emis_avpes format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dat_emis_avpes-fim like avpes_emitid.dat_emis_avpes format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dat_final_period_avpes-ini like avpes_emitid.dat_final_period_avpes format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dat_final_period_avpes-fim like avpes_emitid.dat_final_period_avpes format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var da-dat_previs_conclus_meta-ini like avpes_meta_indual.dat_previs_conclus_meta format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dat_previs_conclus_meta-fim like avpes_meta_indual.dat_previs_conclus_meta format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var c-cod_rh_ccusto-ini like funcionario.cod_rh_ccusto format "x(08)" initial "" no-undo.
def new shared var c-cod_rh_ccusto-fim like funcionario.cod_rh_ccusto format "x(08)" initial "ZZZZZZZZ" no-undo.
def new shared var i-cdn_empresa-ini like avpes_emitid.cdn_empresa format "zz9" initial 0 no-undo.
def new shared var i-cdn_empresa-fim like avpes_emitid.cdn_empresa format "zz9" initial 999 no-undo.
def new shared var l-log_avpes_finaliz-ini like avpes_emitid.log_avpes_finaliz format "S/N" initial no no-undo.
def new shared var l-log_avpes_finaliz-fim like avpes_emitid.log_avpes_finaliz format "S/N" initial yes no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/********************* Fun‡Æo para tratamento (tradu‡Æo) de strings *************************/

FUNCTION translate RETURNS CHARACTER (str AS CHAR) FORWARD.

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

DEF BUFFER b-rh_pessoa_fisic FOR rh_pessoa_fisic.

define new shared stream str-rp.


if connected("dthrpyc") then do:
  def var v_han_fpapi003 as handle         no-undo.
  def VAR v_log_per_sal  as log    init no no-undo.
  run prghur/fpp/fpapi003.p persistent set v_han_fpapi003 (input tt-param.usuario,
                                                           input tt-param.v_num_tip_aces_usuar).
  RUN prghur/fpp/fpapi006.p (INPUT  v_cod_usuar_corren, 
                             INPUT  v_num_tip_aces_usuar, 
                             INPUT  v_cod_grp_usuar_lst, 
                             OUTPUT v_log_per_sal).
end.


assign c-programa     = "aval0254"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Avalia‡äes HCM"
       c-sistema      = "".


find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".


if  tt-param.destino = 3
then assign tt-param.arquivo = session:temp-directory + "aval02BB.csv".


run grapi/gr2013c.p(input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input yes).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       v-cod-tipo-grafico-dv203 = tt-param.v-cod-tipo-grafico-dv203
       num_avpes_emitid = tt-param.num_avpes_emitid
       i-num_pessoa_fisic_avaldor-ini = tt-param.i-num_pessoa_fisic_avaldor-ini
       i-num_pessoa_fisic_avaldor-fim = tt-param.i-num_pessoa_fisic_avaldor-fim
       da-dat_emis_avpes-ini = tt-param.da-dat_emis_avpes-ini
       da-dat_emis_avpes-fim = tt-param.da-dat_emis_avpes-fim
       da-dat_final_period_avpes-ini = tt-param.da-dat_final_period_avpes-ini
       da-dat_final_period_avpes-fim = tt-param.da-dat_final_period_avpes-fim
       da-dat_previs_conclus_meta-ini = tt-param.da-dat_previs_conclus_meta-ini
       da-dat_previs_conclus_meta-fim = tt-param.da-dat_previs_conclus_meta-fim
       c-cod_rh_ccusto-ini = tt-param.c-cod_rh_ccusto-ini
       c-cod_rh_ccusto-fim = tt-param.c-cod_rh_ccusto-fim
       i-cdn_empresa-ini = tt-param.i-cdn_empresa-ini
       i-cdn_empresa-fim = tt-param.i-cdn_empresa-fim
       l-log_avpes_finaliz-ini = tt-param.l-log_avpes_finaliz-ini
       l-log_avpes_finaliz-fim = tt-param.l-log_avpes_finaliz-fim
.

def new global shared var v_cod_arq_gerdoc  as char no-undo.



run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

DEFINE VARIABLE chExcelApplication AS COM-HANDLE.
DEFINE VARIABLE ch-WorkSheet       AS COM-HANDLE                            NO-UNDO.
DEF VAR i-linha AS INT NO-UNDO.


/* gr9020h.p */
output stream str-rp close.
CREATE "Excel.Application" chExcelApplication.   
chexcelapplication:workbooks:add().              
ch-WorkSheet = chExcelApplication:Sheets:ITEM(1).
chexcelapplication:sheets:ITEM(1):NAME = "aval02bb".

DEF VAR c-desc-meta LIKE avpes_meta_indual.des_meta_indual NO-UNDO.
for each rh_pessoa_fisic no-lock,
    each avpes_emitid no-lock
         where avpes_emitid.num_pessoa_fisic = rh_pessoa_fisic.num_pessoa_fisic and
               avpes_emitid.cdn_empresa >= i-cdn_empresa-ini and 
               avpes_emitid.cdn_empresa <= i-cdn_empresa-fim and
               avpes_emitid.dat_emis_avpes >= da-dat_emis_avpes-ini and 
               avpes_emitid.dat_emis_avpes <= da-dat_emis_avpes-fim and
               avpes_emitid.dat_final_period_avpes >= da-dat_final_period_avpes-ini and 
               avpes_emitid.dat_final_period_avpes <= da-dat_final_period_avpes-fim and
               avpes_emitid.log_avpes_finaliz >= l-log_avpes_finaliz-ini and 
               avpes_emitid.log_avpes_finaliz <= l-log_avpes_finaliz-fim and
               avpes_emitid.num_pessoa_fisic_avaldor >= i-num_pessoa_fisic_avaldor-ini and 
               avpes_emitid.num_pessoa_fisic_avaldor <= i-num_pessoa_fisic_avaldor-fim,
    each avpes_reg_mestre NO-LOCK OF avpes_emitid,
    each avpes_grp NO-LOCK OF avpes_reg_mestre
         where avpes_emitid.num_avpes_emitid  >= num_avpes_emitid,
    EACH avpes_grp_item 
         WHERE avpes_grp_item.cdn_grp_avpes = avpes_grp.cdn_grp_avpes NO-LOCK,
    each avpes_meta_indual NO-LOCK 
         where avpes_meta_indual.num_pessoa_fisic = rh_pessoa_fisic.num_pessoa_fisic and
               avpes_meta_indual.cdn_item_avpes = avpes_grp_item.cdn_item_avpes and
               avpes_meta_indual.dat_previs_conclus_meta >= da-dat_previs_conclus_meta-ini and 
               avpes_meta_indual.dat_previs_conclus_meta <= da-dat_previs_conclus_meta-fim:

    FIND FIRST funcionario no-lock
         where funcionario.cdn_empresa = avpes_emitid.cdn_empresa and
               /*funcionario.num_pessoa_fisic = rh_pessoa_fisic.num_pessoa_fisic AND
               funcionario.dat_desligto_func = ? AND*/
               funcionario.cdn_estab = avpes_emitid.cdn_estab and
               funcionario.cdn_funcionario = avpes_emitid.cdn_funcionario AND
               funcionario.cod_rh_ccusto >= c-cod_rh_ccusto-ini and 
               funcionario.cod_rh_ccusto <= c-cod_rh_ccusto-fim NO-ERROR.
    IF NOT AVAIL funcionario THEN NEXT.


    FIND FIRST rh_ccusto no-lock
         where rh_ccusto.cdn_empresa = funcionario.cdn_empresa and
               rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto NO-ERROR.
    IF NOT AVAIL rh_ccusto THEN NEXT.

    FIND FIRST cargo_basic no-lock
         where cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic NO-ERROR.
    IF NOT AVAIL cargo_basic THEN NEXT.
    
    assign v-num-reg-lidos = v-num-reg-lidos + 1
           i-linha = v-num-reg-lidos.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
    if  v-num-reg-lidos = 1 then do:
        ASSIGN chexcelapplication:Range("A"  + STRING(i-linha)):VALUE = "Avaliador (Cod.P.F¡sica)"
               Chexcelapplication:Range("B"  + STRING(i-linha)):VALUE = "Nome Avaliador"
               chexcelapplication:Range("C"  + STRING(i-linha)):VALUE = "Num Aval"
               chexcelapplication:Range("D"  + STRING(i-linha)):VALUE = "Cod. Avalia‡Æo"
               chexcelapplication:Range("E"  + STRING(i-linha)):VALUE = "Descri‡Æo Avalia‡Æo"

               chexcelapplication:Range("F"  + STRING(i-linha)):VALUE = "Avaliado_cod.PF¡sica"
               chexcelapplication:Range("G"  + STRING(i-linha)):VALUE = "Emp"
               Chexcelapplication:Range("H"  + STRING(i-linha)):VALUE = "Est"
               chexcelapplication:Range("I"  + STRING(i-linha)):VALUE = "Avaliado_Cod.Matr¡cula"
               chexcelapplication:Range("J"  + STRING(i-linha)):VALUE = "Nome do Avaliado"
               chexcelapplication:Range("K"  + STRING(i-linha)):VALUE = "CPF"
               chexcelapplication:Range("L"  + STRING(i-linha)):VALUE = "Meta Individual"
               
               chexcelapplication:Range("M"  + STRING(i-linha)):VALUE = "ConclusÆo"
               chexcelapplication:Range("N"  + STRING(i-linha)):VALUE = "Resultado"
               chexcelapplication:Range("O"  + STRING(i-linha)):VALUE = "Fim Periodo"

               Chexcelapplication:Range("P"  + STRING(i-linha)):VALUE = "C Custo"
               chexcelapplication:Range("Q"  + STRING(i-linha)):VALUE = "Descri‡Æo Ccusto"

               chexcelapplication:Range("R"  + STRING(i-linha)):VALUE = "Cargo"
               Chexcelapplication:Range("S"  + STRING(i-linha)):VALUE = "Desc Cargo".

        assign v-num-reg-lidos = v-num-reg-lidos + 1
               i-linha = v-num-reg-lidos.
    END.

    
   ASSIGN c-desc-meta = avpes_meta_indual.des_meta_indual
          c-desc-meta = REPLACE(c-desc-meta, CHR(10), " ")
          c-desc-meta = REPLACE(c-desc-meta, CHR(13), " ")
          c-desc-meta = REPLACE(c-desc-meta, CHR(44), "-").

   FIND b-rh_pessoa_fisic WHERE b-rh_pessoa_fisic.num_pessoa_fisic = avpes_emitid.num_pessoa_fisic_avaldor NO-ERROR. 

   ASSIGN chexcelapplication:Range("A" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("B" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("C" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("D" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("E" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("F" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("G" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("H" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("I" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("J" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("K" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("L" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("M" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("N" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("O" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("P" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("Q" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("R" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("S" + STRING(i-linha)):numberformat = "@".

          chexcelapplication:Range("A"  + STRING(i-linha)):VALUE = avpes_emitid.num_pessoa_fisic_avaldor.
          Chexcelapplication:Range("B"  + STRING(i-linha)):VALUE = IF AVAIL  b-rh_pessoa_fisic THEN b-rh_pessoa_fisic.nom_pessoa_fisic ELSE ""    .
          chexcelapplication:Range("C"  + STRING(i-linha)):VALUE = avpes_emitid.num_avpes_emitid        .
          chexcelapplication:Range("D"  + STRING(i-linha)):VALUE = avpes_emitid.cdn_avpes_padr          .
          chexcelapplication:Range("E"  + STRING(i-linha)):VALUE = avpes_reg_mestre.des_avpes_padr      .

          chexcelapplication:Range("F"  + STRING(i-linha)):VALUE = avpes_emitid.num_pessoa_fisic        .
          chexcelapplication:Range("G"  + STRING(i-linha)):VALUE = avpes_emitid.cdn_empresa             .
          Chexcelapplication:Range("H"  + STRING(i-linha)):VALUE = avpes_emitid.cdn_estab               .
          chexcelapplication:Range("I"  + STRING(i-linha)):VALUE = funcionario.cdn_funcionario    .          
          chexcelapplication:Range("J"  + STRING(i-linha)):VALUE = rh_pessoa_fisic.nom_pessoa_fisic     .
          chexcelapplication:Range("K"  + STRING(i-linha)):VALUE = rh_pessoa_fisic.cod_id_feder         .
          chexcelapplication:Range("L"  + STRING(i-linha)):VALUE = c-desc-meta                                .

          chexcelapplication:Range("M" + STRING(i-linha)):numberformat = "@".
          chexcelapplication:Range("M"  + STRING(i-linha)):VALUE = string(avpes_meta_indual.dat_conclus,"99/99/9999")   .
          chexcelapplication:Range("N"  + STRING(i-linha)):VALUE = avpes_meta_indual.des_restdo         .
          chexcelapplication:Range("O"  + STRING(i-linha)):VALUE = avpes_emitid.dat_final_period_avpes  .

          Chexcelapplication:Range("P"  + STRING(i-linha)):VALUE = funcionario.cod_rh_ccusto            .
          chexcelapplication:Range("Q"  + STRING(i-linha)):VALUE = rh_ccusto.des_rh_ccusto              .
          chexcelapplication:Range("R"  + STRING(i-linha)):VALUE = funcionario.cdn_cargo_basic          .
          Chexcelapplication:Range("S"  + STRING(i-linha)):VALUE = cargo_basic.des_cargo_basic.         .
   
end.

chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
chExcelApplication:ActiveWorkbook:SaveAs(tt-param.arquivo,,,,,,).
chExcelApplication:QUIT().   
RELEASE OBJECT chExcelApplication NO-ERROR.






if opsys <> "win32"
then
return translate("Este programa deve ser executado em sistema operacional Windows").

if v-num-reg-lidos <> 0 
then do:
    if  session:set-wait-state("general") then.
    RUN pi-abre-excel.

    /*
    if  session:set-wait-state("general") then.
    run pi-cria-tabela-dinamica-excel. /* GIRON*/*/
end.
else do:
    if   i-num-ped-exec-rpw = 0 then 
         message translate("NÆo foram gerados dados suficientes para a cria‡Æo do cen rio dinƒmico.") view-as alert-box information. 
    else return translate("NÆo foram gerados dados suficientes para a cria‡Æo do cen rio dinƒmico.").
end.

PROCEDURE pi-abre-excel:

    DEF VAR v-ch-Excel         AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Workbook      AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotCache    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotTable    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Publish       AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-1             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-2             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-3             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-4             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-5             AS COM-HANDLE NO-UNDO.
    DEF VAR v-titulo           as char format 'X(60)'.
    DEF VAR i-cont             as int.
    DEF VAR l-error            as log.

    assign v-titulo = "Avalia‡äes HCM".

    assign v-num-reg-lidos = v-num-reg-lidos + 1. /* Incrementa em 1 para linha de labels*/

    IF NOT VALID-HANDLE(v-ch-Excel)
    then do:
        CREATE "Excel.Application " v-ch-Excel CONNECT NO-ERROR.
        IF ERROR-STATUS:ERROR THEN CREATE "Excel.Application" v-ch-Excel.
    end.

    v-ch-Excel:DisplayAlerts = FALSE.
    v-ch-Excel:VISIBLE = NO.

    if i-num-ped-exec-rpw = 0
        then v-ch-Excel:Workbooks:Open(replace(tt-param.arquivo,"/","\")).
        else v-ch-Excel:Workbooks:Open(replace(c-dir-spool-servid-exec,"/","\") + "\" + tt-param.arquivo).
    v-ch-Excel:Sheets(1):NAME = translate("Dados").
    v-ch-Workbook = v-ch-Excel:ActiveWorkbook.

    v-ch-Excel:COLUMNS("A:S"):EntireColumn:AUTOFIT().

    if tt-param.i-ferra-abre = 2 /* Microsoft Internet Explorer */
    then do:
        if i-num-ped-exec-rpw <> 0
        then do:
            if v-ch-Workbook:ActiveSheet:NAME = translate("Cen rio")
            then v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(6,replace(c-dir-spool-servid-exec,"/","\") + "\" + ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME, v-titulo,2) no-error.
            else v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(5,replace(c-dir-spool-servid-exec,"/","\") + "\" + ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME,"":U,3) no-error.
            v-ch-Publish:PUBLISH(TRUE).
        end.
        else do:
            if v-ch-Workbook:ActiveSheet:NAME = translate("Cen rio")
            then v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(6,ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME, v-titulo ,2) no-error.
            else v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(5,ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME,"":U,3) no-error.
            v-ch-Publish:PUBLISH(TRUE).
            if  search(ENTRY(1,tt-param.arquivo,".") + ".html") <> ?
            then do:
                file-info:file-name = search(ENTRY(1,tt-param.arquivo,".") + ".html").
                run pi-gera-marcadagua-html(input file-info:file-name).
                if  tt-param.destino = 3
                then
                    run OpenDocument (input file-info:full-pathname) no-error.
                else message translate("Arquivos gerados com sucesso.") view-as alert-box information.
            end.
        end.
    end.

    if session:set-wait-state("") then.

    if tt-param.i-ferra-abre = 1 /* Microsoft Excel */
    then do:
        if i-num-ped-exec-rpw = 0
        then do:
            v-ch-excel:ActiveWorkbook:SaveAs(replace(tt-param.arquivo,".xls", ".cvs"),1,"","",no,no,no) no-error.
            v-ch-excel:quit().
            if  tt-param.destino = 3
            then do:
                file-info:file-name = search(replace(tt-param.arquivo,".xls", ".cvs")).
                run OpenDocument (input file-info:full-pathname) no-error.
            end.
            else message translate("Arquivos gerados com sucesso.") view-as alert-box information.
        end.
        else
            v-ch-excel:ActiveWorkbook:SaveAs(replace(c-dir-spool-servid-exec,"/","\") + "\" , replace(tt-param.arquivo,".xls", ".cvs"),1,"","",no,no,no) no-error.
    end.

    ASSIGN v_cod_arq_gerdoc = ( IF c-dir-spool-servid-exec <> ""
                                THEN (REPLACE(c-dir-spool-servid-exec,"/","\") + "\" + replace(tt-param.arquivo,".cvs":U,".xls":U))
                                ELSE replace(tt-param.arquivo,".cvs",".xls":U)).


    RELEASE OBJECT v-ch-1 no-error.
    RELEASE OBJECT v-ch-2 no-error.
    RELEASE OBJECT v-ch-3 no-error.
    RELEASE OBJECT v-ch-4 no-error.
    RELEASE OBJECT v-ch-5 no-error.
    RELEASE OBJECT v-ch-Publish  no-error.
    RELEASE OBJECT v-ch-Workbook no-error.
    if tt-param.i-ferra-abre = 2 or /* Microsoft Internet Explorer */
       i-num-ped-exec-rpw   <> 0
    then v-ch-excel:quit().
    RELEASE OBJECT v-ch-Excel no-error.

END.
Procedure pi-cria-tabela-dinamica-excel:

    DEF VAR v-ch-Excel         AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Workbook      AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotCache    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotTable    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Publish       AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-1             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-2             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-3             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-4             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-5             AS COM-HANDLE NO-UNDO.
    DEF VAR v-titulo           as char format 'X(60)'.
    DEF VAR i-cont             as int.
    DEF VAR l-error            as log.

    assign v-titulo = "Avalia‡äes HCM".

    assign v-num-reg-lidos = v-num-reg-lidos + 1. /* Incrementa em 1 para linha de labels*/

    IF NOT VALID-HANDLE(v-ch-Excel)
    then do:
        CREATE "Excel.Application " v-ch-Excel CONNECT NO-ERROR.
        IF ERROR-STATUS:ERROR THEN CREATE "Excel.Application" v-ch-Excel.
    end.

    v-ch-Excel:DisplayAlerts = FALSE.
    v-ch-Excel:VISIBLE = NO.

    if i-num-ped-exec-rpw = 0
        then v-ch-Excel:Workbooks:Open(replace(tt-param.arquivo,"/","\")).
        else v-ch-Excel:Workbooks:Open(replace(c-dir-spool-servid-exec,"/","\") + "\" + tt-param.arquivo).
    v-ch-Excel:Sheets(1):NAME = translate("Dados").
    v-ch-Workbook = v-ch-Excel:ActiveWorkbook.

    
    v-ch-Excel:ActiveSheet:PivotTableWizard(1,translate("Dados") + "!A1:S" + string(v-num-reg-lidos),"", v-titulo). /*ERA P - GIRON*/
    v-ch-Workbook:ActiveSheet:Cells(1, 1):SELECT.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):ColumnGrand = FALSE.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):RowGrand = FALSE.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliador (Cod.P.F¡sica)")):ORIENTATION = 3 no-error.
    /*GIRON*/

    
    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Avaliador (Cod.P.F¡sica) na tabela dinƒmica, pois o mesmo provocou um estouro no limite de p ginas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Avaliador (Cod.P.F¡sica) selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliador (Cod.P.F¡sica)")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliador (Cod.P.F¡sica)")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliador (Cod.P.F¡sica)")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliador (Cod.P.F¡sica)")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliador (Cod.P.F¡sica)")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliador (Cod.P.F¡sica)")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Emp")):ORIENTATION = 3 no-error.
    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Emp na tabela dinƒmica, pois o mesmo provocou um estouro no limite de p ginas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Emp selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    
    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Emp")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Emp")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Emp")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Emp")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Emp")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Emp")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cod. Avalia‡Æo")):ORIENTATION = 3 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Cod. Avalia‡Æo na tabela dinƒmica, pois o mesmo provocou um estouro no limite de p ginas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Cod. Avalia‡Æo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cod. Avalia‡Æo")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cod. Avalia‡Æo")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cod. Avalia‡Æo")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cod. Avalia‡Æo")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cod. Avalia‡Æo")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cod. Avalia‡Æo")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Est")):ORIENTATION = 3 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Est na tabela dinƒmica, pois o mesmo provocou um estouro no limite de p ginas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Est selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Est")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Est")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Est")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Est")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Est")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Est")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo Avalia‡Æo")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Descri‡Æo Avalia‡Æo na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Descri‡Æo Avalia‡Æo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo Avalia‡Æo")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo Avalia‡Æo")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo Avalia‡Æo")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo Avalia‡Æo")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo Avalia‡Æo")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo Avalia‡Æo")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Num Aval")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Num Aval na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Num Aval selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Num Aval")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Num Aval")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Num Aval")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Num Aval")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Num Aval")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Num Aval")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_Cod.Matr¡cula")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Avaliado_Cod.Matr¡cula na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Avaliado_Cod.Matr¡cula selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_Cod.Matr¡cula")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_Cod.Matr¡cula")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_Cod.Matr¡cula")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_Cod.Matr¡cula")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_Cod.Matr¡cula")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_Cod.Matr¡cula")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_cod.PF¡sica")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Avaliado_cod.PF¡sica na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Avaliado_cod.PF¡sica selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_cod.PF¡sica")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_cod.PF¡sica")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_cod.PF¡sica")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_cod.PF¡sica")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_cod.PF¡sica")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Avaliado_cod.PF¡sica")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome do Avaliado")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Nome do Avaliado na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Nome do Avaliado selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome do Avaliado")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome do Avaliado")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome do Avaliado")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome do Avaliado")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome do Avaliado")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome do Avaliado")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("CPF")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo CPF na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo CPF selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("CPF")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("CPF")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("CPF")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("CPF")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("CPF")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("CPF")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Meta Individual")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Meta Individual na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Meta Individual selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Meta Individual")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Meta Individual")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Meta Individual")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Meta Individual")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Meta Individual")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Meta Individual")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("ConclusÆo")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo ConclusÆo na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo ConclusÆo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("ConclusÆo")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("ConclusÆo")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("ConclusÆo")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("ConclusÆo")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("ConclusÆo")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("ConclusÆo")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Resultado")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Resultado na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Resultado selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Resultado")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Resultado")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Resultado")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Resultado")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Resultado")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Resultado")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo CCusto")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Descri‡Æo CCusto na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Descri‡Æo CCusto selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo CCusto")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo CCusto")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo CCusto")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo CCusto")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo CCusto")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo CCusto")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C Custo")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo C Custo na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo C Custo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C Custo")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C Custo")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C Custo")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C Custo")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C Custo")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C Custo")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cargo")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Cargo na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Cargo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cargo")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cargo")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cargo")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cargo")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cargo")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Cargo")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Desc Cargo")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Desc Cargo na tabela dinƒmica, pois o mesmo provocou um estouro no limite de linhas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Desc Cargo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Desc Cargo")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Desc Cargo")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Desc Cargo")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Desc Cargo")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Desc Cargo")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Desc Cargo")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome Avaliador")):ORIENTATION = 3 no-error.
    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss¡vel inserir o campo Nome Avaliador na tabela dinƒmica, pois o mesmo provocou um estouro no limite de p ginas da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Nome Avaliador selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    
    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome Avaliador")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome Avaliador")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome Avaliador")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome Avaliador")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome Avaliador")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Nome Avaliador")):Subtotals(06) = no.

    END. 

    
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fim Per¡odo")):ORIENTATION = 4 no-error.

    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          MESSAGE "NÆo foi poss¡vel inserir o campo Fim Per¡odo na tabela dinƒmica, pois o mesmo provocou um estouro no limite de dados da tabela dinƒmica do Excel." skip
                  "Pelo motivo descrito acima a tabela dinƒmica ser  gerada sem o campo Fim Per¡odo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.


    IF index(v-ch-Excel:Version, "9.") = 0 THEN /* Tem Office >= XP */ 
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Contar de " + translate("Fim Per¡odo") + ""):NAME = translate("Conta Fim Per¡odo") no-error. 
ELSE /* Tem Office 2000 */
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Contagem de " + translate("Fim Per¡odo") + ""):NAME = translate("Conta Fim Per¡odo") no-error. 
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Count of " + translate("Fim Per¡odo") + ""):NAME = translate("Conta Fim Per¡odo") no-error.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Conta Fim Per¡odo")):FUNCTION = 1 no-error.

    v-ch-Workbook:ActiveSheet:NAME = translate("Cen rio").


    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):DisplayImmediateItems = True.

    if index(v-ch-Excel:Version, "8.") = 0 and /* Tem Office 2000 */
       integer(v-cod-tipo-grafico-dv203) <> 0 /* Nenhum */
    then do:
        v-ch-Workbook:Charts:ADD().
        v-ch-Workbook:ActiveChart:ChartType = integer(v-cod-tipo-grafico-dv203).
        v-ch-Workbook:ActiveSheet:NAME = translate("Gr fico").
    end.

    if tt-param.i-ferra-abre = 1
    then do:
        v-ch-Excel:Sheets:Add().
        v-ch-Workbook:ActiveSheet:NAME = "Data Viewer 3.00".

        v-ch-Workbook:ActiveSheet:Range("A1:P34"):Interior:ColorIndex = 15.
        v-ch-Workbook:ActiveSheet:Range("D6:D17"):Interior:ColorIndex = 5.
        v-ch-Workbook:ActiveSheet:Range("E6:I17"):Interior:ColorIndex = 2.

        ASSIGN v-ch-Workbook:ActiveSheet:Cells(8, "E"):Value = "Datasul"
               v-ch-Workbook:ActiveSheet:Cells(8, "E"):Font:Name = "Arial".

        ASSIGN v-ch-Workbook:ActiveSheet:Cells(9, "E"):Value = "Data Viewer 3.00"
               v-ch-Workbook:ActiveSheet:Cells(9, "E"):Font:Name = "Arial"
               v-ch-Workbook:ActiveSheet:Cells(9, "E"):Font:Size = 25
               v-ch-Workbook:ActiveSheet:Cells(9, "E"):Font:Bold = YES
               v-ch-Workbook:ActiveSheet:Cells(9, "E"):Font:Italic = YES.

        ASSIGN v-ch-Workbook:ActiveSheet:Cells(10, "G"):Value = "Exploring Databases"
               v-ch-Workbook:ActiveSheet:Cells(10, "G"):Font:Name = "Arial".

    ASSIGN v-ch-1 = v-ch-Workbook:ActiveSheet:Range("E12").
    ASSIGN v-ch-2 = v-ch-Workbook:ActiveSheet:Range("E13").
    ASSIGN v-ch-3 = v-ch-Workbook:ActiveSheet:Range("E14").
    ASSIGN v-ch-4 = v-ch-Workbook:ActiveSheet:Range("E15").
    if index(v-ch-Excel:Version, "8.") = 0 
    then do:
        v-ch-Workbook:ActiveSheet:Hyperlinks:ADD(V-CH-1,"mailto:marketing_tecnologia@datasul.com.br", , , translate("E-mail Marketing Tecnologia")).
        v-ch-Workbook:ActiveSheet:Hyperlinks:ADD(V-CH-2,"http://www.datasul.com.br", , , translate("Site Oficial Datasul")).
        v-ch-Workbook:ActiveSheet:Hyperlinks:ADD(V-CH-3,"http://www.datasuldirect.com.br", , , translate("Datasul Direct")).
        v-ch-Workbook:ActiveSheet:Hyperlinks:ADD(V-CH-4,"http://www.aspdatasul.com.br/Acesso/Demo.php", , , translate("Acesso Demo ASP Datasul")).
    end.

        v-ch-Excel:Sheets("Data Viewer 3.00"):Select.
    assign v-ch-5 = v-ch-Excel:Sheets(translate("Dados")).
    v-ch-Excel:Sheets("Data Viewer 3.00"):MOVE(,v-ch-5).
        v-ch-Excel:Sheets(translate("Cen rio")):SELECT.
    end.
    
    /* C¢digo movido para procedure pi-abre-excel */

End procedure.

def stream str-origem.
def stream str-destino.
Procedure pi-gera-marcadagua-html:

    def input param p-cod-arq-origem as char.

    def var v-mpt  as memptr no-undo.
    def var i-cont as integer no-undo.
    def var v-chr  as char no-undo.
    def var v-tag  as char no-undo.
    def var l-tag  as logical no-undo.
    def var v-cod-linha-process as char no-undo.

    FILE-INFO:FILE-NAME = p-cod-arq-origem.
    SET-SIZE(v-mpt) = FILE-INFO:FILE-SIZE.
    input stream str-origem from value(p-cod-arq-origem) binary no-convert.
    import stream str-origem v-mpt.
    INPUT  STREAM str-origem  CLOSE.

    output stream str-destino to value(session:temp-directory + "cenario.tmp") CONVERT TARGET 'iso8859-1'.

    DO  i-cont = 1 TO FILE-INFO:FILE-SIZE:

        assign v-chr = chr(get-byte(v-mpt,i-cont)).

        IF v-chr = "<":U THEN DO: assign l-Tag = YES. END.

        IF l-Tag THEN assign v-Tag = v-Tag + v-chr.
        ELSE put stream str-destino unformatted v-chr.

        IF v-Tag = "<body>":U THEN DO:
            assign l-Tag = NO. put stream str-destino unformatted v-Tag.
            put stream str-destino unformatted "<a name=" chr(34) "dv300inicio" chr(34) "></a>" skip "<a href=" chr(34) "#dv300" chr(34) "class=nav><p class=MsoNormal align=right style='text-align:right'><font size=" chr(34) 1 chr(34) "face=" chr(34) "Arial" chr(34) ">Data Viewer 3.00</font></p></a>" skip.
        end.

        IF v-Tag = "</body>":U THEN DO:
            put stream str-destino unformatted "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip.
            put stream str-destino unformatted "<a name=" chr(34) "dv300" chr(34) "></a>" skip.
            put stream str-destino unformatted "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip.
            put stream str-destino unformatted "<center>" skip.
            put stream str-destino unformatted "<table border=" chr(34) 0 chr(34) "width=" chr(34) 348 chr(34) "cellspacing=" chr(34) 1 chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "bordercolordark=" chr(34) "#FFFFFF" chr(34) ">" skip.
            put stream str-destino unformatted "  <tr>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 57 chr(34) "bgcolor=" chr(34) "#0000FF" chr(34) "rowspan=" chr(34) 7 chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) ">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </td>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 277 chr(34) "bgcolor=" chr(34) "#FFFFFF" chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "><font size=" chr(34) 2 chr(34) "face=" chr(34) "Arial" chr(34) ">&nbsp;&nbsp;Datasul</font></td>" skip.
            put stream str-destino unformatted "  </tr>" skip.
            put stream str-destino unformatted "  <tr>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 277 chr(34) "bgcolor=" chr(34) "#FFFFFF" chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "><b><i><font size=" chr(34) "6" chr(34) "face=" chr(34) "Arial" chr(34) ">&nbsp;Data Viewer 3.00</font></i></b></td>" skip.
            put stream str-destino unformatted "  </tr>" skip.
            put stream str-destino unformatted "  <tr>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 277 chr(34) "align=" chr(34) "right" chr(34) "bgcolor=" chr(34) "#FFFFFF" chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "><font face=" chr(34) "Arial" chr(34) "size="chr(34) 2 chr(34) ">Exploring Databases</font><p>&nbsp;</td>" skip.
            put stream str-destino unformatted "  </tr>" skip.
            put stream str-destino unformatted "  <tr>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 277 chr(34) "bgcolor=" chr(34) "#FFFFFF" chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "><font color=" chr(34) "#FFFFFF" chr(34) "><font face=" chr(34) "Arial" chr(34) "size="chr(34) 2 chr(34) ">&nbsp;&nbsp;<a href=" chr(34) "mailto:marketing_tecnologia@datasul.com.br" chr(34) ">" translate("E-mail Marketing Tecnologia") "</a></font></td>" skip.
            put stream str-destino unformatted "  </tr>" skip.
            put stream str-destino unformatted "  <tr>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 277 chr(34) "bgcolor=" chr(34) "#FFFFFF" chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "><font color=" chr(34) "#FFFFFF" chr(34) "><font face=" chr(34) "Arial" chr(34) "size="chr(34) 2 chr(34) ">&nbsp;&nbsp;<a href=" chr(34) "http://www.datasul.com.br" chr(34) ">" translate("Site Oficial Datasul") "</a></font></td>" skip.
            put stream str-destino unformatted "  </tr>" skip.
            put stream str-destino unformatted "  <tr>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 277 chr(34) "bgcolor=" chr(34) "#FFFFFF" chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "><font color=" chr(34) "#FFFFFF" chr(34) "><font face=" chr(34) "Arial" chr(34) "size="chr(34) 2 chr(34) ">&nbsp;&nbsp;<a href=" chr(34) "http://www.datasuldirect.com.br" chr(34) ">" translate("Datasul Direct") "</a><o:p></o:p></font></span></td>" skip.
            put stream str-destino unformatted "  </tr>" skip.
            put stream str-destino unformatted "  <tr>" skip.
            put stream str-destino unformatted "    <td width=" chr(34) 277 chr(34) "bgcolor=" chr(34) "#FFFFFF" chr(34) "bordercolor=" chr(34) "#FFFFFF" chr(34) "bordercolorlight=" chr(34) "#FFFFFF" chr(34) "><font color=" chr(34) "#FFFFFF" chr(34) "><font face=" chr(34) "Arial" chr(34) "size="chr(34) 2 chr(34) ">&nbsp;&nbsp;<a href=" chr(34) "http://www.aspdatasul.com.br/Acesso/Demo.php" chr(34) ">" translate("Acesso Demo ASP Datasul") "</a><o:p></o:p></font></span></td>" skip.
            put stream str-destino unformatted "  </tr>" skip.
            put stream str-destino unformatted "</table>" skip.
            put stream str-destino unformatted "</center>" skip.
            put stream str-destino unformatted "<a href=" chr(34) "#dv300inicio" chr(34) "class=nav><p class=MsoNormal align=center style='text-align:center'><font size=" chr(34) 1 chr(34) "face=" chr(34) "Arial" chr(34) ">" translate("Voltar") "</font></p></a>" skip.
            put stream str-destino unformatted "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip "<p>&nbsp;</p>" skip.
            put stream str-destino unformatted v-Tag.
            assign l-Tag = NO 
                   v-Tag = "".
        end.

        IF v-Tag = "<param":U THEN DO:
            assign l-Tag = NO.
            put stream str-destino unformatted v-Tag.
            assign v-Tag = "".
        end.

        IF v-chr = ">":U THEN DO:
            IF l-Tag = YES THEN DO: put stream str-destino unformatted v-Tag. ASSIGN l-tag = NO. END.
            put stream str-destino unformatted SKIP.
            ASSIGN v-Tag = "".
        end.

    end.

    OUTPUT STREAM str-destino CLOSE.

    os-copy value(session:temp-directory + "cenario.tmp") value(p-cod-arq-origem).
    os-delete value(session:temp-directory + "cenario.tmp").

End Procedure.

FUNCTION translate RETURNS CHARACTER (str AS CHAR):
    RETURN str.
END FUNCTION.


if connected("dthrpyc") then
  delete procedure v_han_fpapi003.

    output stream str-rp close.

Procedure OpenDocument:
    def input param c-doc as char no-undo.
    def var c-exec        as char no-undo.
    def var h-Inst        as int  no-undo.
    def var c-arq         as char no-undo.

    run ConverteparaNomeDos (input-output c-doc).

    assign c-exec = fill("x",255).

    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if  h-inst >= 0 and h-inst <= 32
    then do:
        message translate("NÆo foi encontrada associa‡Æo do arquivo (.htm)") skip translate("com nenhum software de visualiza‡Æo. Deseja associar agora?") view-as alert-box question buttons yes-no title translate("Associar arquivo") UPDATE l-associar AS LOGICAL.
        if  l-associar = yes
        then
            run ShellExecuteA (input 0,
                               input "open",
                               input "rundll32.exe",
                               input "shell32.dll,OpenAs_RunDLL "+ c-doc,
                               input "",
                               input 1,
                               output h-inst).

        else
            return.
    end.

    assign c-arq = "'" + string(c-exec) + " " + string(c-doc).
    assign c-arq = replace(c-arq,"'","").

    run WinExec (input c-arq,
                 input 1,
                 output h-inst).

    if  h-inst < 0 or
        h-inst > 32
    then
        return "OK".
    else
    return "NOK".

END PROCEDURE.

PROCEDURE ConverteparaNomeDos:
    def input-output param c-Nome as char no-undo.
    def var iLen   as int  init 255 no-undo.
    def var pShort as memptr.

    repeat:
        set-size(pShort) = iLen.
        run GetShortPathNameA (c-Nome,
                               get-pointer-value(pShort),
                               get-size(pShort),
                               output iLen).
        if get-size(pShort) >= iLen then leave.
        set-size(pShort) = 0.
    end.
    c-Nome = get-string(pShort,1).
END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "shell32.dll":
    define input parameter lpFile as char.
    define input parameter lpDirectory as char.
    define input-output parameter lpResult as char.
    define return parameter hInstance as long.
END PROCEDURE.

PROCEDURE ShellExecuteA EXTERNAL "shell32.dll":
    define input parameter hwnd as long.
    define input parameter lpOperation as char.
    define input parameter lpFile as char.
    define input parameter lpParameters as char.
    define input parameter lpDirectory as char.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.
END PROCEDURE.

PROCEDURE GetShortPathNameA EXTERNAL "KERNEL32":
    DEF INPUT  PARAM lpszLongPath  AS CHAR NO-UNDO.
    DEF INPUT  PARAM lpszShortPath AS LONG NO-UNDO.
    DEF INPUT  PARAM cchBuffer     AS LONG NO-UNDO.
    DEF RETURN PARAM lenBuffer     AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE WinExec EXTERNAL "KERNEL32":
    define input parameter lpszCmdLine as char.
    define input parameter fuCmdShow as LONG.
    define return parameter nTask as LONG.
END PROCEDURE.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */

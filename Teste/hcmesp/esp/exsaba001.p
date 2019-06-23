/*****************************************************************************
**       Programa: EXSABA001.p
**       Data....: 29/07/13
**       Autor...: DATASUL S.A.
**       Objetivo: Export SABA
**       VersÆo..: 1.00.000 - adm
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "EXSABA001".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").
DEF VAR vdatacancel AS CHAR FORMAT 'X(10)' NO-UNDO.
DEF VAR vstatus AS CHAR FORMAT 999.

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

def var ID as character format "x(2)" init "".

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


assign c-programa     = "EXSABA001"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Export SABA"
       c-sistema      = "".


find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".


run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input yes).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
.

def var l-imprime as logical no-undo.

assign l-imprime = no.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.


    PUT STREAM str-rp
            'ID'                        '|'
            'NAME'                      '|'
            'DOMAIN'                    '|'
            'DESCRIPTION'               '|'
            'CUSTOM0'                   '|'
            'CUSTOM1'                   '|'
            'CUSTOM2'                   '|'
            'CUSTOM3'                   '|'
            'CUSTOM4'                   '|'
            'CUSTOM5'                   '|'
            'CUSTOM6'                   '|'
            'CUSTOM7'                   '|'
            'CUSTOM8'                   '|'
            'CUSTOM9'                   '|'
            'JOB_FAMILY'                '|'
            'START_DATE'                '|'
            'END_DATE'                  '|'
            'STATUS'                    '|'
            'IS_CRITICAL'               '|'
            'KEY_RESPONSIBILITIES'      '|'
            'LOCALE'                    '|'
            'CURRENCY'                  '|'
            'PAY_STRUCTURE'             '|'
            'PAY_GRADE'                 '|'
            'TRGT_BASE_COMP'            '|'
            'TRGT_VAR_COMP'             '|'
            'TRGT_TOT_ANL_COMP'         '|'
            'MRKT_ANL_COMP_MIN'         '|'
            'MRKT_ANL_COMP_MAX'         '|'
            'JOB_LEVEL'                 '|'
            'NEXT_CARR_STEP1'           '|'
            'NEXT_CARR_STEP2'           '|'
            'NEXT_CARR_STEP3'           '|'
            'EXCUSTOM1'                 '|'
            'EXCUSTOM2'                 '|'
            'EXCUSTOM3'                 '|'
            'JOB_ROLE1'                 '|'
            'ROLE_IS_REQUIRED1'         '|'
            'JOB_ROLE2'                 '|'
            'ROLE_IS_REQUIRED2'         
        SKIP.
    
FOR EACH cargo NO-LOCK.


    FIND FIRST niv_cargo WHERE niv_cargo.cdn_niv_cargo = cargo.cdn_niv_cargo NO-LOCK NO-ERROR.

    FIND FIRST cargo_basic_familia 
        WHERE cargo_basic_familia.cdn_cargo_basic = cargo.cdn_cargo_basic 
        NO-LOCK NO-ERROR.

        FOR EACH familia_jsl 
            WHERE familia_jsl.cdn_familia_jsl = cargo_basic_familia.cdn_familia_jsl 
            NO-LOCK:
            
            vdatacancel = ''.

            IF idi_avaliac_cargo <> 4 THEN DO:
                ASSIGN vdatacancel = '' 
                       vstatus = '100'.
                
            END.
            ELSE DO:
                ASSIGN vdatacancel = IF dat_cancel_cargo = ? THEN '' ELSE STRING(iso-date(dat_cancel_cargo)) 
                       vstatus = '200'.
            END.
    
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign l-imprime = yes.

    put stream str-rp unformatted 
                string(cargo.cdn_cargo_basic) + string(cargo.cdn_niv_cargo ) '|'    /*C?igo do cargo + c?igo do Nivel do Cargo*/
                string(cargo.cdn_cargo_basic) + string(cargo.cdn_niv_cargo ) '|'    /*Nome do cargo*/  /*Modificado dia 16/09 conforme solicitad‡Æo do Rafael campo anterior "cargo.cdn_cargo_basic"*/
                'COMMON' '|'                           /*Valor Default*/
                cargo.des_cargo '|'                   /*Descri?o do cargo*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                '' '|'                               /*N? Utilizado*/
                familia_jsl.des_familia_jsl '|'       /*Descri?o da Familia do Cargo*/
                iso-date(dat_descr_cargo) '|'         /*Data da cria‡Æo do cargo*/
                vdatacancel       '|'                 /*Data do Cancelamento do Cargo*/
                vstatus '|'                           /*Status do cargo 100-ativo /200-inativo */
                '' '|'                                /*Criticidade do Cargo*/
                '' '|'                                /*Key_Responsibilities*/
                'pt_BR' '|'                           /*Java Locale Brasil*/
                'crncy000000000000023' '|'            /*Currency ISO-CODE Moeda Real*/
                '' '|'                                /*PAY_STRUCTURE*/
                '' '|'                                /*PAY_GRADE*/       
                '' '|'                                /*TRGT_BASE_COMP*/
                '' '|'                                /*TRGT_VAR_COMP*/
                '' '|'                                /*TRGT_TOT_ANL_COMP*/
                '' '|'                                /*MRKT_ANL_COMP_MIN*/
                '' '|'                                /*MRKT_ANL_COMP_MAX*/
                cargo.cdn_niv_cargo '|'               /*JOB_LEVEL*/        
                '' '|'                                /*NEXT_CARR_STEP1*/
                '' '|'                                /*NEXT_CARR_STEP2*/
                '' '|'                                /*NEXT_CARR_STEP3*/
                '' '|'                                /*EXCUSTOM1*/        
                '' '|'                                /*EXCUSTOM2*/
                '' '|'                                /*EXCUSTOM3*/
                '' '|'                                /*JOB_ROLE1*/
                '' '|'                                /*ROLE_IS_REQUIRED1*/
                '' '|'                                /*JOB_ROLE2*/        
                ''                                    /*ROLE_IS_REQUIRED2*/

     
    skip.
    
end.

END.


if connected("dthrpyc") then
  delete procedure v_han_fpapi003.
    
    
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

/*****************************************************************************
**       Programa: exsaba002.p
**       Data....: 29/07/13
**       Autor...: DATASUL S.A.
**       Objetivo: Export SABA
**       VersÆo..: 1.00.000 - adm
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "EXSABA002".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").
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

DEF VAR vTitle AS CHAR FORMAT "x(4)".

DEFINE TEMP-TABLE tt-depara-sit
    FIELD tt-cdn-sit-afast AS INT FORMAT 'z9'
    FIELD tt-saba-sit AS CHAR FORMAT 'x(20)'
    INDEX tt-idx-afast IS PRIMARY tt-cdn-sit-afast ASCENDING. 

INPUT FROM 'd:\quarentena\hcm210esp\esp\erpxsaba.txt'.
    REPEAT:
        CREATE tt-depara-sit.
        IMPORT DELIMITER '|'  tt-depara-sit.
    END.
INPUT CLOSE.


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
DEF VAR vdataterminated AS CHAR FORMAT 'X(10)' NO-UNDO.
DEF VAR vdatanasc AS CHAR FORMAT 'X(10)' NO-UNDO.


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


assign c-programa     = "exsaba002"
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
                'ID'                            '|'
                'TITLE'                         '|'
                'USERNAME'                      '|'
                'FNAME'                         '|'
                'LNAME'                         '|'
                'MNAME'                         '|'
                'SUFFIX'                        '|'
                'SECURITY_DOMAIN'               '|'
                'STATUS'                        '|'
                'HOME_DOMAIN'                   '|'
                'GENDER'                        '|'
                'COMPANY'                       '|'
                'PERSON_NO'                     '|'
                'JOB_TYPE'                      '|'
                'SS_NO'                         '|'
                'MANAGER'                       '|'
                'JOB_TITLE'                     '|'
                'ADDITIONAL_APPROVER'           '|'
                'LOCATION'                      '|'
                'PERSON_TYPE'                   '|'
                'HIRED_ON'                      '|'
                'DATE_OF_BIRTH'                 '|'
                'ETHNICITY'                     '|'
                'RELIGION'                      '|'
                'TERMINATED_ON'                 '|'
                'DISCOUNT'                      '|'
                'IS_MANAGER'                    '|'
                'LOCALE'                        '|'
                'TIMEZONE'                      '|'
                'PASSWORD'                      '|'
                'HOMEPHONE'                     '|'
                'WORKPHONE'                     '|'
                'EMAIL'                         '|'
                'FAX'                           '|'
                'ADDR_1'                        '|'
                'ADDR_2'                        '|'
                'ADDR_3'                        '|'
                'CITY'                          '|'
                'STATE'                         '|'
                'ZIP'                           '|'
                'COUNTRY'                       '|'
                'DESIRED_JOB_TYPE'              '|'
                'CUSTOM0'                       '|'
                'CUSTOM1'                       '|'
                'CUSTOM2'                       '|'
                'CUSTOM3'                       '|'
                'CUSTOM4'                       '|'
                'CUSTOM5'                       '|'
                'CUSTOM6'                       '|'
                'CUSTOM7'                       '|'
                'CUSTOM8'                       '|'
                'CUSTOM9'                       '|'
                'JOB_LEVEL'                     '|'
                'SECRET_QUESTION'               '|'
                'SECRET_ANSWER'                 '|'
                'JOB_STARTED_ON'                '|'
                'RATE'                          '|'
                'RATE_CURRENCY'                 '|'
                'IS_INSTRUCTOR'                 '|'
                'EHRI_CODE'                     '|'
                'LOCAL_CURRENCY'                '|'
                'ANL_BASE_COMP'                 '|'
                'VAR_CASH_COMP'                 '|'
                'TRGT_BASE_COMP'                '|'
                'TRGT_VAR_COMP'                 '|'
                'TRGT_TOT_ANL_COMP'             '|'
                'PAY_STRUCTURE'                 '|'
                'PAY_GRADE'                     '|'
                'MRKT_PAY_GRD_MIN'              '|'
                'MRKT_PAY_GRD_MAX'              '|'
                'CORRESPONDENCE1'               '|'
                'CORRESPONDENCE2'               '|'
                'CORRESPONDENCE3'               '|'
                'OFFERING_LANGUAGE'             '|'
                'DELIVERY_TYPE'                 '|' 
                'AUDIENCE_TYPE1'                '|' 
                'AUDIENCE_TYPE2'                '|' 
                'SECURITY_ROLE1'                '|' 
                'DOMAIN1'                       '|' 
                'SECURITY_ROLE2'                '|' 
                'DOMAIN2'                       '|' 
                'ALTERNATE_MANAGER1'            '|' 
                'ALTERNATE_MANAGER2'            '|' 
                'APPROVER_TYPE1'                '|' 
                'APPROVER1'                     '|' 
                'APPROVER_TYPE2'                '|' 
                'APPROVER2'                     '|' 
                'IM_ALIAS_TYPE1'                '|' 
                'IM_ALIAS1'                     '|' 
                'IM_ALIAS_PREF1'                '|'
                'IM_ALIAS_TYPE2'                '|'
                'IM_ALIAS2'                     '|'
                'IM_ALIAS_PREF2'                '|'
                'EXCUSTOM1'                     '|'
                'EXCUSTOM2'                     '|'
                'EMAIL1'                        '|'
                'EMAIL2'                        
        SKIP.
    
FOR EACH hcm.empresa NO-LOCK:
    FOR EACH rh_estab WHERE rh_estab.cdn_empresa = hcm.empresa.ep-codigo NO-LOCK:
        FOR EACH funcionario WHERE funcionario.cdn_empresa = hcm.empresa.ep-codigo
                               AND funcionario.cdn_estab = rh_estab.cdn_estab 
                               NO-LOCK:
            FIND FIRST cargo_basic WHERE cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic NO-LOCK NO-ERROR.
             FIND FIRST cargo WHERE cargo.cdn_cargo_basic = cargo_basic.cdn_cargo_basic NO-LOCK NO-ERROR.
              FIND FIRST rh_pessoa_fisic WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-LOCK NO-ERROR.
                    FIND LAST sit_afast_func WHERE sit_afast_func.cdn_empresa = funcionario.cdn_empresa
                                           AND sit_afast_func.cdn_estab = funcionario.cdn_estab
                                           AND sit_afast_func.cdn_funcionario = funcionario.cdn_funcionario NO-LOCK NO-ERROR.
                        IF AVAIL sit_afast_func THEN
                            FIND FIRST tt-depara-sit WHERE tt-cdn-sit-afast = sit_afast_func.cdn_sit_afast_func NO-LOCK NO-ERROR.
                                IF AVAIL tt-depara-sit THEN

                    
                                     ASSIGN vTitle = 'Ms.'.
                            
                                     IF rh_pessoa_fisic.idi_sexo = 1 THEN 
                                         ASSIGN vTitle = 'Mrs.'.

                    
                                          vdataterminated = ''. /*TRATAMENTO DATA DE DEMISSÇO*/
                                    
                                          IF funcionario.dat_desligto_func = ? THEN DO:
                                              ASSIGN vdataterminated = '' .
                                          END.

                                          ELSE DO:
                                              ASSIGN vdataterminated = string(iso-date(funcionario.dat_desligto_func)).
                                          END.

                                          vdatanasc = ''. /*TRATAMENTO DATA DE ANIVERSARIO*/

                                          IF funcionario.dat_nascimento = ? THEN DO:
                                              ASSIGN vdatanasc = '' .
                                          END.

                                          ELSE DO:
                                              ASSIGN vdatanasc = string(iso-date(funcionario.dat_nascimento)).
                                          END.

                                           assign v-num-reg-lidos = v-num-reg-lidos + 1.
                                           run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
                                        
                                           assign l-imprime = yes.
                                        
                                           put stream str-rp unformatted 

                                              string(hcm.empresa.ep-codigo) + string(rh_estab.cdn_estab) + string(funcionario.cdn_funcionario)          '|'      /*ID*/
                                              vTitle                                                                                                       '|'      /*Title Mrs/Ms*/
                                              string(hcm.empresa.ep-codigo) + string(rh_estab.cdn_estab) + string(funcionario.cdn_funcionario)          '|'      /*Username*/
                                              ENTRY(1,rh_pessoa_fisic.nom_pessoa_fisic,"")                                                                 '|'      /*FName*/
                                              SUBSTRING(STRING(SUBSTRING(rh_pessoa_fisic.nom_pessoa_fisic,INDEX(rh_pessoa_fisic.nom_pessoa_fisic,' '),40)),2,40) FORMAT 'x(40)'    '|'      /*LName*/
                                              ''                                                                                                           '|'      /*MName*/
                                              ''                                                                                                           '|'      /*Suffix*/
                                              'brazil'                                                                                                     '|'      /*Security Domain*/
                                              tt-saba-sit                                                                                                  '|'      /*Status*/
                                              'brazil'                                                                                                     '|'      /*Home Domain*/
                                              IF rh_pessoa_fisic.idi_sexo = 1 THEN 0 ELSE 1                                                                '|'      /*Geder*/
                                              hcm.empresa.nome                                                                                          '|'      /*Company*/
                                              string(hcm.empresa.ep-codigo) + string(rh_estab.cdn_estab) + string(funcionario.cdn_funcionario)          '|'      /*Person_No*/
                                              cargo_basic.des_cargo_basic                                                                                  '|'      /*Job_Type*/
                                              ''                                                                                                           '|'      /*SS_NO*/
                                              ''                                                                                                           '|'      /*Manager*/
                                              cargo_basic.des_cargo_basic                                                                                  '|'      /*Job_Title*/
                                              ''                                                                                                           '|'      /*Additional_Approver*/       
                                              funcionario.cod_rh_ccusto                                                                                    '|'      /*Location*/
                                              ''                                                                                                           '|'      /*Person_type*/
                                              iso-date(funcionario.dat_admis_func)                                                                         '|'      /*Hired_on*/
                                              vdatanasc                                                                                                    '|'      /*Date_of_birth*/
                                              ''                                                                                                           '|'      /*Ethnicity*/
                                              ''                                                                                                           '|'      /*Religion*/
                                              vdataterminated                                                                                              '|'      /*terminated_on*/
                                              ''                                                                                                           '|'      /*discount*/
                                              IF cargo.cdn_niv_hier_funcnal = 3 OR cargo.cdn_niv_hier_funcnal = 4 THEN 'YES' ELSE 'NO'                     '|'      /*IS_Manager*/
                                              'pt_BR'                                                                                                      '|'      /*Locale*/
                                              'tzone000000000000017'                                                                                       '|'      /*Timezone*/
                                              ''                                                                                                           '|'      /*Password*/
                                              ''                                                                                                           '|'      /*HOmephone*/
                                              ''                                                                                                           '|'      /*workphone*/
                                              rh_pessoa_fisic.nom_e_mail                                                                                   '|'      /*email*/
                                              ''                                                                                                           '|'      /*FAX*/
                                              '' /*rh_pessoa_fisic.nom_ender_rh*/                                                                          '|'      /*ADDR_1*/
                                              ''                                                                                                           '|'      /*ADDR_2*/
                                              ''                                                                                                           '|'      /*ADDR_3*/
                                              '' /*rh_pessoa_fisic.nom_cidad_rh*/                                                                          '|'      /*City*/
                                              rh_pessoa_fisic.cod_unid_federac_rh                                                                          '|'       /*State*/
                                              ''    /*rh_pessoa_fisic.cod_cep_rh*/                                                                         '|'      /*ZIP*/
                                              rh_pessoa_fisic.cod_pais_ender                                                                               '|'      /*Country*/
                                              ''                                                                                                          '|'      /*DESIRED_JOB_TYPE*/
                                              ''                                                                                                          '|'      /*CUSTOM0*/
                                              ''                                                                                                          '|'      /*CUSTOM1*/
                                              ''                                                                                                          '|'      /*CUSTOM2*/
                                              ''                                                                                                          '|'      /*CUSTOM3*/
                                              ''                                                                                                          '|'      /*CUSTOM4*/
                                              ''                                                                                                          '|'      /*CUSTOM5*/
                                              ''                                                                                                          '|'      /*CUSTOM6*/
                                              ''                                                                                                          '|'      /*CUSTOM7*/
                                              ''                                                                                                          '|'      /*CUSTOM8*/
                                              ''                                                                                                          '|'      /*CUSTOM9*/
                                              ''                                                                                                          '|'      /*JOB_LEVEL*/
                                              ''                                                                                                          '|'      /*SECRET_QUESTION*/
                                              ''                                                                                                          '|'      /*SECRET_ANSWER*/
                                              ''                                                                                                          '|'      /*JOB_STARTED_ON*/
                                              ''                                                                                                          '|'      /*RATE*/
                                              ''                                                                                                          '|'      /*RATE_CURRENCY*/
                                              ''                                                                                                          '|'      /*IS_INSTRUCTOR*/
                                              ''                                                                                                          '|'      /*EHRI_CODE*/
                                              'crncy000000000000023'                                                                                       '|'      /*LOCAL_CURRENCY*/
                                              ''                                                                                                          '|'      /*ANL_BASE_COMP*/
                                              ''                                                                                                          '|'      /*VAR_CASH_COMP*/
                                              ''                                                                                                          '|'      /*TRGT_BASE_COMP*/
                                              ''                                                                                                          '|'      /*TRGT_VAR_COMP*/
                                              ''                                                                                                          '|'      /*TRGT_TOT_ANL_COMP*/
                                              ''                                                                                                          '|'      /*PAY_STRUCTURE*/
                                              ''                                                                                                          '|'      /*PAY_GRADE*/
                                              ''                                                                                                          '|'      /*MRKT_PAY_GRD_MIN*/
                                              ''                                                                                                          '|'      /*MRKT_PAY_GRD_MAX*/
                                              ''                                                                                                          '|'      /*CORRESPONDENCE1*/
                                              ''                                                                                                          '|'      /*CORRESPONDENCE2*/
                                              ''                                                                                                          '|'      /*CORRESPONDENCE3*/
                                              ''                                                                                                          '|'      /*OFFERING_LANGUAGE*/
                                              ''                                                                                                          '|'      /*DELIVERY_TYPE*/
                                              ''                                                                                                          '|'      /*AUDIENCE_TYPE1*/
                                              ''                                                                                                          '|'      /*AUDIENCE_TYPE2*/
                                              'Internal Person Basic Priviledges'                                                                          '|'      /*SECURITY_ROLE1*/
                                              'common'                                                                                                     '|'      /*DOMAIN1*/
                                              ''                                                                                                          '|'      /*SECURITY_ROLE2*/
                                              ''                                                                                                          '|'      /*DOMAIN2*/
                                              ''                                                                                                          '|'      /*ALTERNATE_MANAGER1*/
                                              ''                                                                                                          '|'      /*ALTERNATE_MANAGER2*/
                                              ''                                                                                                          '|'      /*APPROVER_TYPE1*/
                                              ''                                                                                                          '|'      /*APPROVER1*/
                                              ''                                                                                                          '|'      /*APPROVER_TYPE2*/
                                              ''                                                                                                          '|'      /*APPROVER2*/
                                              ''                                                                                                          '|'      /*IM_ALIAS_TYPE1*/
                                              ''                                                                                                          '|'      /*IM_ALIAS1*/
                                              ''                                                                                                          '|'      /*IM_ALIAS_PREF1*/
                                              ''                                                                                                          '|'      /*IM_ALIAS_TYPE2*/
                                              ''                                                                                                          '|'      /*IM_ALIAS2*/
                                              ''                                                                                                          '|'      /*IM_ALIAS_PREF2*/
                                              ''                                                                                                          '|'      /*EXCUSTOM1*/
                                              ''                                                                                                          '|'      /*EXCUSTOM2*/
                                              ''                                                                                                          '|'      /*EMAIL1*/
                                              ''                                                                                                                /*EMAIL2*/

     
    skip.
    
end.

END.
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

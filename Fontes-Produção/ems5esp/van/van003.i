/****************************************************************************************** 
** 	   Programa: van003.i
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 25/09/2018
** Change/Chamado: 
**      Objetivo: Definiá∆o de vari†veis - programa van\van003.p
**                Segmentos A e B layout 240 posiá‰es
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

/* Buffers */
DEF BUFFER banco               FOR ems5.banco.
DEF BUFFER bf-banco            FOR ems5.banco.
DEF BUFFER bf-movto_cta_corren FOR movto_cta_corren.
DEF BUFFER bf-cta_corren       FOR cta_corren.
DEF BUFFER bf-estabelecimento  FOR estabelecimento.
DEF BUFFER bf-pessoa_jurid     FOR pessoa_jurid.
DEF BUFFER bf-agenc_bcia       FOR agenc_bcia.

/* Variaveis output */
DEFINE VARIABLE cfile AS CHARACTER   NO-UNDO.

/* Vari†veis Header do arquivo */
DEFINE VARIABLE c-header   AS CHARACTER    NO-UNDO FORMAT "X(240)".
DEFINE VARIABLE c-banco    AS CHARACTER    NO-UNDO FORMAT "999".
DEFINE VARIABLE c-cnpj     AS CHARACTER    NO-UNDO FORMAT "99999999999999".
DEFINE VARIABLE c-convenio AS CHARACTER    NO-UNDO FORMAT "x(20)".
DEFINE VARIABLE c-prodtst  AS CHARACTER    NO-UNDO FORMAT "X(02)".
DEFINE VARIABLE c-agencia  AS CHARACTER    NO-UNDO FORMAT "99999".
DEFINE VARIABLE c-digAg    AS CHARACTER    NO-UNDO FORMAT "X(01)".
DEFINE VARIABLE c-conta    AS CHARACTER    NO-UNDO FORMAT "x(10)".
DEFINE VARIABLE c-digCta   AS CHARACTER    NO-UNDO FORMAT "X(01)".
DEFINE VARIABLE c-nomEmp   AS CHARACTER    NO-UNDO FORMAT "X(30)".
DEFINE VARIABLE c-nomBanco AS CHARACTER    NO-UNDO FORMAT "X(30)".
DEFINE VARIABLE i_num_rem  AS INTEGER      NO-UNDO FORMAT "999999".
DEFINE VARIABLE c-dig-agcB AS CHARACTER    NO-UNDO FORMAT "X(01)".
DEFINE VARIABLE i-cont     AS INTEGER      NO-UNDO.

/* Vari†veis Trailler do Arquivo */
DEFINE VARIABLE c-trailler AS CHARACTER   NO-UNDO FORMAT "X(240)".

/* Vari†veis Header do Lote */
DEFINE VARIABLE c-headerLote  AS CHARACTER   NO-UNDO FORMAT "X(240)".
DEFINE VARIABLE c-formalancto AS CHARACTER   NO-UNDO FORMAT "x(02)".
DEFINE VARIABLE c-end     AS CHARACTER NO-UNDO FORMAT "X(30)".
DEFINE VARIABLE c-cep     AS CHARACTER NO-UNDO FORMAT "X(8)".
DEFINE VARIABLE c-bairro  AS CHARACTER NO-UNDO FORMAT "X(15)".
DEFINE VARIABLE c-cidade  AS CHARACTER NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE c-uf      AS CHARACTER NO-UNDO FORMAT "X(02)".

/* Vari†veis do Lote A */
DEFINE VARIABLE c-SegA         AS CHARACTER   NO-UNDO FORMAT "X(240)".
DEFINE VARIABLE c-bco-favor    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-ag-bcofav    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-camCen       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-div-agfav    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cta-fav      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dig-cta-fav  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dig-agcta    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nomFav       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-num_id_movto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-vl-pagto     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dt-pagto     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cod-finalid-ted     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-compl-finalid-pagto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-compl-tp-servico    AS CHARACTER   NO-UNDO.

/* Vari†veis do Lote B */
DEFINE VARIABLE c-SegB       AS CHARACTER NO-UNDO FORMAT "X(240)".
DEFINE VARIABLE c-endFav     AS CHARACTER NO-UNDO FORMAT "X(30)".
DEFINE VARIABLE c-cepFav     AS CHARACTER NO-UNDO FORMAT "X(8)".
DEFINE VARIABLE c-bairroFav  AS CHARACTER NO-UNDO FORMAT "X(15)".
DEFINE VARIABLE c-cidadeFav  AS CHARACTER NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE c-cnpj-Fav   AS CHARACTER NO-UNDO FORMAT "X(15)".
DEFINE VARIABLE c-ufFav      AS CHARACTER NO-UNDO FORMAT "X(02)".

/* Vari†veis do Trailler do Lote */
DEFINE VARIABLE c-traLote AS CHARACTER   NO-UNDO FORMAT "X(240)".

/* Vari†veis para Van */
DEFINE NEW GLOBAL SHARED VAR cTipoOrigem AS CHAR NO-UNDO.
DEFINE VAR cId         AS CHAR NO-UNDO. 
DEFINE VAR cProc       AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR r_row_webserver AS ROWID NO-UNDO.
DEFINE NEW SHARED VAR r_row_param_dir AS ROWID NO-UNDO.
DEFINE VARIABLE h-van006 AS HANDLE      NO-UNDO.


/* Leitura das tabelas para montar as vari†veis */
FIND movto_cta_corren NO-LOCK WHERE
        ROWID(movto_cta_corren) = ipr-row-table NO-ERROR.
IF NOT AVAIL movto_cta_corren THEN RETURN "N∆o encontrado movimento de conta corrente!".

/* Identifica o banco */
FIND cta_corren NO-LOCK WHERE
     cta_corren.cod_cta_corren = movto_cta_corren.cod_cta_corren NO-ERROR.
IF NOT AVAIL cta_corren THEN RETURN "N∆o encontrada conta corrente!".

FIND agenc_bcia NO-LOCK 
    WHERE agenc_bcia.cod_banco      = cta_corren.cod_banco
      AND agenc_bcia.cod_agenc_bcia = cta_corren.cod_agenc_bcia NO-ERROR.
IF NOT AVAIL agenc_bcia THEN
    RETURN "N∆o encontrada agància da conta de Origem.".

/* busca conta Destino - Forma de Lanáamento */
FIND bf-movto_cta_corren NO-LOCK 
    WHERE bf-movto_cta_corren.num_id_movto_cta_corren = movto_cta_corren.num_id_movto_cta_trans NO-ERROR.
IF NOT AVAIL bf-movto_cta_corren THEN
    RETURN "N∆o encontrado Movimento de destino".

FIND bf-cta_corren NO-LOCK WHERE
     bf-cta_corren.cod_cta_corren = bf-movto_cta_corren.cod_cta_corren NO-ERROR.
IF NOT AVAIL bf-cta_corren 
    THEN RETURN "Conta Destino n∆o encontrada!".

FIND bf-agenc_bcia NO-LOCK 
    WHERE bf-agenc_bcia.cod_banco      = bf-cta_corren.cod_banco
      AND bf-agenc_bcia.cod_agenc_bcia = bf-cta_corren.cod_agenc_bcia NO-ERROR.
IF NOT AVAIL bf-agenc_bcia THEN
    RETURN "N∆o encontrada agància da conta Destino.".

/* Identifica Empresa */
FIND estabelecimento NO-LOCK WHERE
     estabelecimento.cod_estab = cta_corren.cod_estab NO-ERROR.
IF NOT AVAIL estabelecimento THEN
    RETURN "N∆o encontrado estabelecimento Origem!".

FIND pessoa_jurid NO-LOCK WHERE
     pessoa_Jurid.num_pessoa_jurid = estabelecimento.num_pessoa_jurid NO-ERROR.
IF NOT AVAIL pessoa_Jurid THEN
    RETURN "N∆o encontrada pessoa jur°dica do estabelecimento!". 

/* Identifica Favorecido */
FIND bf-estabelecimento NO-LOCK WHERE
     bf-estabelecimento.cod_estab = bf-cta_corren.cod_estab NO-ERROR.
IF NOT AVAIL bf-estabelecimento THEN
    RETURN "N∆o encontrado estabelecimento de destino!".

FIND bf-pessoa_jurid NO-LOCK WHERE
     bf-pessoa_Jurid.num_pessoa_jurid = bf-estabelecimento.num_pessoa_jurid NO-ERROR.
IF NOT AVAIL bf-pessoa_jurid THEN
    RETURN "Pessoa jur°dica destino n∆o encontrada!".

/* Busca c¢digo do convenio do banco */
FIND FIRST portad_finalid_econ NO-LOCK
    WHERE portad_finalid_econ.cod_estab         = cta_corren.cod_estab
      AND portad_finalid_econ.cod_cart_bcia     = "APB"
      AND portad_finalid_econ.cod_finalid_econ  = cta_corren.cod_finalid_econ
      AND portad_finalid_econ.cod_cta_corren    = cta_corren.cod_cta_corren NO-ERROR.
IF NOT AVAIL portad_finalid_econ THEN
    RETURN "Finalidade do Portador de Origem n∆o encontrado!".

FIND portad_edi NO-LOCK 
    WHERE portad_edi.cod_modul_dtsul  = "APB" 
      AND portad_edi.cod_estab        = cta_corren.cod_estab
      AND portad_edi.cod_portador     = portad_finalid_econ.cod_portador /* Verificar */
      AND portad_edi.cod_cart_bcia    = "APB"
      AND portad_edi.cod_finalid_econ = cta_corren.cod_finalid_econ NO-ERROR.
IF NOT AVAIL portad_edi THEN
    RETURN "Portador EDI n∆o foi encontrado!".

/* Busca nome do Banco */
FIND banco NO-LOCK 
    WHERE banco.cod_banco = cta_corren.cod_banco NO-ERROR.
IF NOT AVAIL banco THEN
    RETURN "N∆o encontrado Banco Origem.".

FIND bf-banco NO-LOCK WHERE
     bf-banco.cod_banco = bf-cta_corren.cod_banco NO-ERROR.
IF NOT AVAIL bf-banco THEN
    RETURN "N∆o encontrado Banco de Destino.".        

/* Header Arq */
ASSIGN c-banco    = STRING(cta_corren.cod_banco,"999") WHEN AVAIL cta_corren
       c-cnpj     = pessoa_jurid.cod_id_feder
       c-convenio = SUBSTR(portad_edi.des_tip_var_portad_edi,1,INDEX(portad_edi.des_tip_var_portad_edi,CHR(10),1) - 1)
       c-prodtst  = "  " /* Brancos qdo produá∆o */ 
       c-agencia  = cta_corren.cod_agenc_bcia
       c-digAg    = CAPS(agenc_bcia.cod_digito_agenc_bcia )
       c-conta    = cta_corren.cod_cta_corren_bco
       c-digCta   = CAPS(cta_corren.cod_digito_cta_corren) 
       c-nomEmp   = estabelecimento.nom_pessoa
       c-nomBanco = banco.nom_banco
       c-dig-agcB = CAPS(SUBSTR(cta_corren.cod_digito_agenc_cta_corren,2,1))
       i_num_rem  = portad_edi.num_prox_remes_msg_edi + 1.

DO i-cont = 1 TO 31:
    ASSIGN c-convenio = REPLACE(c-convenio,CHR(i-cont),"").
END.

/* Header do Lote */
ASSIGN c-formalancto = IF cta_corren.cod_banco <> bf-cta_corren.cod_banco THEN "03"/* DOC / TED */  
                       ELSE "01" /* CrÇdito em CC */
       c-end    = pessoa_jurid.nom_endereco     
       c-cep    = pessoa_jurid.cod_cep          
       c-bairro = pessoa_jurid.nom_bairro       
       c-cidade = pessoa_jurid.nom_cidade       
       c-uf     = pessoa_jurid.cod_unid_federac.

/* Segmento A */
ASSIGN c-bco-favor    = bf-cta_corren.cod_banco
       c-ag-bcofav    = bf-cta_corren.cod_agenc_bcia
       c-div-agfav    = CAPS(bf-agenc_bcia.cod_digito_agenc_bcia )
       c-cta-fav      = bf-cta_corren.cod_cta_corren_bco
       c-dig-cta-fav  = CAPS(SUBSTR(bf-cta_corren.cod_digito_cta_corren,1,1)) /* revisada a posiá∆o de inicio em  25/04/2019 / CAPS(SUBSTR(bf-cta_corren.cod_digito_cta_corren,2,1))*/
       c-dig-agcta    = CAPS(bf-cta_corren.cod_digito_agenc_cta_corren)
       c-nomFav       = bf-estabelecimento.nom_pessoa
       c-num_id_movto = STRING(movto_cta_corren.num_id_movto_cta_corren)
       c-dt-pagto     = STRING(movto_cta_corren.dat_movto_cta_corren,"99999999")
       c-vl-pagto     = STRING(movto_cta_corren.val_movto_cta_corren * 100,"999999999999999").

/* Segmento B */
ASSIGN c-endFav    = bf-pessoa_jurid.nom_endereco     
       c-cepFav    = bf-pessoa_jurid.cod_cep
       c-bairroFav = bf-pessoa_jurid.nom_bairro
       c-cidadeFav = bf-pessoa_jurid.nom_cidade
       c-cnpj-Fav  = bf-pessoa_jurid.cod_id_feder
       c-nomFav    = bf-pessoa_jurid.nom_pessoa
       c-ufFav     = bf-pessoa_jurid.cod_unid_federac.

/* Camara Centralizadora */
IF c-formalancto = "03"  THEN 
   c-camCen = (IF movto_cta_corren.val_movto_cta_corren <= 4999.99
                THEN "700" ELSE "018").

IF c-formalancto = "01"  THEN 
     c-camCen = (IF cta_corren.cod_estab = bf-cta_corren.cod_estab THEN "000" ELSE "  ").
/* Fim Camara Centralizadora */

/* DOC TED */
IF c-formalancto = "03" THEN /* DOC ou TED */
   ASSIGN c-cod-finalid-ted     = '00010'   
          c-compl-finalid-pagto = 'CC'      
          c-compl-tp-servico    = '01'.       

IF c-formalancto = "01" THEN
  ASSIGN c-cod-finalid-ted     = '' 
         c-compl-finalid-pagto = 'CC'
         c-compl-tp-servico    = ''.

PROCEDURE pi-exporta:

    ASSIGN cFile =  ipc-dir-rem + "\Transf_Conta_" + STRING(movto_cta_corren.num_id_movto_cta_corren) + ".rem".      

    OUTPUT TO VALUE(cFile) CONVERT SOURCE "UTF-8" TARGET "ISO8859-1".
   
        PUT c-header     SKIP
            c-headerLote SKIP
            c-SegA       SKIP   
            c-SegB       SKIP
            c-traLote    SKIP
            c-trailler   SKIP.                                                            
                                                                                           
    OUTPUT CLOSE.   

/*        ASSIGN cTipoOrigem = "erp"                                                                                   */
/*               cId = "01"                                                                                            */
/*               cProc = "EnviaRemessa".                                                                               */
/*                                                                                                                     */
/*        /* Identifica a conta corrente que ser† utilizada para enviar o pdsid correto */                             */
/*        FIND FIRST es_param_webserver NO-LOCK                                                                        */
/*             WHERE es_param_webserver.identificador = cId                                                            */
/*               AND es_param_webserver.nomeserv      = cProc NO-ERROR.                                                */
/*        IF NOT AVAIL es_param_webserver THEN DO:                                                                     */
/*                                                                                                                     */
/*            MESSAGE "N∆o foi poss°vel identificar parametro para Web SERVER" SKIP                                    */
/*                    "Identificador" cId SKIP                                                                         */
/*                    "Processo" cProc SKIP                                                                            */
/*                    "Entre em contato com TI para verificar o cadastro van001-w01"  SKIP                             */
/*                    "Arquivo de pagamento escritural n∆o ser† enviado para a Van."                                   */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                   */
/*            RETURN ERROR.                                                                                            */
/*        END.                                                                                                         */
/*                                                                                                                     */
/*        FIND FIRST es_param_van_dir NO-LOCK                                                                          */
/*            WHERE es_param_van_dir.identificador   = es_param_webserver.identificador                                */
/*              AND es_param_van_dir.nomeserv        = es_param_webserver.nomeserv                                     */
/*              AND es_param_van_dir.cod_intercambio = es_param_webserver.cod_intercambio                              */
/*              AND es_param_van_dir.cod_cta_corren  = portad_finalid_econ.cod_cta_corren NO-ERROR.                    */
/*        IF NOT AVAIL es_param_van_dir THEN DO:                                                                       */
/*                                                                                                                     */
/*            MESSAGE "N∆o foi poss°vel identificar o PDSID da conta corrente" portad_finalid_econ.cod_cta_corren SKIP */
/*                    "Identificador" cId SKIP                                                                         */
/*                    "Processo" cProc SKIP                                                                            */
/*                    "Entre em contato com TI para verificar o cadastro van001-w01"  SKIP                             */
/*                    "Arquivo de pagamento escritural n∆o ser† enviado para a Van."                                   */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                   */
/*            RETURN ERROR.                                                                                            */
/*        END.                                                                                                         */
/*                                                                                                                     */
/*        ASSIGN r_row_webserver = ROWID(es_param_webserver)                                                           */
/*               r_row_param_dir = ROWID(es_param_van_dir).                                                            */
/*                                                                                                                     */
/*                                                                                                                     */
/*        RUN van\van006.p PERSISTENT SET h-van006.                                                                    */
/*                                                                                                                     */
/*        RUN pi-remessa IN h-van006.                                                                                  */
       
    RETURN "OK".
END.

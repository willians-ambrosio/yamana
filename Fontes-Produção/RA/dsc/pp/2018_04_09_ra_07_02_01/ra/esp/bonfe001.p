
/*--------------------------------------------------------------------------------
    File        : bonfe001.p
    Purpose     :

    Syntax      :

    Description : BO de Recebimento Automatico XML

    Author(s)   : Alberto Duzi
    Created     : 2009/2010 DSC - Desenvolvimento de Sistemas e Consultoria
    Atualizacao : Versao: 1.04.000 - 01/10/2010 - Alberto Duzi
    Atualizacao : Versao: 1.04.016 - 02/02/2011 - Eugenio A. Marietti 
                                     Adicionada rotina para verificacao de 
                                     inconsistencias entre o XML e os dados
                                     dos cadastros do ERP (DSCRAXX028)
    Atualizacao : Versao: 1.04.020 - 07/06/2011 - Rodrigo Baione 
                                     Alteracao de verificacao dos dados da
                                     item-fornec, nao permitindo alteracoes
                                     na tabela padrao da datasul. Criar 
                                     tabela especifica armazenando os 
                                     dados sugeridos pelo usuario. No 
                                     momento do envio da nfe para api de 
                                     integracao se houver divergencia 
                                     nos dados da item-fornec com a tabela
                                     especifica realizar as alteracoes 
                                     necessarias para que os dados enviados
                                     estejam de acordo com a item-fornec.
    Atualizacao : Versao: 1.04.021 - 04/10/2011 - Fabio Guedes
                                     Inclusao de rotina de gravacao de erro
                                     na tabela NFE-LOG-ERROS na procedure
                                     PI_MENSAGEM_ERRO
    Atualizacao : Versao: 1.04.022 - 25/10/2011 - Fabio Guedes
                                     Susbstituida atribuicao do tipo da nota
                                     no recebimento fiscal pela rotina que
                                     valida e devolve o tipo correto
    Atualizacao : Versao: 1.04.023 - 27/10/2011 - Fabio Guedes
                                     Inserida a chamada da rotina de recalculo
                                     dos impostos para garantir que todos
                                     sejam atualizados no recebimento fiscal
    Atualizacao : Versao: 1.04.024 - 08/11/2011 - Fabio Guedes
                                     Criacao das regras de validacao do
                                     tipo da nota fiscal no recebimento
                                     fisico (Procedure PI_VALIDA_TIPO_NOTA_FISICO)
    Atualizacao :                    20/05/2014 - L?a Campos
                                     Find do emitente passa a ser pelo nome-abrev e nao mais
                                     pelo cgc (Devido ao cadastro de inativo com cnpj duplicado)
                                     08/10/2014 - L?a - Comentei a linha pois estava dando erro
                                     ao implantar nota com item de servico
                                     /*IF bf-nfe-it-nota-fisc-rec.imp-iss-vAliq > 0 THEN*/ 
                                     - 27-10-2014 - Lìa - criei o campo tipo na tt-nfe-recebimento para identificar
                                       quando for somente consulta sefaz ou carrega xml. Passou atualizar os dados 
                                       apenas quando for carrega xml.
                                     - 03-11-2014 - L?a Criacao do parametro Pedagio_bo.
                                     - 02/02/15   - Passa parametro rowid(nfe-it-nota-fisc-rec) para esra010.p
                                     
*/                                     
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ------------------ Listagem de Procedures ---------------------------                                              
| pi_busca_danfe                    | pi_busca_deposito_cq              |        
| pi_carrega_xml                    | pi_consulta_sefaz                 |
| pi_copia_xml_dscrc                | pi_entrada_receb_fiscal           |
| pi_entrada_receb_fisico           | pi_gera_relac_emit_cfop_nat       |
| pi_gera_relac_item_fornec         | pi_limpa_epc                      |
| pi_mensagem_erro                  | pi_nfe_1_compra                   |
| pi_nfe_2_devol_cli                | pi_nfe_3_retorno_terc             |
| pi_nfe_4_remessa_terc             | pi_retorna_fornecedor             |
| pi_retorno_sefaz_dscrc            | pi_valida_barra                   |
| pi_valida_dados                   | pi_valida_diretorios              |
| pi_valida_entrada                 | pi_valida_relacionamentos         |
| pi_valida_retorno_sefaz           | pi_valida_tipo_nota               |
| pi_exclui_registro                | pi_atualiza_status                |
| pi_relaciona_ordem                | pi_relaciona_ser_lote             |
| pi_relaciona_doc_nota             | pi_verifica_relac_item            |
| pi_item_receb_fiscal              | pi_nota_receb_fiscal              |
| pi_item_recebimento               | pi_nota_receb_fisico              |
| pi_item_receb_fisico              | pi_valida_upc                     |
| pi_recalcula_imposto              | pi_gera_rat_lote_fiscal           |
| pi_atualiza_niv_inspec            | pi_verifica_inconsist             |
| pi_grava_inconsist                | pi_valida_tipo_nota_fisico        |
----------------------------------------------------------------------- */
                                          
/* ***************************  Definitions  ************************** */
{dsc\ra\include\buffers.i}
{cdp\cdcfgdis.i} /* --- Definicao versao do EMS --- */
{cdp\cdcfgmat.i}
{dsc\ra\include\variaveis-nfe-receb.i}

{inbo\boin090.i  tt-docum-est} 
{inbo\boin366.i  tt-rat-docum}
{inbo\boin176.i  tt-item-doc-est}
{inbo\boin092.i  tt-dupli-apagar}
{inbo\boin567.i  tt-dupli-imp}
{inbo\boin176.i3 tt-ordem-item }
    
{include\i-epc200.i bonfe001}

DEF TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence         AS   INTE
    FIELD ErrorNumber           AS   INTE
    FIELD ErrorDescription      AS   CHAR
    FIELD ErrorParameters       AS   CHAR
    FIELD ErrorType             AS   CHAR
    FIELD ErrorHelp             AS   CHAR
    FIELD ErrorSubType          AS   CHAR.
    
DEF TEMP-TABLE tt-doc-fisico NO-UNDO LIKE doc-fisico
    FIELD rw-doc-fisico         AS   ROWID.

DEF TEMP-TABLE tt-it-doc-fisico NO-UNDO LIKE it-doc-fisico
    FIELD rw-it-doc-fisico      AS   ROWID.

DEF TEMP-TABLE tt-docum-est-aux NO-UNDO LIKE tt-docum-est.

DEF TEMP-TABLE tt-item-doc-est-aux NO-UNDO
    FIELD chave-acesso-nfe      LIKE nfe-it-nota-fisc-rec.chave-acesso-nfe
    FIELD seq-item              LIKE tt-item-doc-est.sequencia
    FIELD sequencia             LIKE tt-item-doc-est.sequencia
    FIELD nat-operacao          LIKE tt-item-doc-est.nat-operacao
    INDEX ix_docto AS PRIMARY UNIQUE chave-acesso-nfe seq-item sequencia nat-operacao.

DEF TEMP-TABLE tt-item-doc-est-2 NO-UNDO LIKE tt-item-doc-est.

DEF TEMP-TABLE tt-nfe-recebimento NO-UNDO
    FIELD chave-acesso-nfe      AS   CHAR FORMAT "x(050)"
    FIELD tipo                  AS   LOGICAL FORMAT 'Carrega/Consulta'.

DEF TEMP-TABLE tt-item-fornec NO-UNDO
    FIELD im-if-narrativa       LIKE item-fornec.narrativa
    FIELD im-if-concentracao    LIKE item-fornec.concentracao
    FIELD im-if-rendimento      LIKE item-fornec.rendimento
    FIELD im-if-cod-msg         LIKE item-fornec.cod-mensagem
    FIELD im-if-hora-ini        LIKE item-fornec.hora-ini
    FIELD im-if-hora-fim        LIKE item-fornec.hora-fim
    FIELD im-if-conceito        LIKE item-fornec.conceito
    FIELD im-if-observacao      LIKE item-fornec.observacao
    FIELD im-if-serie-nota      LIKE item-fornec.serie-nota
    FIELD im-if-num-nota        LIKE item-fornec.numero-nota
    FIELD im-if-horiz-fixo      LIKE item-fornec.horiz-fixo
    FIELD im-if-ped-fornec      LIKE item-fornec.ped-fornec
    FIELD im-if-tp-inspecao     LIKE item-fornec.tp-inspecao
    FIELD im-if-criticidade     LIKE item-fornec.criticidade
    FIELD im-if-qt-max-ord      LIKE item-fornec.qt-max-ordem
    FIELD im-if-niv-qua-ac      LIKE item-fornec.niv-qua-ac
    FIELD im-if-niv-inspecao    LIKE item-fornec.niv-inspecao
    FIELD im-if-it-codigo       LIKE item-fornec.it-codigo
    FIELD im-if-cod-emitente    LIKE item-fornec.cod-emitente
    FIELD im-if-item-do-forn    LIKE item-fornec.item-do-forn
    FIELD im-if-unid-med-for    LIKE item-fornec.unid-med-for
    FIELD im-if-fator-conver    LIKE item-fornec.fator-conver
    FIELD im-if-num-casa-dec    LIKE item-fornec.num-casa-dec
    FIELD im-if-tempo-ressup    LIKE item-fornec.tempo-ressup
    FIELD im-if-lote-minimo     LIKE item-fornec.lote-minimo
    FIELD im-if-lote-mul-for    LIKE item-fornec.lote-mul-for
    FIELD im-if-perc-compra     LIKE item-fornec.perc-compra
    FIELD im-if-cod-cond-pag    LIKE item-fornec.cod-cond-pag
    FIELD im-if-classe-repro    LIKE item-fornec.classe-repro
    FIELD im-perc-pont-forn     LIKE item-fornec.perc-pont-forn
    FIELD im-ind-pont           LIKE item-fornec.ind-pont
    FIELD im-perc-dev-forn      LIKE item-fornec.perc-dev-forn
    FIELD im-if-contr-forn      AS   CHAR FORMAT "x(001)"
    FIELD im-if-reaj-tabela     AS   CHAR FORMAT "x(001)"
    FIELD im-if-ativo           AS   CHAR FORMAT "x(001)"
    FIELD im-if-cot-aut         AS   CHAR FORMAT "x(001)"
    FIELD im-if-acao            AS   INTE INIT 0. /* --- 0 - Gera/Altera, 1 - Gera, 2 - Altera --- */

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD identif-segment       AS   CHAR
    FIELD cd-erro               AS   INTE
    FIELD desc-erro             AS   CHAR FORMAT "x(80)".
    
DEF TEMP-TABLE tt-mensagem-erro NO-UNDO
    FIELD i-sequen              AS   INTE
    FIELD cd-erro               AS   INTE
    FIELD mensagem              AS   CHAR FORMAT "x(255)".

DEF TEMP-TABLE tt-nfe-relac-ordem-rec NO-UNDO LIKE nfe-relac-ordem-rec
    FIELD l-retirar             AS   LOG INIT NO.

DEF TEMP-TABLE tt-nfe-relac-ser-lote-rec NO-UNDO LIKE nfe-relac-ser-lote-rec
    FIELD l-retirar             AS   LOG INIT NO.

DEF TEMP-TABLE tt-nfe-relac-doc-nota-rec  NO-UNDO LIKE nfe-relac-doc-nota-rec
    FIELD l-retirar             AS   LOG INIT NO.

DEF TEMP-TABLE tt2-nfe-relac-doc-nota-rec  NO-UNDO LIKE nfe-relac-doc-nota-rec
    FIELD i-seq-doc             AS   INT INIT 0
    FIELD i-rel-doc             AS   INT INIT 0.

DEF TEMP-TABLE tt-rat-lote NO-UNDO LIKE rat-lote
    FIELD r-rowid               AS   ROWID.

DEF TEMP-TABLE tt-rat-lote-aux NO-UNDO LIKE rat-lote
    FIELD r-rowid               AS   ROWID.
    
DEF TEMP-TABLE tt-movto-pend NO-UNDO LIKE movto-pend
    FIELD r-rowid               AS   ROWID.

DEF TEMP-TABLE tt-param NO-UNDO /* @EMA! - 1.04.016 */
    FIELD destino               AS   INTE
    FIELD arquivo               AS   CHAR    
    FIELD chave-acesso-nfe      AS   CHAR
    FIELD cod-estabel           AS   CHAR EXTENT 2
    FIELD serie                 AS   CHAR EXTENT 2
    FIELD nr-nota-fis           AS   CHAR EXTENT 2
    FIELD cod-emit              AS   INTE EXTENT 2
    FIELD nome-abrev            AS   CHAR EXTENT 2
    FIELD dt-emis               AS   DATE EXTENT 2
    FIELD l-pendente            AS   LOG
    FIELD l-corrigida           AS   LOG.

DEFINE TEMP-TABLE tt-nfe-nota-fiscal-rec-fisico  NO-UNDO LIKE nfe-nota-fiscal-rec
    FIELD tipo-nota AS INT
    INDEX chave-esp AS PRIMARY UNIQUE
    chave-acesso-nfe
    tipo-nota.
     
DEFINE TEMP-TABLE tt-nfe-it-nota-fisc-rec-fisico NO-UNDO LIKE nfe-it-nota-fisc-rec
    FIELD tipo-nota AS INT.

DEFINE TEMP-TABLE tt-ordens-fifo NO-UNDO
        FIELD it-codigo     LIKE ITEM.it-codigo
        FIELD de-quantidade AS DEC 
        FIELD sequencia     AS CHAR 
        FIELD rw-item       AS ROWID.

DEF VAR rw-nota-fiscal-rec      AS   ROWID            NO-UNDO.
DEF VAR rw-retorna-fornec       AS   ROWID            NO-UNDO.
DEF VAR l_permite_usu           AS   LOGICAL          NO-UNDO.
DEF VAR h-acomp                 AS   HANDLE           NO-UNDO.
DEF VAR l-consulta              AS   LOGICAL          NO-UNDO.
DEFINE VARIABLE l-pendente-mla  AS   LOGICAL          NO-UNDO.
DEFINE VARIABLE de-saldo-prazo  AS   DECIMAL          NO-UNDO.
DEFINE VARIABLE i-contador      AS   INTEGER          NO-UNDO.
DEFINE VARIABLE xped-char-aux   LIKE nfe-it-nota-fisc-rec.item-xped-char NO-UNDO.
DEFINE VARIABLE nat-oper-cabec  LIKE nfe-it-nota-fisc-rec.item-nat-operacao. 

DEF BUFFER bf-nfe-nota-fiscal-rec   FOR nfe-nota-fiscal-rec.
DEF BUFFER bfu-nfe-nota-fiscal-rec  FOR nfe-nota-fiscal-rec.
DEF BUFFER bf2-nfe-nota-fiscal-rec  FOR nfe-nota-fiscal-rec.
DEF BUFFER bf5-nfe-nota-fiscal-rec  FOR nfe-nota-fiscal-rec.
DEF BUFFER bf-nfe-it-nota-fisc-rec  FOR nfe-it-nota-fisc-rec.
DEF BUFFER bfu-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
DEF BUFFER bf4-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
DEF BUFFER bf5-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
DEF BUFFER bf2-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
DEF BUFFER bf3-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
DEF BUFFER bf-nfe-his-nota-fis-rec  FOR nfe-his-nota-fis-rec.
DEF BUFFER bf2-nfe-his-nota-fis-rec FOR nfe-his-nota-fis-rec.
DEF BUFFER bf-ord-prod              FOR ord-prod.
DEF BUFFER bf-it-doc-fisico         FOR it-doc-fisico.
DEF BUFFER bf-natur-oper            FOR natur-oper.
DEF BUFFER bf2-natur-oper           FOR natur-oper.
DEF BUFFER bf-item                  FOR ITEM.
DEF BUFFER bf2-item                 FOR ITEM.
DEF BUFFER bf-docum-est             FOR docum-est.
DEF BUFFER bf-item-doc-est          FOR item-doc-est.
DEF BUFFER bf2-item-doc-est         FOR item-doc-est.

DEF BUFFER bf-ordem-compra          FOR ordem-compra.
DEF BUFFER bf-doc-pend-aprov        FOR doc-pend-aprov.     
DEF BUFFER b-emitente               FOR emitente.
DEF BUFFER b-estabelec              FOR estabelec.

DEF STREAM s-log.
DEFINE SHARED VARIABLE c-seg-usuario AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tt-tipos-natur-fisico NO-UNDO
        FIELD nat-operacao LIKE natur-oper.nat-operacao
        FIELD tipo-nota    AS INT
        FIELD sequencia    AS INT.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'Multi_natureza' 
    AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
ASSIGN l-multi_natureza = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'CST_ICMS' 
    AND nfe-it-param-rec.valor-1-item-parametro = "XML" NO-ERROR. 
ASSIGN l-CST-ICMS-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'CST_ICMS' 
    AND nfe-it-param-rec.valor-1-item-parametro = "ERP" NO-ERROR. 
ASSIGN l-CST-ICMS-erp = AVAIL nfe-it-param-rec. /*para apenas calcular como ERP o cliente que escrever ERP*/

FIND FIRST nfe-it-param-rec NO-LOCK
     WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
       AND nfe-it-param-rec.cod-item-parametro     = 'CEST'
       AND nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
ASSIGN l-cest = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_IPI_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-IPI-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_IPI'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-IPI-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_PIS_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-PIS-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_PIS'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-PIS-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_COFINS_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-COFINS-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_COFINS'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-COFINS-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-xml = AVAIL nfe-it-param-rec.
    
FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS_ST_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-ST-dev-xml = AVAIL nfe-it-param-rec.
    
FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS_ST'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-ST-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'nota_beneficiamento' 
    AND nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR. 
ASSIGN l-nota-beneficiamento = AVAIL nfe-it-param-rec.      

FUNCTION fc-saldo-ordem RETURNS DECIMAL
  ( p-numero-ordem AS INT , p-parcela AS INT, p-rw-item AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE de-quant AS DECIMAL     NO-UNDO.  
        
    ASSIGN de-quant = 0.
    FIND FIRST nfe-it-nota-fisc-rec NO-LOCK
        WHERE  ROWID(nfe-it-nota-fisc-rec) = p-rw-item.

     IF AVAIL nfe-it-nota-fisc-rec  THEN DO:

       FOR EACH bf-nfe-it-nota-fisc-rec NO-LOCK
           WHERE bf-nfe-it-nota-fisc-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe
           AND   bf-nfe-it-nota-fisc-rec.item-num-ordem   = p-numero-ordem
           AND   bf-nfe-it-nota-fisc-rec.item-parcela     = p-parcela
           AND   bf-nfe-it-nota-fisc-rec.seq-item        <> nfe-it-nota-fisc-rec.seq-item:
           

           ASSIGN de-quant = de-quant + INT(bf-nfe-it-nota-fisc-rec.item-qCom).

       END.

       FOR EACH nfe-relac-ordem-rec NO-LOCK
           WHERE nfe-relac-ordem-rec.chave-acesso-nfe =  nfe-it-nota-fisc-rec.chave-acesso-nfe
           AND   nfe-relac-ordem-rec.seq-item         <> nfe-it-nota-fisc-rec.seq-item
           AND   nfe-relac-ordem-rec.numero-ordem     =  p-numero-ordem
           AND   nfe-relac-ordem-rec.parcela-oc       =  p-parcela:

           ASSIGN de-quant = de-quant + INT(nfe-relac-ordem-rec.quantidade).


       END.

    END.
    
    
    
    
    RETURN de-quant.   /* Function return value. */

END FUNCTION.




/* ************************************************************************************** */

PROCEDURE pi_busca_danfe:

    DEF INPUT  PARAM p_origem        AS INTEG                            NO-UNDO.
    DEF INPUT  PARAM p_chave_danfe   AS CHAR FORMAT "x(060)"             NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    DEF VAR l-erro           AS LOG  INIT YES NO-UNDO.
    DEF VAR c-natur-oper-aux AS CHAR INIT ""  NO-UNDO.
    DEF VAR l-canc           AS LOG           NO-UNDO.
    DEF VAR l-retorna        AS LOG  INIT YES NO-UNDO.

    
    
    DEF BUFFER bf-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
    DEFINE BUFFER bf-tt-tipos-natur-fisico FOR tt-tipos-natur-fisico .
    DEFINE VARIABLE h-acomp-1 AS HANDLE      NO-UNDO.

    DEFINE VARIABLE c-itens-erro-class AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE l-erro-class AS LOGICAL NO-UNDO.
    
    /** p_origem(1) - PAINEL RECEBIMENTO FISICO **
     ** p_origem(2) - PAINEL RECEBIMENTO FISCAL **    
     ** p_origem(3) - RECEBIMENTO FISICO        **
     ** p_origem(4) - RECEBIMENTO FISCAL        **/

    /* --- Busca Nota Fiscal Eletronica --- */

    FIND FIRST nfe-nota-fiscal-rec WHERE 
               nfe-nota-fiscal-rec.chave-acesso-nfe = TRIM(p_chave_danfe) NO-LOCK NO-ERROR.

    FIND FIRST nfe-dfe NO-LOCK
        WHERE nfe-dfe.chave-acesso = nfe-nota-fiscal-rec.chave-acesso-nfe NO-ERROR.

    /*Verifica se a Nota estÿ como Desconhecimento de Operacao ou Operacao NAO Realizada no MD*/
    IF AVAIL nfe-nota-fiscal-rec THEN DO:
        DEF VAR l-erro-sit AS LOG.
        IF SEARCH("dsc\md\esp\esmd012.p") <> ? THEN  /*buscar se a NF esta no MD*/
            RUN dsc\md\esp\esmd012.p (INPUT p_chave_danfe,
                                      OUTPUT l-erro-sit).
        IF l-erro-sit THEN
            RETURN NO-APPLY.
    END.
    
    ELSE DO: /*Se a Nota n’o estÿ no RA, procura no MD, sen’o existir no MD, cria o registro no MD para dar ciencia*/

        IF SEARCH("dsc\md\esp\esmd011.p") <> ? THEN DO: /*buscar se a NF esta no MD*/

            RUN dsc\md\esp\esmd011.p (INPUT p_chave_danfe).
            
            RETURN NO-APPLY.
        END.
        ELSE DO:

            ASSIGN p_mensagem_erro = "1,"                                       +
                                     "Nota Fiscal Eletronica nao encontrada !," +
                                     "Informe uma Nota Fiscal Eletronica valida.".
    
            LEAVE.
        END.
    END.        

    FIND emitente WHERE 
         emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                           +
                                 "Fornecedor nao cadastrado !," +
                                 "O Fornecedor da Nota Fiscal Eletronica nao foi encontrado no cadastro.".

        LEAVE.
    END.

    
    /* --- Verifica Parametros Globais --- */
                        
    FIND FIRST nfe-param-rec WHERE 
               nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

    IF NOT AVAIL nfe-param-rec THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                                                       +
                                 "Parametros Globais do Recebimento NFE nao encontrado !,"  +
                                 "Registro de Parametros Globais (param_global) precisa ser cadastrado corretamente em Parametros do Recebimento NFE (esnfe400).".

        LEAVE.
    END.

    /* --- Verifica se o Usuario tem Permissao para o Estabelecimento --- */
    
    ASSIGN l_permite_usu = NO.

    FIND FIRST nfe-usuar-estab WHERE 
               nfe-usuar-estab.cod-usuario = c-seg-usuario                   AND 
               nfe-usuar-estab.cod-estabel = nfe-nota-fiscal-rec.cod-estabel NO-LOCK NO-ERROR.

    IF NOT AVAIL nfe-usuar-estab THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                           +
                                 "Usuario nao tem Permissao !," +
                                 "O Usuario nao tem Permissao para o Estabelecimento " + nfe-nota-fiscal-rec.cod-estabel +
                                 ". O Usuario precisa ser cadastrado corretamente em Usuario x Estabelecimento (esnfe500).".

        LEAVE.
    END.
    ELSE 
    DO:
        IF nfe-usuar-estab.lg-cons-sefaz THEN
            ASSIGN l_permite_usu = YES. /* Permite que o Usuario continue o recebimento sem consulta no Sefaz - RZ */
    END.

    /* --- Verifica se a NFE foi Implantada Manual --- */
             
    IF nfe-dfe.sit-erp = 3 THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                                       +
                                 "Entrada no Recebimento nao permitida !,"  +
                                 "A Nota Fiscal Eletronica foi Implantada Manualmente.".

        LEAVE.
    END.

    /* --- Verifica se a NFE foi Cancelada --- */

    IF nfe-dfe.sit-sefaz = 3 THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                                       +
                                 "Entrada no Recebimento nao permitida !,"  +
                                 "A Nota Fiscal Eletronica foi Cancelada. " +
                                 "Existe XML de Cancelamento carregado para a Nota Fiscal Eletronica.".

        LEAVE.
    END.

    /* --- Verifica se a NFE esta autorizada --- */

    
    IF nfe-dfe.sit-sefaz = 2 THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                                       +
                                 "Entrada no Recebimento nao permitida !,"  +
                                 "A Nota Fiscal Eletronica N?O esta Autorizada.".

        LEAVE.
    END.

    /* --- Verifica Retorno Consulta Sefaz --- */

    
    FIND FIRST nfe-it-param-rec WHERE 
               nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
               nfe-it-param-rec.cod-item-parametro = "consulta_sefaz"            NO-LOCK NO-ERROR.

    FIND FIRST nfe-usuar-estab WHERE
               nfe-usuar-estab.cod-usuario = c-seg-usuario NO-LOCK.

    IF AVAIL nfe-it-param-rec                           AND
       nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN 
    DO:
        /* --- Dispara Consulta Sefaz para Telas do Recebimento FISICO/FISCAL --- */

    
        IF p_origem = 3 OR    /* Recebimento Fisico */
           p_origem = 4 THEN  /* Recebimento Fiscal */
        DO:

            RUN utp/ut-acomp.p PERSISTENT SET h-acomp-1.

            RUN pi-inicializar IN h-acomp-1 (INPUT "Carregando NFE!").

            RUN pi-acompanhar IN h-acomp-1 (INPUT "Aguarde!").
                
            RUN PI_LIMPA_RETURN.

            
            
            RUN pi_consulta_sefaz (INPUT  p_origem,
                                   INPUT  ROWID(nfe-nota-fiscal-rec),
                                   OUTPUT p_mensagem_erro).

            IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.

            RUN pi-acompanhar IN h-acomp-1 (INPUT "Aguarde Executando Consulta!").

            PAUSE 4 NO-MESSAGE.

            RUN pi-acompanhar IN h-acomp-1 (INPUT "Aguarde Finalizando Consulta!").

            PAUSE 4 NO-MESSAGE.

            RUN pi_valida_diretorios (OUTPUT p_mensagem_erro).
                
            IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.
                
            
            ASSIGN c-nome-dir-rec            = nfe-it-param-rec.valor-1-item-parametro  /* --- Entrada XML   --- */
                   c-nome-dir-rec-dest       = nfe-it-param-rec.valor-2-item-parametro  /* --- NFE Lidos     --- */
                   c-nome-dir-rec-sefaz-dest = nfe-it-param-rec.valor-3-item-parametro  /* --- Sefaz         --- */
                   c-nome-dir-erro           = nfe-it-param-rec.valor-5-item-parametro. /* --- NFE Erro      --- */
            
            RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec).
            RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec-dest).
            RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec-sefaz-dest).
            RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-erro).
                                                               
            ASSIGN c-chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe.

            ASSIGN c-file = nfe-nota-fiscal-rec.cgc                    + "_" + 
                            string(int(nfe-nota-fiscal-rec.ide-Serie)) + "_" + 
                            string(int(nfe-nota-fiscal-rec.ide-nnf))   + "_ret-nfe.xml" .

            RUN dsc\ra\esp\esnfe201b.p (INPUT c-nome-dir-rec,              /* --- Origem    --- */
                                        INPUT c-nome-dir-rec-sefaz-dest,   /* --- Destino   --- */
                                        INPUT c-nome-dir-erro,             /* --- Erro      --- */
                                        INPUT c-file,                      /* --- Arquivo   --- */
                                        INPUT-OUTPUT c-chave-acesso-nfe).  /* --- Chave NFe --- */

            RUN pi-finalizar IN h-acomp-1.
        END.


        /* --- Nao Autorizado pela Consulta Sefaz --- *//*aquii*/

        IF nfe-dfe.sit-sefaz < 4 THEN DO:
            
            IF nfe-usuar-estab.lg-cons-sefaz = NO THEN DO:
            
                FIND FIRST nfe-his-nota-fis-rec WHERE 
                           nfe-his-nota-fis-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso AND 
                           nfe-his-nota-fis-rec.ind-origem       = 2 /* --- Consulta Sefaz --- */   USE-INDEX ix_retorno NO-LOCK NO-ERROR.
    
                IF AVAIL nfe-his-nota-fis-rec THEN
                    ASSIGN p_mensagem_erro = "1," +
                                             "NFE nao autorizada pela SEFAZ.," + REPLACE(nfe-his-nota-fis-rec.desc-status,",",".").
                ELSE                  
                    ASSIGN p_mensagem_erro = "1,"                                           +
                                             "Nao foi possivel realizar Consulta Sefaz !,"  +
                                             "Tente novamente.".
                LEAVE.
            END.
            ELSE DO:
                MESSAGE "Certeza que Deseja Efetivar Entrada no Recebimento sem a consulta no SEFAZ ?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-consulta AS LOG. 
                
                IF l-consulta THEN DO:
                    FIND CURRENT nfe-dfe EXCLUSIVE-LOCK.
                    ASSIGN nfe-dfe.sit-erp = 4.
                    FIND CURRENT nfe-dfe NO-LOCK.
                END.
                ELSE LEAVE.
            END.           
        END.
        ELSE DO:
            FIND FIRST nfe-param-rec WHERE 
                       nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

            FIND FIRST nfe-it-param-rec WHERE 
                       nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                       nfe-it-param-rec.cod-item-parametro = "mensageria"                NO-LOCK NO-ERROR.

            IF AVAIL nfe-it-param-rec THEN DO:
                /* --- Consulta Sefaz Modelo DSCRC / Se habilita Validacoes Internas --- */

                IF nfe-it-param-rec.valor-1-item-parametro = "DSCRC" AND
                   nfe-it-param-rec.valor-3-item-parametro = "SIM"   THEN DO:
                    FIND FIRST nfe-his-nota-fis-rec WHERE 
                               nfe-his-nota-fis-rec.chave-acesso = nfe-nota-fiscal-rec.chave-acesso AND 
                               nfe-his-nota-fis-rec.ind-origem   = 2 /* --- Consulta Sefaz --- */   AND 
                               nfe-his-nota-fis-rec.cod-status   = "100"                            USE-INDEX ix_retorno NO-LOCK NO-ERROR.

                    IF AVAIL nfe-his-nota-fis-rec THEN DO:
                        FIND FIRST bf-nfe-his-nota-fis-rec WHERE 
                                   bf-nfe-his-nota-fis-rec.chave-acesso  = nfe-his-nota-fis-rec.chave-acesso AND 
                                   bf-nfe-his-nota-fis-rec.dt-retorno   >= nfe-his-nota-fis-rec.dt-retorno   AND 
                                   bf-nfe-his-nota-fis-rec.hr-retorno   >= nfe-his-nota-fis-rec.hr-retorno   AND 
                                   bf-nfe-his-nota-fis-rec.ind-origem    = 2 /* --- Consulta Sefaz --- */    AND 
                                   bf-nfe-his-nota-fis-rec.cod-status   BEGINS("DSC")                        USE-INDEX ix_retorno NO-LOCK NO-ERROR.

                        IF AVAIL bf-nfe-his-nota-fis-rec THEN DO:
                            /* --- Autoriza Entrada da Nota com Erros de Validacao --- */

                            MESSAGE "Certeza que Deseja Efetivar Entrada no Recebimento com Erros de Validacao no XML (DSCRC) ?"
                                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-entrada AS LOG.

                            IF NOT l-entrada THEN 
                            DO:
                                /* --- Elimina registro do sistema --- */

                                MESSAGE "Deseja Excluir a NFE do Sistema de Recebimento Automatico ?"
                                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-deleta AS LOG.

                                IF l-deleta THEN 
                                    RUN pi_exclui_registro (INPUT ROWID(nfe-nota-fiscal-rec)).
                                ELSE 
                                    RUN pi_atualiza_status (INPUT ROWID(nfe-nota-fiscal-rec), 
                                                            INPUT 2,
                                                            INPUT 0).

                                LEAVE.
                            END.
                        END.
                    END.
                END. /* IF nfe-it-param-rec.valor-1-item-parametro = "DSCRC" AND.... */
            END. /* IF AVAIL nfe-it-param-rec THEN.... */
        END. /* ELSE do IF nfe-nota-fiscal-rec.cod-situacao <> 4 THEN.... */
    END. /* IF AVAIL nfe-it-param-rec AND.... */

    /***********Classificacao Fiscal Rontan*****************/

    FIND FIRST nfe-it-param-rec no-lock
        WHERE nfe-it-param-rec.cod-parametro      = "PARAM_global" 
        AND   nfe-it-param-rec.cod-item-parametro = "valida_class_fiscal" NO-ERROR.


    IF AVAIL nfe-it-param-rec THEN DO:

        ASSIGN c-itens-erro-class = "".
        FOR EACH bf-nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK: 
    
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-ERROR.
    
            IF AVAIL ITEM THEN DO:
                IF ITEM.class-fiscal <> REPLACE(bf-nfe-it-nota-fisc-rec.item-NCM,".","") THEN 
                    ASSIGN c-itens-erro-class = c-itens-erro-class + " " + string(bf-nfe-it-nota-fisc-rec.seq-item) + ", ".
            END.
        END.    
    
        IF c-itens-erro-class <> "" THEN DO:
            IF nfe-it-param-rec.valor-1-item-parametro = "alerta" THEN DO:
                MESSAGE "ALERTA CADASTRO DE CLASSIFICACAO FISCAL!"  SKIP
                        "SEQUENCIAS:" + c-itens-erro-class + " ESTAO COM O CADASTRO DE CLASSIFICA??O FISCAL DIFERENTE DO XML, FAVOR ENTRAR EM CONTATO O DEPARTAMENTO FISCAL." SKIP(1)
                        "DESEJA EFETIVAR MESMO ASSIM?"                    
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-erro-class.
        
                IF l-erro-class = NO THEN 
                    RETURN NO-APPLY.
            END.                    

            IF nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN DO:
                MESSAGE "ALERTA CADASTRO DE CLASSIFICA??O FISCAL!"  SKIP
                        "SEQUENCIAS:" + c-itens-erro-class + " ESTAO COM O CADASTRO DE CLASSIFICA??O FISCAL DIFERENTE DO XML, FAVOR ENTRAR EM CONTATO O DEPARTAMENTO FISCAL." SKIP(1)
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    
                    RETURN NO-APPLY.
            END.                    
        END.                        
    END.                            
    
    /* --- Validacao Recebimento Fiscal --- */
    IF p_origem = 2 OR    /* Painel Recebimento Fiscal */
       p_origem = 4 THEN DO: /* Recebimento Fiscal        */
    
        /* Baione - Verificacao de preenchimento de Natureza de Operacao corretamente para NFes carregadas para o Recebimento Fiscal */
        DO WHILE l-retorna = YES:
            ASSIGN l-erro2 = NO.

            FOR EACH bf-nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK: /*renato-lock*/
                IF NOT CAN-FIND(FIRST natur-oper WHERE 
                                      natur-oper.nat-operacao = STRING(bf-nfe-it-nota-fisc-rec.item-nat-operacao) NO-LOCK) THEN DO:
                    ASSIGN l-erro2 = YES
                           c-erro  = c-erro       +
                                     "Sequencia " + STRING(bf-nfe-it-nota-fisc-rec.seq-item) + " - "  +
                                     "Natureza de Operacao Invalida ! " + CHR(10).
                END.
            END.
    
            IF l-erro2 = YES THEN DO: 
                ASSIGN p_mensagem_erro = "1,"     +
                                         c-erro   + "," +
                                         "Preencha o campo Natureza de Operacao corretamente. (15)".
    
                IF p_origem = 4 THEN /* Recebimento Fiscal */
                    RUN pi_chama_manutencao (INPUT ROWID(nfe-nota-fiscal-rec),
                                             INPUT-OUTPUT p_mensagem_erro,
                                             INPUT 2,  /* Fiscal */
                                             OUTPUT l-canc).
                ELSE do: 
                    l-canc = YES.
                    RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                          OUTPUT i-acao).
                    RETURN NO-APPLY.
    
                END.

                IF l-canc = YES THEN 
                DO:
                    ASSIGN l-retorna = NO.

                    LEAVE.
                END.
            END.
            ELSE 
                ASSIGN l-retorna = NO.
        END. /* DO WHILE l-retorna = YES.... */

        /*FIM - Baione - Verificacao de preenchimento de Natureza de Operacao corretamente para NFes carregadas para o Recebimento Fiscal*/

        ASSIGN l-retorna = YES.
    
        /* Baione - Verificacao de preenchimento de Classificacao Fiscal corretamente para NFes carregadas para o Recebimento Fiscal */

        DO WHILE l-retorna = YES:
            ASSIGN l-erro2 = NO.

            FOR EACH bf-nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK: /*renato-lock*/
                IF NOT CAN-FIND(FIRST classif-fisc WHERE 
                                      classif-fisc.class-fiscal = REPLACE(bf-nfe-it-nota-fisc-rec.item-NCM,".","") NO-LOCK) THEN 
                DO:
                    ASSIGN l-erro2 = YES
                           c-erro  = c-erro      +
                                    "Sequencia " + STRING(bf-nfe-it-nota-fisc-rec.seq-item) + " - "  +
                                    "Classificacao Fiscal Invalida ! " + CHR(10).
                END.
            END.
    
            IF l-erro2 = YES THEN 
            DO: 
                ASSIGN p_mensagem_erro = "1,"     +
                                         c-erro   + "," +
                                         "Preencha o campo Classificacao Fiscal corretamente. (16)".
    
                IF p_origem = 4 THEN /* Recebimento Fiscal */
                    RUN pi_chama_manutencao (INPUT ROWID(nfe-nota-fiscal-rec),
                                             INPUT-OUTPUT p_mensagem_erro,
                                             INPUT 2, /* Fiscal */
                                             OUTPUT l-canc).

                ELSE do: 
                    l-canc = YES.
                   
                    RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                          OUTPUT i-acao).

                    ASSIGN p_mensagem_erro = "".
                    RETURN NO-APPLY.
    
                END.


                IF l-canc = YES THEN 
                DO:
                    ASSIGN l-retorna = NO.

                    LEAVE.
                END.
            END.
            ELSE 
                ASSIGN l-retorna = NO.
        END. /* DO WHILE l-retorna = YES.... */

        /*FIM - Baione - Verificacao de preenchimento de Classificacao Fiscal corretamente para NFes carregadas para o Recebimento Fiscal*/

        ASSIGN l-retorna = YES.

        /* --- Verifica Permissao de Recebimento Fiscal --- */
        

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "recebimento_fiscal"        NO-LOCK NO-ERROR.

        IF (AVAIL nfe-it-param-rec                            AND
            nfe-it-param-rec.valor-1-item-parametro <> "SIM") OR
            NOT AVAIL nfe-it-param-rec                        THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                   +
                                     "Recebimento Fiscal nao permitido !,"  +
                                     "Nao permitido opcao de Recebimento Fiscal em Parametros do Recebimento NFE (esnfe400).".

            LEAVE.
        END.
        
        /* --- Verifica se a NFE ja foi Importada para o Recebimento Fisico --- */

        FIND FIRST nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK NO-ERROR.

         /*ponot fiscal*/
        RUN pi_limpa_epc.

        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocumEst"
               tt-epc.cod-parameter = "Chave docum-est separado ;".
               tt-epc.val-parameter = STRING(nfe-nota-fiscal-rec.ide-Serie)    + ';' +
                                      nfe-nota-fiscal-rec.ide-nNF              + ';' +
                                      string(emitente.cod-emitente)            + ';' +     
                                      nfe-it-nota-fisc-rec.item-nat-operacao   + ';' .
                   
        CREATE tt-epc.
            
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocumEst"
               tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).


        {include\i-epc201.i "ValidaExisteDocumEst"}
        /*ponot fiscal fim*/

        IF RETURN-VALUE = "NOK" THEN LEAVE.


        RUN pi_valida_tipo_nota_fisico (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                        OUTPUT i-tipo-nfe).

        RUN pi_limpa_epc.

        
        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocFisico"
               tt-epc.cod-parameter = "Chave doc-fisico separado ;".
               tt-epc.val-parameter = STRING(nfe-nota-fiscal-rec.ide-Serie)    + ';' +
                                      nfe-nota-fiscal-rec.ide-nNF              + ';' +
                                      string(emitente.cod-emitente)            + ';' +     
                                      nfe-it-nota-fisc-rec.item-nat-operacao   + ';' +
                                      string(i-tipo-nfe).
                   
        CREATE tt-epc.
            
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocFisico"
               tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).


        {include\i-epc201.i "ValidaExisteDocFisico"}

        IF RETURN-VALUE = "NOK" THEN LEAVE.

        FIND LAST doc-fisico WHERE 
                  doc-fisico.serie-docto  = STRING(nfe-nota-fiscal-rec.ide-Serie) AND 
                  doc-fisico.nro-docto    = nfe-nota-fiscal-rec.ide-nNF           AND 
                  doc-fisico.cod-emitente = emitente.cod-emitente                 AND 
                  doc-fisico.tipo-nota    = i-tipo-nfe                            NO-LOCK NO-ERROR.

        IF AVAIL doc-fisico THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                   +
                                     "Nota Fiscal Eletronica ja existe !,"  +
                                     "A Nota Fiscal Eletronica foi encontrada no Recebimento Fisico.".
            LEAVE.
        END.
        
        /* --- Verifica se a NFE ja foi Importada para o Recebimento Fiscal --- */
        IF nfe-dfe.sit-erp = 2 OR nfe-dfe.sit-erp = 3 THEN DO:
            ASSIGN p_mensagem_erro = "2,"                                   +
                                     "Nota Fiscal Eletronica duplicada !,"  +
                                     "A Nota Fiscal Eletronica ja esta como Implantada no Recebimento Fiscal.".
            LEAVE.
        END.
    END. /* IF p_origem = 2 OR.... */
    
    /* --- Validacao Recebimento Fisico --- */

    IF p_origem = 1 OR   /* Painel Recebimento Fisico */
       p_origem = 3 THEN /* Recebimento Fisico        */
    DO:
        /* Baione - Verificacao de Ordem de Compra aprovada pelo Responsavel */
        DO WHILE l-retorna = YES:
            ASSIGN l-erro2 = NO.

            FOR EACH bf-nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK: /*renato-lock*/
                FOR FIRST bf-ordem-compra FIELDS (bf-ordem-compra.it-codigo 
                                                  bf-ordem-compra.num-pedido 
                                                  bf-ordem-compra.situacao 
                                                  bf-ordem-compra.numero-ordem) NO-LOCK WHERE 
                          bf-ordem-compra.it-codigo  = bf-nfe-it-nota-fisc-rec.it-codigo AND 
                          bf-ordem-compra.num-pedido = bf-nfe-it-nota-fisc-rec.item-xPed AND 
                          bf-ordem-compra.situacao   = 2 /* --- Confirmada --- */ :
                    FOR FIRST bf-doc-pend-aprov FIELDS(bf-doc-pend-aprov.numero-ordem 
                                                       bf-doc-pend-aprov.ind-situacao) NO-LOCK WHERE 
                              bf-doc-pend-aprov.numero-ordem = bf-ordem-compra.numero-ordem AND  
                              (bf-doc-pend-aprov.ind-situacao = 1 OR bf-doc-pend-aprov.ind-situacao = 3):
                        ASSIGN l-erro2 = YES
                               c-erro  = c-erro       +
                                         "Sequencia " + STRING(bf-nfe-it-nota-fisc-rec.seq-item) + " - "  +
                                         "Ordem de Compra possui pend?ncia de aprovacao." + CHR(10).
                    END.
                END.
            END.
            
            IF l-erro2 = YES THEN 
            DO: 
                ASSIGN p_mensagem_erro = "1,"     +
                                         c-erro   + "," +
                                         "Verifique a aprovacao da(s) Ordem(ns) de Compra.".
    
                IF p_origem = 3 THEN /* Recebimento Fisico */
                    RUN pi_chama_manutencao (INPUT ROWID(nfe-nota-fiscal-rec),
                                             INPUT-OUTPUT p_mensagem_erro,
                                             INPUT 1, /* Fisico */
                                             OUTPUT l-canc).

                IF l-canc = YES THEN 
                DO:
                    ASSIGN l-retorna = NO.

                    LEAVE.
                END.
            END.
            ELSE 
                ASSIGN l-retorna = NO.
        END. /* DO WHILE l-retorna = YES.... */

        ASSIGN l-retorna = YES.

        
        /*Operacao Triangular*/  
        FOR EACH bf-nfe-it-nota-fisc-rec NO-LOCK
            WHERE bf-nfe-it-nota-fisc-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe :
            
            FIND FIRST natur-oper NO-LOCK
                WHERE natur-oper.nat-operacao = bf-nfe-it-nota-fisc-rec.item-nat-operacao NO-ERROR.
                
            IF AVAIL natur-oper THEN DO:
                
                IF   natur-oper.log-oper-triang = YES
                 AND natur-oper.baixa-estoq     = NO   THEN DO:
                    
                    ASSIGN p_mensagem_erro = "1,"                                   +
                                             "Recebimento Fisico nao permitido !,"  +
                                             "Nao permitido Recebimento de Nota Fiscal Operacao Triangular para o Recebimento Fisico.".

                    LEAVE.

                END.

            END.

        END.

        IF p_mensagem_erro <> "" THEN LEAVE.


        /* Baione - Verificacao de Ordem de Compra aprovada pelo Responsavel */
        
/*         IF CAN-FIND(FIRST nfe-relac-doc-nota-rec WHERE                                                                                   */
/*                           nfe-relac-doc-nota-rec.chave-acesso = nfe-nota-fiscal-rec.chave-acesso NO-LOCK) THEN DO:                       */
/*             ASSIGN p_mensagem_erro = "1,"                                   +                                                            */
/*                                      "Recebimento Fisico nao permitido !,"  +                                                            */
/*                                      "Nao permitido Relacionamento de Documento ou Nota Fiscal Referenciada para o Recebimento Fisico.". */
/*                                                                                                                                          */
/*             LEAVE.                                                                                                                       */
/*         END.                                                                                                                             */
/*Renato Nitroquimica                                                                                                                       */

        /* --- Verifica Permissao de Recebimento Fisico --- */

        FIND FIRST param-estoq NO-LOCK NO-ERROR.

        

        FIND FIRST nfe-param-rec NO-LOCK
            WHERE nfe-param-rec.cod-parametro = "param_global" NO-ERROR.


         

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "recebimento_fisico"        NO-LOCK NO-ERROR.
        
        

        IF (AVAIL nfe-it-param-rec                            AND
            nfe-it-param-rec.valor-1-item-parametro <> "SIM") OR
            NOT AVAIL nfe-it-param-rec                        OR
           (AVAIL param-estoq                                 AND
            NOT param-estoq.rec-fisico)                       THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                   +
                                     "Recebimento Fisico nao permitido !,"  +
                                     "Nao permitido opcao de Recebimento Fisico em Parametros do Recebimento NFE (esnfe400).".
            LEAVE.
        END.

        /* --- Verifica se a NFE ja foi Importada para o Recebimento Fiscal --- */

        
        /*ponot fiscal*/
        FIND FIRST nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK NO-ERROR.

        RUN pi_limpa_epc.

        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocumEst"
               tt-epc.cod-parameter = "Chave docum-est separado ;".
               tt-epc.val-parameter = STRING(nfe-nota-fiscal-rec.ide-Serie)    + ';' +
                                      nfe-nota-fiscal-rec.ide-nNF              + ';' +
                                      string(emitente.cod-emitente)            + ';' +     
                                      nfe-it-nota-fisc-rec.item-nat-operacao   + ';' .
                   
        CREATE tt-epc.
            
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocumEst"
               tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).


        {include\i-epc201.i "ValidaExisteDocumEst"}
        /*ponot fiscal fim*/

        IF RETURN-VALUE = "NOK" THEN LEAVE.
                
        
        /*Ponto Fisico*/
        FIND FIRST nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK NO-ERROR.

        RUN pi_valida_tipo_nota_fisico (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                        OUTPUT i-tipo-nfe).

        RUN pi_limpa_epc.

        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocFisico"
               tt-epc.cod-parameter = "Chave doc-fisico separado ;".
               tt-epc.val-parameter = STRING(nfe-nota-fiscal-rec.ide-Serie)    + ';' +
                                      nfe-nota-fiscal-rec.ide-nNF              + ';' +
                                      string(emitente.cod-emitente)            + ';' +     
                                      nfe-it-nota-fisc-rec.item-nat-operacao   + ';' +
                                      string(i-tipo-nfe).
                   
        CREATE tt-epc.
            
        ASSIGN tt-epc.cod-event     = "ValidaExisteDocFisico"
               tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).


        {include\i-epc201.i "ValidaExisteDocFisico"}
        /*Ponto Fisico fim*/

        
        IF RETURN-VALUE = "NOK" THEN LEAVE.

        FIND FIRST docum-est WHERE 
                   docum-est.serie-docto    = STRING(nfe-nota-fiscal-rec.ide-Serie)  AND 
                   docum-est.nro-docto      = nfe-nota-fiscal-rec.ide-nNF            AND 
                   docum-est.cod-emitente   = emitente.cod-emitente                  AND 
                   docum-est.nat-operacao   = nfe-it-nota-fisc-rec.item-nat-operacao NO-LOCK NO-ERROR.

        IF AVAIL docum-est THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                   +
                                     "Nota Fiscal Eletronica ja existe !,"  +
                                     "A Nota Fiscal Eletronica foi encontrada no Recebimento Fiscal.".

            LEAVE.
        END.

        /* --- Verifica se a NFE ja foi Importada para o Recebimento Fisico --- */

        IF nfe-dfe.sit-erp = 2 OR nfe-dfe.sit-erp = 3 THEN DO:
            ASSIGN p_mensagem_erro = "2,"                                   +
                                     "Nota Fiscal Eletronica duplicada !,"  +
                                     "A Nota Fiscal Eletronica ja esta como Implantada no Recebimento Fisico.".
            LEAVE.
        END.
    END. /* IF p_origem = 1 OR.... */

    IF p_origem = 3 OR   /* Recebimento Fisico        */
       p_origem = 4 THEN /* Recebimento Fiscal        */
    DO:
        /* --- Verifica Permissao de Retorna Fornecedor --- */

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "retorna_fornecedor"        NO-LOCK NO-ERROR.

        IF AVAIL nfe-it-param-rec                           AND
           nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN 
        DO:
            /* --- Retorna Fornecedor --- */

            RUN pi_retorna_fornecedor (INPUT 2, /* --- Telas (UPC) de Recebimento FISICO e FISCAL --- */
                                       INPUT ROWID(nfe-nota-fiscal-rec),
                                       OUTPUT p_mensagem_erro).

            IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.
        END.
    END.

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "BuscaDanfe"
           tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).

    {include\i-epc201.i "BuscaDanfe"}

    /* --- Verifica Erro de Validacao --- */

    IF RETURN-VALUE <> ""   AND
       RETURN-VALUE <> "OK"   
       /*RETURN-VALUE <> "Inicializacao..."  */ THEN 
    DO:
        ASSIGN p_mensagem_erro = RETURN-VALUE.

        LEAVE.
    END.

    /* --- Valida Entrada da Nota no Recebimento --- */

    RUN pi_valida_entrada (INPUT  ROWID(nfe-nota-fiscal-rec),
                           INPUT  IF p_origem = 1 OR p_origem = 3 THEN 1 ELSE 2,
                           OUTPUT p_mensagem_erro).

    IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.

    /* --- Verifica Observacao da Nota Fiscal --- */

    IF nfe-nota-fiscal-rec.cod-observa = 0 THEN 
    DO:
        
        
        /* --- Executa tela de Manutencao de Nota e Itens --- */

        RUN dsc\ra\esp\esnfe200d.w (INPUT  ROWID(nfe-nota-fiscal-rec),
                                    INPUT  IF p_origem = 1 OR p_origem = 3 THEN 1 ELSE 2,
                                    OUTPUT l-canc).

        IF l-canc THEN LEAVE.
    END.

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "EntradaRecebimento"
           tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).

    {include\i-epc201.i "EntradaRecebimento"}

    IF RETURN-VALUE = "NOK" THEN DO:
        RETURN NO-APPLY.
    END.
    
    DO WHILE l-retorna = YES: /* Tenta implantar a NFe se o usuario alterar quando tiver erros. */
        
        IF p_origem = 2 OR    /* Painel Recebimento Fiscal */
           p_origem = 4 THEN DO: /* Recebimento Fiscal        */
            RUN pi_limpa_epc.                                   
            CREATE tt-epc.                                      
            ASSIGN tt-epc.cod-event     = "EntradaRecebFiscal"
                   tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
                   tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).

            {include\i-epc201.i "EntradaRecebFiscal"}

            /* --- Se nao Efetivar entrada via EPC retorna NOK para executar rotina Padrao --- */

            IF c-executa-bonfe001 = "PADRAO" THEN DO:
                /* --- Verifica Inconsist?ncias da Nota --- */

                RUN pi_verifica_inconsist (INPUT ROWID(nfe-nota-fiscal-rec)). /* @EMA! - 1.04.016 */
    
                /* --- Efetiva Entrada da Nota no Recebimento FISCAL --- */
                
                RUN pi_entrada_receb_fiscal (INPUT ROWID(nfe-nota-fiscal-rec), OUTPUT p_mensagem_erro).
            END.
            ELSE DO:
                /* --- Verifica Erro de Validacao --- */

                IF RETURN-VALUE <> ""   AND
                   RETURN-VALUE <> "OK" THEN ASSIGN p_mensagem_erro = RETURN-VALUE.
            END.
        END. /* IF p_origem = 2 OR........*/
    
        IF p_origem = 1 OR   /* Painel Recebimento Fisico */
           p_origem = 3 THEN DO: /* Recebimento Fisico        */
            /*Nova Validacao Fisico duas naturezas*/
            
            FOR EACH nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK:
                
                RUN pi_valida_tipo_nota (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                         OUTPUT i-tipo-nfe,
                                         OUTPUT i-tp-oper-terc).
                    
                FIND FIRST tt-tipos-natur-fisico 
                    WHERE tt-tipos-natur-fisico.nat-operacao = nfe-it-nota-fisc-rec.item-nat-operacao
                    AND   tt-tipos-natur-fisico.tipo-nota    = i-tipo-nfe NO-ERROR.
                    
                IF NOT AVAIL tt-tipos-natur-fisico THEN DO:
                    
                    CREATE tt-tipos-natur-fisico.
                    ASSIGN tt-tipos-natur-fisico.nat-operacao = nfe-it-nota-fisc-rec.item-nat-operacao         
                           tt-tipos-natur-fisico.tipo-nota    = i-tipo-nfe .
                END.                                                        
            END.

            FIND FIRST nfe-it-param-rec NO-LOCK
                WHERE nfe-it-param-rec.cod-parametro      = "param_global"
                AND   nfe-it-param-rec.cod-item-parametro = "valida_duas_nat_fisico" NO-ERROR.
            
            IF NOT AVAIL nfe-it-param-rec
                OR nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN DO: /*se n’o houver o parametro ou estiver como SIM (Valida)*/

                FOR EACH tt-tipos-natur-fisico:
                    
                    FIND FIRST bf-tt-tipos-natur-fisico
                        WHERE bf-tt-tipos-natur-fisico.tipo         =  tt-tipos-natur-fisico.tipo
                        AND   bf-tt-tipos-natur-fisico.nat-operacao <> tt-tipos-natur-fisico.nat-operacao  NO-ERROR.
                        
                    IF  AVAIL bf-tt-tipos-natur-fisico THEN DO:
                        ASSIGN p_mensagem_erro = "1,"                                   +
                                                 "Nota Fiscal Eletronica bloqueada! Naturezas erradas Recebimento Fisico,"  +
                                                 "Existem Itens na Nota Fiscal Eletronica de Naturezas Diferentes e tipos iguais. Somente eh permitida a entrada de notas NO recebimento fisico de naturezas diferentes caso elas tenham os tipos diferentes(Compra e Retorno de Beneficiamento por exemplo). Verifique os itens com AS naturezas " + bf-tt-tipos-natur-fisico.nat-operacao + " e " + tt-tipos-natur-fisico.nat-operacao.
    
                        RETURN "nok".                          
                    END.                                       
                END.      
            

                FIND FIRST nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK NO-ERROR.
    
                IF AVAIL nfe-it-nota-fisc-rec THEN DO:
                    FOR EACH bf-nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec WHERE 
                             bf-nfe-it-nota-fisc-rec.item-nat-operacao <> nfe-it-nota-fisc-rec.item-nat-operacao NO-LOCK:
                        ASSIGN p_mensagem_erro = "1,"                                   +
                                                 "Nota Fiscal Eletronica bloqueada !,"  +
                                                 "Existem Itens na Nota Fiscal Eletronica de Naturezas Diferentes. Entrada permitida somente no Recebimento Fiscal.".
    
                        RETURN "nok".
                        /*LEAVE.*/
                    END.
                END.
            END.
            IF TRIM(p_mensagem_erro) <> "" THEN DO: 
                ASSIGN l-retorna = NO.
                LEAVE.
            END.
            
            /* --- Verifica se existe Item de servico --- */

            FIND FIRST nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec WHERE 
                       nfe-it-nota-fisc-rec.imp-iss-vAliq > 0 NO-LOCK NO-ERROR.

            IF AVAIL nfe-it-nota-fisc-rec THEN 
            DO:
                ASSIGN p_mensagem_erro = "1,"                                          +
                                         "Documento possui itens de servico (ISSQN).," +
                                         "Entrada nao permitida no Recebimento Fisico.".

                ASSIGN l-retorna = NO.

                LEAVE.
            END.
               
            RUN pi_limpa_epc.

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "EntradaRecebFisico"
                   tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
                   tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).

            {include\i-epc201.i "EntradaRecebFisico"}

            /* --- Se nao Efetivar entrada via EPC retorna NOK para executar rotina Padrao --- */

            IF c-executa-bonfe001 = "PADRAO" THEN 
            DO:
                /* --- Efetiva Entrada da Nota no Recebimento FISICO --- */

                RUN pi_entrada_receb_fisico (INPUT  ROWID(nfe-nota-fiscal-rec),
                                             OUTPUT p_mensagem_erro).
            END.
            ELSE 
            DO:
                /* --- Verifica Erro de Validacao --- */

                IF RETURN-VALUE <> ""   AND
                   RETURN-VALUE <> "OK" THEN ASSIGN p_mensagem_erro = RETURN-VALUE.
            END.
        END. /* IF p_origem = 1 OR.... */

        /* --- Se so a API retornar ERRO entao abre manutencao de Itens --- */

        IF TRIM(p_mensagem_erro)                <> ""   AND
           DECI(ENTRY(2, p_mensagem_erro, ",")) <> 132  THEN  /* --- Documento ja cadastrado para FISICO --- */
        DO:
            /* --- Atualiza Status SEFAZ da Nota --- */

            MESSAGE "Deseja alterar Dados da Nota Fiscal Eletronica ?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-altera AS LOG.

            IF l-altera THEN 
            DO:
                ASSIGN p_mensagem_erro = "".

                /* --- Executa tela de Manutencao de Nota e Itens --- */

                RUN dsc\ra\esp\esnfe200d.w (INPUT  ROWID(nfe-nota-fiscal-rec),
                                            INPUT  IF p_origem = 1 OR p_origem = 3 THEN 1 ELSE 2,
                                            OUTPUT l-canc).

                IF l-canc THEN 
                DO: 
                    ASSIGN l-retorna = NO.

                    LEAVE.                
                END.
            END.
        END.
        ELSE 
            ASSIGN l-retorna = NO.
    END. /* DO WHILE l-retorna = YES.... */

    ASSIGN p_mensagem_erro = "".

    
        
END PROCEDURE. /* pi_busca_danfe */

/**************************************************************************************************************************/

PROCEDURE pi_chama_manutencao:

    DEF INPUT        PARAM r-nfe-nota-fiscal-rec AS ROWID NO-UNDO.
    DEF INPUT-OUTPUT PARAM p_mensagem_erro       AS CHAR  NO-UNDO.
    DEF INPUT        PARAM p_rs_recebimento      AS INT   NO-UNDO.
    DEF OUTPUT       PARAM l-canc                AS LOG   NO-UNDO.
    
    RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                          OUTPUT i-acao).
                
    ASSIGN p_mensagem_erro = "".
    
    MESSAGE "Deseja alterar Dados da Nota Fiscal Eletronica ?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-altera AS LOG.

    IF l-altera THEN 
    DO:
        /* --- Executa tela de Manutencao de Nota e Itens --- */

        RUN dsc\ra\esp\esnfe200d.w (INPUT  r-nfe-nota-fiscal-rec,
                                    INPUT  p_rs_recebimento,
                                    OUTPUT l-canc).
    END.

END PROCEDURE. /* pi_chama_manutencao */

/**************************************************************************************************************************/

PROCEDURE pi_busca_deposito_cq:

    DEF INPUT  PARAM p_it_codigo         AS CHAR FORMAT "x(16)"         NO-UNDO.
    DEF INPUT  PARAM p_cod_estabel       AS CHAR FORMAT "x(03)"         NO-UNDO.
    DEF OUTPUT PARAM p_cod_depos_cq      AS CHAR FORMAT "x(03)"         NO-UNDO.

    /* --- Verifica se o Item tem Controle de CQ --- */

    FIND FIRST item-uni-estab WHERE 
               item-uni-estab.it-codigo    = p_it_codigo   AND 
               item-uni-estab.cod-estabel  = p_cod_estabel AND 
               item-uni-estab.contr-qualid = YES           NO-LOCK NO-ERROR.

    IF AVAIL item-uni-estab THEN 
    DO:
        FIND FIRST ITEM WHERE 
                   ITEM.it-codigo = item-uni-estab.it-codigo NO-LOCK NO-ERROR.

        IF AVAIL ITEM THEN 
        DO:
            /* --- Busca Deposito de CQ do Estabelecimento (Padrao Sistema DSC) --- */

            FIND FIRST estabelec WHERE 
                       estabelec.cod-estabel = item-uni-estab.cod-estabel NO-LOCK NO-ERROR.

            IF AVAIL estabelec THEN ASSIGN p_cod_depos_cq = estabelec.deposito-cq.
        END.
    END.

END PROCEDURE. /* pi_busca_deposito_cq */

/**************************************************************************************************************************/

PROCEDURE pi_carrega_xml:
    

    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL "" NO-UNDO.
    
    DEFINE VARIABLE c-tipo AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE h_esnfe207 AS HANDLE      NO-UNDO.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
        
    FIND FIRST nfe-param-rec WHERE
               nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

    
    IF AVAIL nfe-param-rec THEN
    DO:
        FIND FIRST nfe-it-param-rec OF nfe-param-rec WHERE
                   nfe-it-param-rec.cod-item-parametro = "banco" NO-LOCK NO-ERROR.

        IF NOT AVAIL nfe-it-param-rec THEN DO:
            RUN pi_mensagem_erro (INPUT "1," + 
                                        "Parametro global BANCO nao encontrado, utilizar ESNFE400 para cadastrar",
                                   OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
                
            RETURN NO-APPLY.


        END.
            
    END.


    RUN pi-inicializar IN h-acomp (INPUT "Carregando XML's").

    /* --- Valida Diretorios do Recebimento --- */

    RUN pi_valida_diretorios (OUTPUT p_mensagem_erro).

    IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.
        
    ASSIGN c-nome-dir-rec            = nfe-it-param-rec.valor-1-item-parametro  /* --- Entrada XML   --- */
           c-nome-dir-rec-dest       = nfe-it-param-rec.valor-2-item-parametro  /* --- NFE Lidos     --- */
           c-nome-dir-rec-sefaz-dest = nfe-it-param-rec.valor-3-item-parametro  /* --- Sefaz         --- */
           c-nome-dir-erro           = nfe-it-param-rec.valor-5-item-parametro. /* --- NFE Erro      --- */

    RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec).
    RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec-dest).
    RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec-sefaz-dest).
    RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-erro).
    
    EMPTY TEMP-TABLE tt-nfe-recebimento.

    INPUT FROM OS-DIR(c-nome-dir-rec).

    REPEAT:
        IMPORT c-file.

        IF INDEX(c-file,"xml") = 0 THEN NEXT.

        /* --- Se tiver Espaco em Branco no Nome XML retira espacos --- */

        IF INDEX(c-file," ") <> 0 THEN 
        DO:
            /* --- Renomeia para nome sem Espaco --- */

            OS-RENAME VALUE((c-nome-dir-rec + c-file)) VALUE(c-nome-dir-rec + REPLACE(c-file," ","")).

            /* --- Indica nome Novo sem Espacos --- */

            ASSIGN c-file = REPLACE(c-file," ","").
        END.

        IF SEARCH(c-nome-dir-rec + c-file) <> ? THEN 
        DO:
            /* --- Separa XMLs por Estabelecimento --- */

            FIND FIRST nfe-param-rec WHERE 
                       nfe-param-rec.cod-parametro = "diretorios_recebimento_nfe" NO-LOCK NO-ERROR.

            FIND FIRST nfe-it-param-rec WHERE 
                       nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                       nfe-it-param-rec.cod-item-parametro = "dir_xml_por_estab"         NO-LOCK NO-ERROR.


                            /* --- Verifica se ? cte u nfe--- */

                
                RUN dsc\ra\esp\esnfe201i.p (INPUT  c-nome-dir-rec,      /* --- Origem  --- */
                                            INPUT  c-file,              /* --- Arquivo --- */
                                            OUTPUT c-tipo).


            IF AVAIL nfe-it-param-rec                           AND
               nfe-it-param-rec.valor-1-item-parametro = "SIM"  AND
               c-tipo = "NFE"                                   THEN 
            DO:
                /* --- Separa XMLs por Estabelecimento --- */


                RUN dsc\ra\esp\esnfe201a.p (INPUT  c-file,          /* --- Arquivo --- */
                                            INPUT  c-nome-dir-rec,  /* --- Origem  --- */
                                            INPUT  c-nome-dir-erro, /* --- Erro    --- */
                                            OUTPUT l-erro).
                IF l-erro THEN NEXT.
            END.
            ELSE 
            DO:

                IF c-tipo = "NFE" THEN DO:

                   ASSIGN c-chave-acesso-nfe = "".
                    RUN dsc\ra\esp\esnfe201.p (INPUT  c-nome-dir-rec,      /* --- Origem  --- */
                                               INPUT  c-nome-dir-rec-dest, /* --- Destino --- */
                                               INPUT  c-nome-dir-erro,     /* --- Erro    --- */
                                               INPUT  c-file,              /* --- Arquivo --- */
                                               OUTPUT c-chave-acesso-nfe).

                END.

                IF c-tipo = "CTE" THEN DO:

                        Run dsc\ra\esp\esnfe201d.p (INPUT  c-nome-dir-rec,          
                                                    INPUT  c-nome-dir-rec-dest, 
                                                    INPUT  c-nome-dir-erro,     
                                                    INPUT  c-file,              
                                                    OUTPUT c-chave-acesso-nfe). 

                END.
                

                /* --- Tabela de Controle de XML --- */

                IF c-chave-acesso-nfe <> "" THEN 
                DO:
                    CREATE tt-nfe-recebimento.

                    ASSIGN tt-nfe-recebimento.chave-acesso-nfe = c-chave-acesso-nfe
                           tt-nfe-recebimento.tipo             = YES. /* Carrega xml */
                END.
            END.
        END. /* IF SEARCH(c-nome-dir-rec + c-file) <> ?..... */
    END. /* REPEAT */

    INPUT CLOSE.

    /* --- XMLs por Estabelecimento --- */

    FIND FIRST nfe-param-rec WHERE 
               nfe-param-rec.cod-parametro = "diretorios_recebimento_nfe" NO-LOCK NO-ERROR.

    FIND FIRST nfe-it-param-rec WHERE 
               nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
               nfe-it-param-rec.cod-item-parametro = "dir_xml_por_estab"         NO-LOCK NO-ERROR.

    IF AVAIL nfe-it-param-rec                           AND
       nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN 
    DO:
        /* --- Verifica Tabela de Usuario x Estabelecimento --- */

        FOR EACH nfe-usuar-estab WHERE 
                 nfe-usuar-estab.cod-usuario = c-seg-usuario NO-LOCK:

            /* --- Se existir diretorio --- */

            FILE-INFO:FILE-NAME = c-nome-dir-rec + nfe-usuar-estab.cod-estabel.

            IF FILE-INFO:FULL-PATHNAME <> ?      AND
               FILE-INFO:FILE-TYPE      = "DRW"  THEN 
            DO:
                /* --- Leitura de XMLs --- */

                INPUT FROM OS-DIR(c-nome-dir-rec + nfe-usuar-estab.cod-estabel).

                REPEAT:
                    IMPORT c-file.

                    IF INDEX(c-file,"xml") = 0 THEN NEXT.

                    /* --- Verifica se existe o XML no Estab que o Usuario tem permissao --- */

                    IF SEARCH(c-nome-dir-rec + nfe-usuar-estab.cod-estabel + "\" + c-file) <> ? THEN 
                    DO:
                        /* --- Carrega XMLs para tabelas RA --- */
                        
                        RUN dsc\ra\esp\esnfe201.p (INPUT (c-nome-dir-rec + nfe-usuar-estab.cod-estabel + "\"), /* --- Origem  --- */
                                                   INPUT  c-nome-dir-rec-dest,                                 /* --- Destino --- */
                                                   INPUT  c-nome-dir-erro,                                     /* --- Erro    --- */
                                                   INPUT  c-file,                                              /* --- Arquivo --- */
                                                   OUTPUT c-chave-acesso-nfe).

                        /* --- Tabela de Controle de XML --- */

                        IF c-chave-acesso-nfe <> "" THEN 
                        DO:
                            CREATE tt-nfe-recebimento.

                            ASSIGN tt-nfe-recebimento.chave-acesso-nfe = c-chave-acesso-nfe
                                   tt-nfe-recebimento.tipo             = YES. /* Carrega xml */
                        END.
                    END.
                END. /* REPEAT */
            END. /* IF FILE-INFO:FULL-PATHNAME <> ?.... */
        END. /* FOR EACH nfe-usuar-estab.... */
    END. /* IF AVAIL nfe-it-param-rec AND.... */

    /* --- Carrega XMLs de Cancelamento --- */

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando XMLs Cancelamento").

    INPUT FROM OS-DIR(c-nome-dir-rec).
        REPEAT:
            IMPORT c-file.

            IF INDEX(c-file,"xml") = 0 THEN NEXT.

            /* --- Se tiver Espaco em Branco no Nome XML retira espacos --- */

            IF INDEX(c-file," ") <> 0 THEN 
            DO:
                /* --- Renomeia para nome sem Espaco --- */

                OS-RENAME VALUE((c-nome-dir-rec + c-file)) VALUE(c-nome-dir-rec + REPLACE(c-file," ","")).

                /* --- Indica nome Novo sem Espacos --- */

                ASSIGN c-file = REPLACE(c-file," ","").
            END.

            IF SEARCH(c-nome-dir-rec + c-file) <> ? THEN 
            DO:
                /* --- Carrega XMLs de Cancelamento --- */

                RUN dsc\ra\esp\esnfe201c.p (INPUT c-nome-dir-rec,       /* --- Origem  --- */
                                            INPUT c-nome-dir-rec-dest,  /* --- Destino --- */
                                            INPUT c-nome-dir-erro,      /* --- Erro    --- */
                                            INPUT c-file).              /* --- Arquivo --- */
            END.
        END. /* REPEAT */
    INPUT CLOSE.

    /* --- Leitura de XML do Retorno Sefaz --- */

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando XMLs de Retorno Sefaz").

    INPUT FROM OS-DIR(c-nome-dir-rec).
        REPEAT:
            IMPORT c-file.

            IF INDEX(c-file,"xml") = 0 THEN NEXT.

            /* --- Se tiver Espaco em Branco no Nome XML retira espacos --- */

            IF INDEX(c-file," ") <> 0 THEN 
            DO:
                /* --- Renomeia para nome sem Espaco --- */

                OS-RENAME VALUE((c-nome-dir-rec + c-file)) VALUE(c-nome-dir-rec + REPLACE(c-file," ","")).

                /* --- Indica nome Novo sem Espacos --- */

                ASSIGN c-file = REPLACE(c-file," ","").
            END.

            IF SEARCH(c-nome-dir-rec + c-file) <> ? THEN 
            DO:
                /* --- Leitura de XML do Retorno Sefaz --- */

                ASSIGN c-chave-acesso-nfe = "".

                RUN dsc\ra\esp\esnfe201b.p (INPUT c-nome-dir-rec,               /* --- Origem    --- */
                                            INPUT c-nome-dir-rec-sefaz-dest,    /* --- Destino   --- */
                                            INPUT c-nome-dir-erro,              /* --- Erro      --- */
                                            INPUT c-file,                       /* --- Arquivo   --- */
                                            INPUT-OUTPUT c-chave-acesso-nfe).   /* --- Chave NFe --- */

                /* --- Tabela de Controle de XML --- */

                IF c-chave-acesso-nfe <> "" THEN 
                DO:
                    CREATE tt-nfe-recebimento.

                    ASSIGN tt-nfe-recebimento.chave-acesso-nfe = c-chave-acesso-nfe
                           tt-nfe-recebimento.tipo             = NO . /* Consulta xml */
                END.


            END.
        END. /* REPEAT */
    INPUT CLOSE.

    /* Executa apenas quando estiver carregando xml - Quando for somente consulta sefaz nao atualiza nada */
    FOR EACH tt-nfe-recebimento WHERE
             tt-nfe-recebimento.tipo = YES /* carrega xml */ NO-LOCK:
        

        /* Leia - 10/04/14 - Atualizar CTE */

        FIND nfe-cte-inf WHERE
             nfe-cte-inf.chave-acesso = tt-nfe-recebimento.chave-acesso NO-LOCK NO-ERROR.

        IF AVAIL nfe-cte-inf THEN DO:
        
            FIND FIRST nfe-dfe NO-LOCK
                WHERE nfe-dfe.chave-acesso = nfe-cte-inf.chave-acesso NO-ERROR.

            FIND FIRST emitente WHERE 
                       emitente.cgc = nfe-cte-inf.emit-id NO-LOCK NO-ERROR.
        
            IF AVAIL emitente THEN 
            DO:
               IF CAN-FIND(FIRST docum-est WHERE 
                                 docum-est.serie-docto     = nfe-cte-inf.ide-serie   AND 
                                 docum-est.nro-docto       = nfe-cte-inf.ide-nCT     AND 
                                 docum-est.cod-emitente    = emitente.cod-emitente   AND 
                                 docum-est.cod-estabel     = nfe-cte-inf.cod-estabel NO-LOCK) THEN DO:
    
                    FIND CURRENT nfe-dfe EXCLUSIVE-LOCK.
                    ASSIGN nfe-dfe.sit-erp = 3. /* --- Implantada Manual --- */
                    FIND CURRENT nfe-dfe NO-LOCK.
               END.
            END.
        END.
        /*Fim Leia*/
        
        FIND FIRST nfe-nota-fiscal-rec no-lock
            WHERE nfe-nota-fiscal-rec.chave-acesso = tt-nfe-recebimento.chave-acesso NO-ERROR.

        FIND FIRST nfe-dfe NO-LOCK
            WHERE nfe-dfe.chave-acesso = nfe-nota-fiscal-rec.chave-acesso-nfe NO-ERROR.
        
	    FOR EACH nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec EXCLUSIVE-LOCK:           
            RUN pi_limpa_epc.
            CREATE tt-epc.
            ASSIGN tt-epc.cod-event     = "DuranteCarregaXML"
                   tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
                   tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).
    
            {include\i-epc201.i "DuranteCarregaXML"}
    
            FIND emitente WHERE 
                 emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.
    
            IF AVAIL emitente THEN DO:
                /* --- Aplica Fator de Conversao na Quantidade --- */
                FIND FIRST item-fornec WHERE 
                           item-fornec.cod-emitente = emitente.cod-emitente           AND 
                           item-fornec.item-do-forn = nfe-it-nota-fisc-rec.item-cprod NO-LOCK NO-ERROR.
    
                IF AVAIL item-fornec THEN DO:
                    /*** MOD.01 - Baione - 07/06/2011 ***/
    
                    IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN 
                    DO:
                        CREATE nfe-item-fornec.
    
                        BUFFER-COPY item-fornec TO nfe-item-fornec. 
                    END.
    
                    FIND FIRST nfe-item-fornec OF item-fornec NO-LOCK NO-ERROR.
    
                    ASSIGN de-fat-conv                      = nfe-item-fornec.fator-conver * EXP(10,(nfe-item-fornec.num-casa-dec * (-1)))
                           nfe-it-nota-fisc-rec.item-qtde   = nfe-it-nota-fisc-rec.item-qCom / de-fat-conv
                           nfe-it-nota-fisc-rec.it-codigo   = nfe-item-fornec.it-codigo
                           nfe-it-nota-fisc-rec.item-vUnCom = nfe-it-nota-fisc-rec.item-vProd / nfe-it-nota-fisc-rec.item-qtde
                           nfe-it-nota-fisc-rec.item-uCom   = nfe-item-fornec.unid-med-for.
    
                    /* --- Aplica Unidade de Medida do Item / Localizacao / Deposito --- */
    
                    FIND FIRST ITEM WHERE
                               item.it-codigo = nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.

                    IF AVAIL ITEM THEN
                        ASSIGN nfe-it-nota-fisc-rec.item-un          = ITEM.un .
                               /*
                               nfe-it-nota-fisc-rec.item-cod-localiz = ITEM.cod-localiz
                               nfe-it-nota-fisc-rec.item-cod-depos   = ITEM.deposito-pad.*/

                END. /* IF AVAIL item-fornec.... */
    
                
    
/*                 FIND FIRST item-uni-estab WHERE                                                            */
/*                            item-uni-estab.it-codigo    = nfe-it-nota-fisc-rec.it-codigo  AND               */
/*                            item-uni-estab.cod-estabel  = nfe-nota-fiscal-rec.cod-estabel NO-LOCK NO-ERROR. */
/*                                                                                                            */
/*                 IF AVAIL item-uni-estab THEN                                                               */
/*                     ASSIGN nfe-it-nota-fisc-rec.item-cod-localiz = item-uni-estab.cod-localiz              */
/*                            nfe-it-nota-fisc-rec.ITEM-cod-depos   = item-uni-estab.deposito-pad.            */
                           

                /* --- Busca Localizacao --- */
                RUN dsc/ra/esp/esra010.p(INPUT  nfe-it-nota-fisc-rec.it-codigo,
                                         INPUT  nfe-nota-fiscal-rec.cod-estabel,
                                         INPUT  0,
                                         OUTPUT nfe-it-nota-fisc-rec.item-cod-depos, 
                                         OUTPUT nfe-it-nota-fisc-rec.item-cod-localiz  ).
            
                /* --- Busca Relacionamento Emitente x CFOP x Natureza --- */
    
                FIND FIRST nfe-emit-cfop-nat WHERE 
                           nfe-emit-cfop-nat.cod-estabel  = nfe-nota-fiscal-rec.cod-estabel        AND 
                           nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                  AND 
                           nfe-emit-cfop-nat.cod-cfop     = STRING(nfe-it-nota-fisc-rec.item-CFOP) AND 
                           nfe-emit-cfop-nat.it-codigo    = nfe-it-nota-fisc-rec.it-codigo         NO-LOCK NO-ERROR.
    
                IF NOT AVAIL nfe-emit-cfop-nat THEN 
                DO:
                    FIND FIRST nfe-emit-cfop-nat WHERE 
                               nfe-emit-cfop-nat.cod-estabel  = nfe-nota-fiscal-rec.cod-estabel        AND 
                               nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                  AND 
                               nfe-emit-cfop-nat.cod-cfop     = STRING(nfe-it-nota-fisc-rec.item-CFOP) AND 
                               nfe-emit-cfop-nat.it-codigo    = "*"                                    NO-LOCK NO-ERROR.
                END.
    
                IF AVAIL nfe-emit-cfop-nat                           AND
                   TRIM(nfe-it-nota-fisc-rec.item-nat-operacao) = "" THEN 
                DO:
                    FIND FIRST natur-oper WHERE 
                               natur-oper.nat-operacao = nfe-emit-cfop-nat.nat-operacao NO-LOCK NO-ERROR.
    
                    IF AVAIL natur-oper THEN DO:
                        ASSIGN nfe-it-nota-fisc-rec.item-nat-operacao = natur-oper.nat-operacao
                               nfe-it-nota-fisc-rec.item-esp-nat      = natur-oper.especie-doc.
                        
                        FIND emitente WHERE 
                             emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.
                    END.
                END.
            END. /* IF AVAIL emitente.... */
    
            /* --- Retona o Tipo de Nota de acordo com a Natureza --- */
    
            RUN pi_valida_tipo_nota (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                     OUTPUT i-tipo-nfe,
                                     OUTPUT i-tp-oper-terc).
    
            IF i-tipo-nfe = 0 THEN
            DO:
                ASSIGN p_mensagem_erro = "1,"                                     +
                                         "Natureza de operacao nao encontrada !," +
                                         "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_CARREGA_XML)".
    
                RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                      OUTPUT i-acao).
            END.
    
            /* --- Aplica Parametros do Sistema --- */
    
            FIND FIRST nfe-param-rec WHERE 
                       nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.
    
            IF AVAIL nfe-param-rec THEN DO:
                /* --- Verifica se e Padrao Anfavea para Pedido do Item --- */
    
                FIND FIRST nfe-it-param-rec WHERE 
                           nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                           nfe-it-param-rec.cod-item-parametro = "padrao_anfavea"            NO-LOCK NO-ERROR.
    
                IF AVAIL nfe-it-param-rec                           AND
                   nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN 
                DO:
                    ASSIGN c-aux  = ""
                           i-aux2 = 0.
    
                    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,"ped=") <> 0 THEN 
                    DO:
                        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,"ped=") + 4) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
                            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN ASSIGN i-aux2 = i-aux2 + 1.
    
                            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).
    
                            IF i-aux2 = 2 THEN 
                            DO:
                                ASSIGN i-aux2 = 0
                                       c-aux  = REPLACE(c-aux,'"','').
    
                                LEAVE.
                            END.
                        END.
                    END.
            
                    /* --- Verifica Pedido EMS --- */
    
                    ASSIGN i-aux = INTE(c-aux) NO-ERROR.
    
                    IF ERROR-STATUS:ERROR = NO THEN 
                    DO:
                        IF CAN-FIND(FIRST pedido-compr WHERE 
                                          pedido-compr.num-pedido = INTE(c-aux) NO-LOCK) THEN 
                            ASSIGN nfe-it-nota-fisc-rec.item-xPed = INTE(c-aux).
                    END.
        
                    RUN pi_limpa_epc.
    
                    CREATE tt-epc.
    
                    ASSIGN tt-epc.cod-event     = "ValidaPadraoAnfavea"
                           tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
                           tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).
    
                    CREATE tt-epc.
    
                    ASSIGN tt-epc.cod-event     = "ValidaPadraoAnfavea"
                           tt-epc.cod-parameter = "pedido"
                           tt-epc.val-parameter = c-aux.
    
                    {include\i-epc201.i "ValidaPadraoAnfavea"}
                        
                    ASSIGN c-aux = "".
                END. /* IF AVAIL nfe-it-param-rec AND... */
    
                /* --- Verifica Parametro de troca Serie --- */
    

                
                FIND FIRST nfe-it-param-rec WHERE 
                           nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                           nfe-it-param-rec.cod-item-parametro = "troca_serie"               NO-LOCK NO-ERROR.
    
                IF AVAIL nfe-it-param-rec                           AND
                   nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN 
                DO:
                    IF TRIM(nfe-nota-fiscal-rec.ide-Serie) = "0" THEN DO:
                        FIND CURRENT nfe-nota-fiscal-rec EXCLUSIVE-LOCK.
                        ASSIGN nfe-nota-fiscal-rec.ide-Serie = "".
                        FIND CURRENT nfe-nota-fiscal-rec NO-LOCK.
                    END.
                END.
            END.
    
            CASE i-tipo-nfe:
                WHEN 1 THEN RUN pi_nfe_1_compra.        /* --- NFE de COMPRA                    --- */ 
                WHEN 2 THEN RUN pi_nfe_2_devol_cli.     /* --- NFE de DEVOLUCAO CLIENTE         --- */
                WHEN 3 THEN RUN pi_nfe_3_retorno_terc.  /* --- NFE de RETORNO TERCEIROS         --- */
                WHEN 4 THEN RUN pi_nfe_4_remessa_terc.  /* --- NFE de REMESSA TERCEIROS         --- */
            END.     
                     
            /* --- Busca Conta Contabil da Ordem ou Item - somente se item diferente de BRANCO --- */
    
            IF TRIM(nfe-it-nota-fisc-rec.it-codigo) <> "" THEN 
            DO:
                FIND FIRST ordem-compra WHERE 
                           ordem-compra.numero-ordem = nfe-it-nota-fisc-rec.item-num-ordem AND 
                           ordem-compra.it-codigo    = nfe-it-nota-fisc-rec.it-codigo      AND 
                           ordem-compra.situacao     = 2 /* --- Confirmada --- */          NO-LOCK NO-ERROR.
    
                IF AVAIL ordem-compra THEN DO:
                    &IF '{&bf_dis_versao_ems}' <= '2.07' &THEN 
                       ASSIGN nfe-it-nota-fisc-rec.item-conta-contabil = ordem-compra.conta-contabil .
                            /*nfe-it-nota-fisc-rec.item-cod-depos      = ordem-compra.dep-almoxar.*/

                       &ELSE 
                          ASSIGN nfe-it-nota-fisc-rec.item-conta-contabil = ordem-compra.ct-codigo.
                    
                    &ENDIF
                    
                    /* --- Busca Localizacao --- */
                    
                    RUN dsc/ra/esp/esra010.p(INPUT  nfe-it-nota-fisc-rec.it-codigo,
                                             INPUT  nfe-nota-fiscal-rec.cod-estabel,
                                             INPUT  ordem-compra.numero-ordem,
                                             OUTPUT nfe-it-nota-fisc-rec.item-cod-depos , 
                                             OUTPUT nfe-it-nota-fisc-rec.item-cod-localiz,
                                             INPUT  ROWID(nfe-it-nota-fisc-rec)).


    
                    FIND FIRST prazo-compra WHERE 
                               prazo-compra.numero-ordem                       = nfe-it-nota-fisc-rec.item-num-ordem AND 
                               (prazo-compra.quant-saldo - prazo-compra.dec-1) > nfe-it-nota-fisc-rec.item-qtde      NO-LOCK NO-ERROR.
    
                    IF AVAIL prazo-compra THEN 
                        ASSIGN nfe-it-nota-fisc-rec.item-parcela-oc = prazo-compra.parcela.
                END.
                ELSE 
                DO:
                    FIND FIRST ITEM WHERE 
                               ITEM.it-codigo = nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.
    
                    IF AVAIL ITEM           AND
                      (ITEM.tipo-contr = 1  OR   /* --- FISICO        --- */
                       ITEM.tipo-contr = 4) THEN /* --- DEBITO DIRETO --- */

                       &IF '{&bf_dis_versao_ems}' <= '2.07' &THEN 
                         ASSIGN nfe-it-nota-fisc-rec.item-conta-contabil = ITEM.conta-aplicacao.

                       &ELSE 
                          ASSIGN nfe-it-nota-fisc-rec.item-conta-contabil = ITEM.ct-codigo.
                    
                       &ENDIF
                END.
            END.
            
            /* --- Busca Dep«sito de Controle de Qualidade --- */

            /*customizacao Metalfrio Sera trocada de lugar*/
/*             DEFINE VARIABLE l-item-pref-excelente AS LOGICAL     NO-UNDO.                  */
/*                                                                                            */
/*             ASSIGN l-item-pref-excelente = NO.                                             */
/*             FIND FIRST item-fornec NO-LOCK                                                 */
/*                 WHERE item-fornec.cod-emitente = emitente.cod-emitente                     */
/*                 AND   item-fornec.item-do-forn = nfe-it-nota-fisc-rec.item-cprod NO-ERROR. */
/*                                                                                            */
/*             IF AVAIL item-fornec THEN DO:                                                  */
/*                                                                                            */
/*                 IF item-fornec.tp-insp = 6 THEN DO:                                        */
/*                     ASSIGN l-item-pref-excelente = YES.                                    */
/*                                                                                            */
/*                 END.                                                                       */
/*                                                                                            */
/*             END.                                                                           */
/*                                                                                            */
/*             IF l-item-pref-excelente = NO THEN DO:                                         */
/*                                                                                            */
/*                 RUN pi_busca_deposito_cq (INPUT nfe-it-nota-fisc-rec.it-codigo,            */
/*                                           INPUT nfe-nota-fiscal-rec.cod-estabel,           */
/*                                           OUTPUT c-aux).                                   */
/*                                                                                            */
/*                 IF TRIM(c-aux) <> "" THEN                                                  */
/*                 DO:                                                                        */
/*                     FIND FIRST deposito WHERE                                              */
/*                                deposito.cod-depos = c-aux NO-LOCK NO-ERROR.                */
/*                                                                                            */
/*                     IF AVAIL deposito THEN                                                 */
/*                         ASSIGN nfe-it-nota-fisc-rec.item-cod-depos = deposito.cod-depos.   */
/*                 END.                                                                       */
/*                                                                                            */
/*             END.                                                                           */
    
            /* --- Verifica se existe XML de Cancelamento para a Nota --- */
    
            IF CAN-FIND(FIRST nfe-nota-canc-rec WHERE 
                              nfe-nota-canc-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe NO-LOCK) THEN DO:
                FIND CURRENT nfe-dfe EXCLUSIVE-LOCK.
                ASSIGN nfe-dfe.sit-sefaz = 3. /* --- Cancelada --- */
                FIND CURRENT nfe-dfe NO-LOCK.
            END.
    
            /* --- Verifica se a Nota foi Implantada Manual --- */
    
            IF CAN-FIND(FIRST doc-fisico WHERE 
                              doc-fisico.serie-docto   = nfe-nota-fiscal-rec.ide-Serie   AND 
                              doc-fisico.nro-docto     = nfe-nota-fiscal-rec.ide-nNF     AND 
                              doc-fisico.cod-emitente  = emitente.cod-emitente           AND 
                              doc-fisico.cod-estabel   = nfe-nota-fiscal-rec.cod-estabel NO-LOCK) THEN DO:
                FIND CURRENT nfe-dfe EXCLUSIVE-LOCK.
                ASSIGN nfe-dfe.sit-erp = 3. /* --- Implantada Manual --- */
                FIND CURRENT nfe-dfe NO-LOCK.
            END.
    
            IF CAN-FIND(FIRST docum-est WHERE 
                              docum-est.serie-docto     = nfe-nota-fiscal-rec.ide-Serie   AND 
                              docum-est.nro-docto       = nfe-nota-fiscal-rec.ide-nNF     AND 
                              docum-est.cod-emitente    = emitente.cod-emitente           AND 
                              docum-est.cod-estabel     = nfe-nota-fiscal-rec.cod-estabel NO-LOCK) THEN DO:
                FIND CURRENT nfe-dfe EXCLUSIVE-LOCK.
                ASSIGN nfe-dfe.sit-erp = 3. /* --- Implantada Manual --- */
                FIND CURRENT nfe-dfe NO-LOCK.
            END.
            
            RUN pi_limpa_epc.
    
            CREATE tt-epc.
    
            ASSIGN tt-epc.cod-event     = "DepoisCarregaXML"
                   tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
                   tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).
    
            {include\i-epc201.i "DepoisCarregaXML"}
        END.
    
    END. /* FOR EACH tt-nfe-recebimento...... */

    RUN pi-finalizar IN h-acomp.

END PROCEDURE. /* pi_carrega_xml */

/**************************************************************************************************************************/

PROCEDURE pi_consulta_sefaz:
    
    DEF INPUT  PARAM p_origem        AS INTE                             NO-UNDO.
    DEF INPUT  PARAM p_rowid_danfe   AS ROWID                            NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    DEF VAR i-aux-cont AS INTE NO-UNDO.
    DEFINE VARIABLE l-erro-consumo AS LOGICAL     NO-UNDO.

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "ConsultaSefaz"
           tt-epc.cod-parameter = "p_rowid_danfe(ROWID)"
           tt-epc.val-parameter = STRING(p_rowid_danfe).

    {include\i-epc201.i "ConsultaSefaz"}

    /* --- Se nao Consulta Sefaz via EPC executa Rotina Padrao --- */

    IF c-executa-bonfe001 = "PADRAO" THEN 
    DO:
        /* --- Parametros Globais --- */

        FIND FIRST nfe-param-rec WHERE 
                   nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "mensageria"                NO-LOCK NO-ERROR.

        IF AVAIL nfe-it-param-rec THEN 
        DO:
            /* --- Tempo de Espera de Resposta Consulta Sefaz --- */

            ASSIGN i-aux2 = INTE(nfe-it-param-rec.valor-2-item-parametro).

            /*** Validacao retirada por solicitacao do Elias - 28/06/2011
            /* --- Verifica se a NFE ja foi Importada para o Recebimento --- */
            FIND FIRST bf-nfe-nota-fiscal-rec WHERE 
                       ROWID(bf-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.
                       
            IF AVAIL bf-nfe-nota-fiscal-rec THEN 
            DO:
                IF bf-nfe-nota-fiscal-rec.cod-situacao = 6  OR       /* --- Implantada          --- */
                   bf-nfe-nota-fiscal-rec.cod-situacao = 7  THEN     /* --- Implantada Manual   --- */
                DO:
                    ASSIGN p_mensagem_erro = "1,"                                   +
                                             "Nota Fiscal Eletronica Implantada !," +
                                             "A Nota Fiscal Eletronica ja esta como Implantada no Recebimento. Nao ? permitido disparar a Consulta Sefaz.".
                    LEAVE.
                END.
            END.
            ***/

            /* --- Consulta Sefaz Modelo DSCRC --- */
            /*Sera feito via xml de consulta para o DSCRC tambem*/
/*             IF nfe-it-param-rec.valor-1-item-parametro = "DSCRC" THEN                      */
/*             DO:                                                                            */
/*                 /* --- Copia XML do Fornecedor para DSCRC disparando Consulta Sefaz --- */ */
/*                                                                                            */
/*                 RUN pi_copia_xml_dscrc (INPUT  p_rowid_danfe,                              */
/*                                         OUTPUT p_mensagem_erro).                           */
/*                                                                                            */
/*                 IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.                                 */
/*             END.                                                                           */
/*             ELSE                                                                           */
/*             DO:                                                                            */
                /* --- Consulta Sefaz Modelo PADRAO     --- */
                /* --- Valida Diret«rios do Recebimento --- */

                RUN pi_valida_diretorios (OUTPUT p_mensagem_erro).

                IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.
                    
                FIND FIRST bf-nfe-nota-fiscal-rec WHERE 
                           ROWID(bf-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

                IF AVAIL bf-nfe-nota-fiscal-rec THEN 
                DO:
                    ASSIGN c-diretorio = nfe-it-param-rec.valor-3-item-parametro /* --- Consulta Sefaz --- */
                           c-arquivo   = bf-nfe-nota-fiscal-rec.chave-acesso-nfe.
                
                    RUN pi_valida_barra (INPUT-OUTPUT c-diretorio).
                
                    /* --- Verifica se Diretorio Sefaz e por ESTAB --- */

                    FIND FIRST nfe-it-param-rec WHERE 
                               nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                               nfe-it-param-rec.cod-item-parametro = "dir_sefaz_por_estab"       NO-LOCK NO-ERROR.

                    IF AVAIL nfe-it-param-rec                          AND
                       nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN 
                    DO:
                        ASSIGN c-aux = c-diretorio + bf-nfe-nota-fiscal-rec.cod-estabel + "\".

                        /* --- Verifica se Diretorio Existe --- */

                        FILE-INFO:FILE-NAME = c-aux.

                        IF FILE-INFO:FULL-PATHNAME = ?     AND
                           FILE-INFO:FILE-TYPE    <> "DRW" THEN 
                        DO:
                            /* --- Cria Diretorio Estabelecimento --- */

                            OS-CREATE-DIR VALUE(c-aux).

                            ASSIGN i-erro = OS-ERROR.

                            IF i-erro <> 0 THEN DO:
                                MESSAGE "Diretorio " bf-nfe-nota-fiscal-rec.cod-estabel " nao foi criado. Erro do sistema #" i-erro
                                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.

                                LEAVE.
                            END.
                        END.

                        ASSIGN c-diretorio = c-aux.
                    END. /* IF AVAIL nfe-it-param-rec AND... */
                
                    /* --- Cabecalho --- */

                    hXMLCon:CREATE-NODE(hRootCon, "consSitNFe", "ELEMENT":U).

                    hXMLCon:APPEND-CHILD(hRootCon).

                    hRootCon:SET-ATTRIBUTE ("versao", "2.00").

                    hRootCon:SET-ATTRIBUTE ("xmlns", "http://www.portalfiscal.inf.br/nfe").
                
                    ASSIGN c-lista = "tpAmb|xServ|chNFe"
                           c-valor = "1"                                + "|" +
                                     "CONSULTAR"                        + "|" +
                                     bf-nfe-nota-fiscal-rec.chave-acesso.

                    DO i-aux = 1 TO 3:
                        hXMLCon:CREATE-NODE(hRecordAux, ENTRY(i-aux,c-lista,'|'), "ELEMENT":U).

                        hRootCon:APPEND-CHILD(hRecordAux).
                
                        hXMLCon:CREATE-NODE(hRecordAuxValue, ENTRY(i-aux,c-lista,'|'), "TEXT":U).

                        hRecordAux:APPEND-CHILD(hRecordAuxValue).

                        hRecordAuxValue:NODE-VALUE = ENTRY(i-aux,c-valor,'|').
                    END.
                        
                    hXMLCon:SAVE("FILE":U, c-diretorio + c-arquivo + "-nfe-ped-sit" + ".xml").
                        
                    DELETE OBJECT hRootCon.

                    DELETE OBJECT hXMLCon.
                END. /* IF AVAIL bf-nfe-nota-fiscal-rec..... */
                ELSE DO:

                    /*Consulta CTE*/
                    FIND FIRST nfe-cte-inf WHERE 
                           ROWID(nfe-cte-inf) = p_rowid_danfe NO-LOCK NO-ERROR.

                    IF avail(nfe-cte-inf) THEN DO:
                    
                        ASSIGN c-diretorio = nfe-it-param-rec.valor-3-item-parametro /* --- Consulta Sefaz --- */
                               c-arquivo   = nfe-cte-inf.chave-acesso.
                    
                        RUN pi_valida_barra (INPUT-OUTPUT c-diretorio).
                    
                        /* --- Verifica se Diretorio Sefaz e por ESTAB --- */
    
                        FIND FIRST nfe-it-param-rec WHERE 
                                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                                   nfe-it-param-rec.cod-item-parametro = "dir_sefaz_por_estab"       NO-LOCK NO-ERROR.
    
                        IF AVAIL nfe-it-param-rec                          AND
                           nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN 
                        DO:
                            ASSIGN c-aux = c-diretorio + bf-nfe-nota-fiscal-rec.cod-estabel + "\".
    
                            /* --- Verifica se Diretorio Existe --- */
    
                            FILE-INFO:FILE-NAME = c-aux.
    
                            IF FILE-INFO:FULL-PATHNAME = ?     AND
                               FILE-INFO:FILE-TYPE    <> "DRW" THEN 
                            DO:
                                /* --- Cria Diretorio Estabelecimento --- */
                                OS-CREATE-DIR VALUE(c-aux).
    
                                ASSIGN i-erro = OS-ERROR.
    
                                IF i-erro <> 0 THEN 
                                DO:
                                    MESSAGE "Diretorio " bf-nfe-nota-fiscal-rec.cod-estabel " nao foi criado. Erro do sistema #" i-erro
                                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    
                                    LEAVE.
                                END.
                            END.
    
                            ASSIGN c-diretorio = c-aux.
                        END. /* IF AVAIL nfe-it-param-rec AND... */
                    
                        /* --- Cabecalho --- */
    
                        hXMLCon:CREATE-NODE(hRootCon, "consSitCTe", "ELEMENT":U).
    
                        hXMLCon:APPEND-CHILD(hRootCon).
    
                        hRootCon:SET-ATTRIBUTE ("versao", "1.04").
    
                        hRootCon:SET-ATTRIBUTE ("xmlns", "http://www.portalfiscal.inf.br/cte").
                    
                        ASSIGN c-lista = "tpAmb|xServ|chCTe"
                               c-valor = "1"                                + "|" +
                                         "CONSULTAR"                        + "|" +
                                         nfe-cte-inf.chave-acesso.
    
                        DO i-aux = 1 TO 3:
                            hXMLCon:CREATE-NODE(hRecordAux, ENTRY(i-aux,c-lista,'|'), "ELEMENT":U).
    
                            hRootCon:APPEND-CHILD(hRecordAux).
                    
                            hXMLCon:CREATE-NODE(hRecordAuxValue, ENTRY(i-aux,c-lista,'|'), "TEXT":U).
    
                            hRecordAux:APPEND-CHILD(hRecordAuxValue).
    
                            hRecordAuxValue:NODE-VALUE = ENTRY(i-aux,c-valor,'|').
                        END.
                            
                        hXMLCon:SAVE("FILE":U, c-diretorio + c-arquivo + "-CTE-ped-sit" + ".xml").
                            
                        DELETE OBJECT hRootCon.
    
                        DELETE OBJECT hXMLCon.
    
                    END.

                    END.

/*             END. /* ELSE do IF nfe-it-param-rec.valor-1-item-parametro = "DSCRC"..... */ */
            
            /* --- Dispara para Telas do Recebimento FISICO/FISCAL --- */

            IF p_origem = 3 OR
               p_origem = 4 THEN 
            DO:
                FIND FIRST bf-nfe-nota-fiscal-rec WHERE 
                           ROWID(bf-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

                FIND FIRST nfe-dfe NO-LOCK
                    WHERE nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso NO-ERROR.

                IF AVAIL bf-nfe-nota-fiscal-rec             AND 
                   nfe-dfe.sit-sefaz <> 4 THEN 
                DO:
                    /* --- Se nao tiver Tempo usa um Default --- */
                    
                    IF i-aux2 = 0 THEN ASSIGN i-aux2 = 5.

                    ASSIGN i-aux-cont = TIME + i-aux2.

                    /* --- Tempo de Expiracao Mensageria --- */

                    DO WHILE i-aux-cont <> TIME:
                        /* --- Valida Arquivo de Retorno de Consulta Sefaz --- */

                        RUN pi_valida_retorno_sefaz (INPUT  p_rowid_danfe,
                                                     OUTPUT l-aux,
                                                     OUTPUT p_mensagem_erro).

                        IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.

                        /* --- Encontrou XML de Retorno --- */

                        IF l-aux THEN LEAVE.
                    END.
                END.
            END.
        END. /* IF AVAIL nfe-it-param-rec.... */
    END. /* IF c-executa-bonfe001 = "PADRAO"..... */
    ELSE 
    DO:
        /* --- Verifica Erro de Validacao --- */

        IF RETURN-VALUE <> ""   AND
           RETURN-VALUE <> "OK" THEN ASSIGN p_mensagem_erro = RETURN-VALUE.
    END.
        
END PROCEDURE. /* pi_consulta_sefaz */

/**************************************************************************************************************************/

PROCEDURE pi_copia_xml_dscrc:
    
    DEF INPUT  PARAM p_rowid_danfe   AS ROWID                            NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    IF AVAIL bfu-nfe-nota-fiscal-rec THEN 
    DO:
        ASSIGN c-nome-dir-rec = bfu-nfe-nota-fiscal-rec.dir-arquivo.

        /* --- Valida Diretorios do Recebimento Sefaz DSCRC --- */

        FIND FIRST nfe-param-rec WHERE 
                   nfe-param-rec.cod-parametro = "diretorios_recebimento_nfe" NO-LOCK NO-ERROR.

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "diretorios_sefaz"          NO-LOCK NO-ERROR.

        IF AVAIL nfe-it-param-rec THEN
            ASSIGN c-nome-dir-rec-dest = nfe-it-param-rec.valor-1-item-parametro. /* --- Diretorio de Entrada DSCRC --- */

        RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec-dest).

        /* --- Copia XML do Fornecedor --- */

        ASSIGN l-aux = NO.

       
        IF SEARCH(c-nome-dir-rec) <> ? THEN 
        DO:
            
            FILE-INFO:FILE-NAME = c-nome-dir-rec-dest.
                    
            IF FILE-INFO:FULL-PATHNAME <> ?      AND
               FILE-INFO:FILE-TYPE      = "DRW"  THEN 
            DO:
                DO i-aux = 1 TO LENGTH(c-nome-dir-rec):
                    IF SUBSTR(c-nome-dir-rec, LENGTH(c-nome-dir-rec) - (i-aux - 1), 1) = "\" THEN 
                    DO:
                        ASSIGN c-nome-dir-rec-dest = c-nome-dir-rec-dest + SUBSTR(c-nome-dir-rec,(LENGTH(c-nome-dir-rec) - i-aux) + 2, i-aux - 1).

                        LEAVE.
                    END.
                END.

                ASSIGN l-aux = YES.

                OS-COPY VALUE(c-nome-dir-rec) VALUE(c-nome-dir-rec-dest).
            END.
        END.

        IF l-aux = NO THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                                           +
                                     "Consulta Sefaz nao Solicitada !,"                             +
                                     "O XML do Fornecedor nao foi encontrado no diretorio LIDOS.".
        END.
    END.
        
END PROCEDURE. /* pi_copia_xml_dscrc */

/**************************************************************************************************************************/

PROCEDURE pi_entrada_receb_fiscal:

    DEF INPUT  PARAM p_rowid_danfe   AS ROWID                           NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL "" NO-UNDO.

    DEFINE VARIABLE h_re1001p AS HANDLE      NO-UNDO.
    DEFINE VARIABLE l-preco-bruto AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE h-acomp-2 AS HANDLE      NO-UNDO.
    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-doc-est.
    EMPTY TEMP-TABLE tt-item-doc-est-aux.
    EMPTY TEMP-TABLE tt-rat-lote.
    EMPTY TEMP-TABLE tt-erro.

    FIND FIRST nfe-nota-fiscal-rec WHERE 
               ROWID(nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    RUN inbo/boin176.p PERSISTENT SET h_boin176.
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp-2.
    RUN pi-inicializar IN h-acomp-2 (INPUT "Carregando Entrada Recebimento Fiscal").

    IF AVAIL nfe-nota-fiscal-rec THEN DO:
        loop-efetiva:
        DO TRANSACTION:

            /* --- Gera Relacionamentos Item x Fornecedor / Emitente x CFOP x Natureza --- */

            RUN pi_valida_relacionamentos (INPUT  p_rowid_danfe,
                                           OUTPUT p_mensagem_erro).

            ASSIGN i-sequencia           = 0
                   de-vl-total-prod-nat  = 0
                   de-vFrete             = 0
                   de-vSeg               = 0
                   de-vDesc              = 0
                   de-vOutro             = 0
                   l-preco-bruto         = NO.

            FIND FIRST nfe-it-param-rec NO-LOCK
                WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
                AND   nfe-it-param-rec.cod-item-parametro     = 'soma_desconto_preco' 
                AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
                
            IF AVAIL nfe-it-param-rec  THEN DO:

                ASSIGN l-preco-bruto = NO.
                MESSAGE "Deseja Incluir o Desconto da ordem no preco unitario (preco bruto)?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-preco-bruto.
            END.                        

            FOR EACH nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK 
                BREAK BY (IF l-multi_natureza THEN
                          nfe-it-nota-fisc-rec.chave-acesso-nfe
                          ELSE nfe-it-nota-fisc-rec.item-nat-operacao):

                RUN pi-acompanhar IN h-acomp-2 (INPUT "ITEM: " + nfe-it-nota-fisc-rec.it-codigo + " seq. " + string(nfe-it-nota-fisc-rec.seq-item)).

                /* --- Itens a serem Implantados --- */
                IF FIRST-OF(IF l-multi_natureza THEN nfe-it-nota-fisc-rec.chave-acesso-nfe
                                ELSE nfe-it-nota-fisc-rec.item-nat-operacao) THEN DO:

                          ASSIGN de-vl-total-vbipi  = 0
                                 de-vl-total-vicms  = 0
                                 de-vl-total-vbicms = 0
                                  de-vl-total       = 0 /*bauducco notas de 3 natur*/
                                 nat-oper-cabec = nfe-it-nota-fisc-rec.item-nat-operacao.

                END.
                    
                RUN pi_item_recebimento (INPUT ROWID(nfe-it-nota-fisc-rec),
                                         INPUT 2,  
                                         INPUT l-preco-bruto,                              /* --- Recebimento Fiscal --- */
                                         INPUT-OUTPUT i-sequencia,
                                         INPUT-OUTPUT TABLE tt-item-doc-est,
                                         INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                         INPUT-OUTPUT TABLE tt-it-doc-fisico,
                                         INPUT-OUTPUT TABLE tt-rat-lote,
                                         OUTPUT p_mensagem_erro).

                /* --- Somat«rio de Produtos da Nota --- */
                ASSIGN de-vl-total-prod-nat  = de-vl-total-prod-nat + bf-nfe-it-nota-fisc-rec.item-vProd.
                
                /*Grava cod ean na item-mat Rontan*/
                FIND FIRST nfe-it-param-rec NO-LOCK
                    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
                    AND   nfe-it-param-rec.cod-item-parametro     = 'cod_ean' 
                    AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.

                IF AVAIL nfe-it-param-rec THEN DO:

                    RUN pi_grava_ean(INPUT nfe-it-nota-fisc-rec.it-codigo,
                                     INPUT nfe-it-nota-fisc-rec.item-cEAN).

                END.

                RUN pi_limpa_epc.

                CREATE tt-epc.

                ASSIGN tt-epc.cod-event     = "ItemAntesRecebFiscal"
                       tt-epc.cod-parameter = "tt-item-doc-est(HANDLE)"
                       tt-epc.val-parameter = STRING(TEMP-TABLE tt-item-doc-est:HANDLE).

                CREATE tt-epc.

                ASSIGN tt-epc.cod-event     = "ItemAntesRecebFiscal"
                       tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
                       tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

                {include\i-epc201.i "ItemAntesRecebFiscal"}

                /* --- Efetiva Entrada no Recebimento Por Natureza de Operacao --- */
                IF LAST-OF(IF l-multi_natureza THEN
                      nfe-it-nota-fisc-rec.chave-acesso-nfe
                      ELSE nfe-it-nota-fisc-rec.item-nat-operacao) THEN DO:

                    /* --- Cria Tabela de Nota do Recebimento Fiscal --- */
                    RUN pi_nota_receb_fiscal (INPUT ROWID(nfe-nota-fiscal-rec),
                                              INPUT IF l-multi_natureza THEN nat-oper-cabec ELSE nfe-it-nota-fisc-rec.item-nat-operacao,
                                              INPUT de-vl-total-prod-nat,
                                              INPUT-OUTPUT TABLE tt-docum-est,
                                              OUTPUT p_mensagem_erro).


                    /* Zera somat«ria de Frete/Descon/Despesas */

                    ASSIGN de-vFrete = 0
                           de-vSeg   = 0
                           de-vDesc  = 0
                           de-vOutro = 0.
                        
                    RUN pi_limpa_epc.

                    CREATE tt-epc.

                    ASSIGN tt-epc.cod-event     = "NotaAntesRecebFiscal"
                           tt-epc.cod-parameter = "tt-docum-est(HANDLE)"
                           tt-epc.val-parameter = STRING(TEMP-TABLE tt-docum-est:HANDLE).

                    CREATE tt-epc.

                    ASSIGN tt-epc.cod-event     = "NotaAntesRecebFiscal"
                           tt-epc.cod-parameter = "tt-item-doc-est(HANDLE)"
                           tt-epc.val-parameter = STRING(TEMP-TABLE tt-item-doc-est:HANDLE).

                    CREATE tt-epc.

                    ASSIGN tt-epc.cod-event     = "NotaAntesRecebFiscal"
                           tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
                           tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

                    {include\i-epc201.i "NotaAntesRecebFiscal"}
                                                                  

                    /* API de inclusao do documento de estoque */
                    RUN rep\reapi316b.p (INPUT "ADD",
                                         INPUT  TABLE tt-docum-est,
                                         INPUT  TABLE tt-rat-docum,
                                         INPUT  TABLE tt-item-doc-est,
                                         INPUT  TABLE tt-dupli-apagar,
                                         INPUT  TABLE tt-dupli-imp,
                                         OUTPUT TABLE tt-erro).

                    
                    IF CAN-FIND(FIRST tt-erro) THEN DO:
                        
                        ASSIGN i-erro = 0.

                        FOR EACH tt-erro NO-LOCK:
                            ASSIGN i-erro = i-erro + 1.
                                           
                            
                            CREATE tt-mensagem-erro.

                            ASSIGN tt-mensagem-erro.i-sequen = i-erro
                                   tt-mensagem-erro.cd-erro  = tt-erro.cd-erro 
                                   tt-mensagem-erro.mensagem = tt-erro.desc-erro.
                        END.

                        RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).

                        &if '{&bf_dis_versao_ems}' >= '2.08':U &then
                        
                            FOR EACH tt-mensagem-erro NO-LOCK:
                                FIND FIRST cadast_msg WHERE 
                                           cadast_msg.cdn_msg = tt-mensagem-erro.cd-erro NO-LOCK NO-ERROR.
                                    
                                IF AVAIL cadast_msg THEN DO:

                                    IF cadast_msg.idi_tip_msg = 1 THEN DO: /* --- ERRO --- */
                                        ASSIGN p_mensagem_erro = "1,"                               +
                                            STRING(tt-mensagem-erro.cd-erro)   + ","  +
                                            STRING(tt-mensagem-erro.mensagem).

                                        LEAVE.
                                    END.
                                END.
                            END.
                        &else
                            FOR EACH tt-mensagem-erro NO-LOCK:
                                FIND FIRST cad-msgs WHERE 
                                           cad-msgs.cd-msg = tt-mensagem-erro.cd-erro NO-LOCK NO-ERROR.
                                    
                                IF AVAIL cad-msgs THEN DO:

                                    IF cad-msgs.tipo-msg = 1 THEN DO: /* --- ERRO --- */
                                        ASSIGN p_mensagem_erro = "1,"                               +
                                               STRING(tt-mensagem-erro.cd-erro)   + ","  +
                                               STRING(tt-mensagem-erro.mensagem).

                                        LEAVE.
                                    END.
                                END.
                            END.
                        &endif

                        IF TRIM(p_mensagem_erro) <> "" THEN UNDO, LEAVE loop-efetiva.
                    END. /* IF CAN-FIND(FIRST tt-erro)..... */
                    ELSE DO:
                        FIND FIRST tt-docum-est NO-LOCK NO-ERROR.

                        /* --- Verifica se a NFE foi Importada para o Recebimento Fiscal a Atualiza CHAVE DA DANFE --- */

                        FIND FIRST docum-est WHERE 
                                   docum-est.serie-docto    = tt-docum-est.serie-docto  AND 
                                   docum-est.nro-docto      = tt-docum-est.nro-docto    AND 
                                   docum-est.cod-emitente   = tt-docum-est.cod-emitente AND 
                                   docum-est.nat-operacao   = tt-docum-est.nat-operacao NO-LOCK NO-ERROR.
                        IF AVAIL docum-est THEN DO:
                            &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN /*TOTVS 11*/                                            
                                FIND FIRST nfe-dfe 
                                    WHERE nfe-dfe.chave-acesso = nfe-nota-fiscal-rec.chave-acesso-nfe NO-LOCK NO-ERROR.
                                IF AVAIL nfe-dfe THEN
                                    IF nfe-dfe.sit-sefaz = 4 THEN do: /*Autorizada*/
                                        FIND CURRENT docum-est EXCLUSIVE-LOCK NO-ERROR.
                                        ASSIGN docum-est.cdn-sit-nfe = 2
                                               substring(docum-est.char-1,153,1) = "2".
                                        FIND CURRENT docum-est NO-LOCK NO-ERROR.
                                    END.
                            &ENDIF

                            /* --- Gera Relacionamento de Rat-Lote customizado no Recebimento Fiscal --- */

                            RUN pi_gera_rat_lote_fiscal (INPUT p_rowid_danfe,
                                                         INPUT TABLE tt-item-doc-est,
                                                         INPUT TABLE tt-rat-lote,
                                                         OUTPUT p_mensagem_erro).

                            FIND FIRST natur-oper NO-LOCK
                                WHERE natur-oper.nat-operacao =  docum-est.nat-operacao NO-ERROR.
                                
                            IF AVAIL natur-oper AND natur-oper.tipo-compra = 4 THEN DO: /*Material Agregado*/                            
                                FOR EACH movto-pend exclusive-lock
                                    where movto-pend.cod-emitente = docum-est.cod-emitente
                                    and   movto-pend.serie-docto  = docum-est.serie-docto
                                    and   movto-pend.nro-docto    = docum-est.nro-docto
                                    and   movto-pend.nat-operacao = docum-est.nat-operacao:
    
                                    DELETE movto-pend.
                                    /*foi necessario pois estava dando erro no RE1001 na atualizacao
                                      microgear e via bo nao temos como enviar um parametro para nao gerar.*/
                                END.
                                
                                IF l-nota-beneficiamento AND nfe-it-nota-fisc-rec.item-nr-ord-produ <> 0 THEN DO: /*‚ beneficiamento e te ordem de Producao*/
                                    FOR EACH bf6-nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK :
                                        FIND FIRST bf-natur-oper NO-LOCK
                                            WHERE bf-natur-oper.nat-operacao = bf6-nfe-it-nota-fisc-rec.item-nat-operacao NO-ERROR.
                                                                                
                                        IF bf-natur-oper.terceiros AND bf-natur-oper.tp-oper-terc = 2 THEN 
                                            ASSIGN c-nat-retorno = bf-natur-oper.nat-operacao.                                        
                                    end.

                                    CREATE movto-pend.
                                    ASSIGN movto-pend.serie-docto  = docum-est.serie-docto   /* Informa‡Æo vida da nota de Retorno de Mat‚ria Prima */
                                           movto-pend.nro-docto    = docum-est.nro-docto     /* Informa‡Æo vida da nota de Retorno de Mat‚ria Prima */
                                           movto-pend.cod-emitente = docum-est.cod-emitente  /* Informa‡Æo vida da nota de Retorno de Mat‚ria Prima */
                                           movto-pend.nat-operacao = c-nat-retorno           /* Informa‡Æo vida da nota de Retorno de Mat‚ria Prima */
                                           movto-pend.tipo         = 2                       /* Tipo do movimento: 1 - Entrada de Acabado, 2 - Baixa de Ordem */
                                           movto-pend.it-codigo    = ""                      /* Informa‡Æo vida da nota de Servi‡o */
                                           movto-pend.nro-comp     = docum-est.nro-docto     /* Informa‡Æo vida da nota de Servi‡o */
                                           movto-pend.nat-comp     = docum-est.nat-operacao. /* Informa‡Æo vida da nota de Servi‡o */

                                    DEFINE VARIABLE h-boin223 AS HANDLE      NO-UNDO.

                                    RUN inbo/boin223.p PERSISTENT SET h-boin223.
    
                                    run findDocumento in h-boin223( docum-est.cod-emitente,
                                                                     docum-est.serie-docto,
                                                                     docum-est.nro-docto,
                                                                     docum-est.nat-operacao ).

                                    run createAcabadoByOP in h-boin223. /*--- cria o acabado a partir no documento atual ---*/
                                
                                    /*--- verifica a ocorr¡ncia de erros ---*/
                                    /*if return-value = "NOK":U then do:
                                       run getRowErrors in h-boin223 ( output table RowErrors ).
                                       run PidisplayErrors.
                                    end.*/
                                
                                    IF VALID-HANDLE(h-boin223) THEN
                                       DELETE OBJECT h-boin223.
                                END.                    
                            END.

                            FOR EACH item-doc-est OF docum-est NO-LOCK:

                                RUN pi-acompanhar IN h-acomp-2 (INPUT "ITEM: " + item-doc-est.it-codigo + " seq. " + string(item-doc-est.sequencia)).

                                RUN rep/re1001p.p PERSISTENT SET h_re1001p (INPUT ROWID(item-doc-est)).
                                run piRetornaErros in h_re1001p (output table RowErrors).  

                                FOR EACH RowErrors:
                                    
                                    ASSIGN i-erro = i-erro + 1.
                                          
                                    CREATE tt-mensagem-erro.

                                    ASSIGN tt-mensagem-erro.i-sequen = i-erro
                                           tt-mensagem-erro.cd-erro  = Rowerrors.ErrorNumber
                                           tt-mensagem-erro.mensagem = Rowerrors.ErrorDescription.
                                END.

                                delete procedure h_re1001p.
                                assign h_re1001p = ?.

                                IF item-doc-est.log-1 = NO THEN DO:
                                    FOR EACH rat-ordem OF item-doc-est EXCLUSIVE-LOCK:
                                        DELETE rat-ordem.                             
                                    END.
                                END.
                            END.

                            FOR EACH item-doc-est OF docum-est NO-LOCK
                                WHERE item-doc-est.log-1 = YES:

                                IF NOT CAN-FIND(FIRST rat-ordem OF item-doc-est) THEN DO:
                                   
                                    ASSIGN i-erro = i-erro + 1.
                                          
                                    CREATE tt-mensagem-erro.
                                    ASSIGN tt-mensagem-erro.i-sequen = i-erro
                                           tt-mensagem-erro.cd-erro  = 2                                                                                    
                                           tt-mensagem-erro.mensagem = "Nao foi possivel encontrar Ordem de Compra para o Item " + item-doc-est.it-codigo + 
                                                                       " Foi selecionado a opcao FIFO de ordem de compra, porem nao foi possivel localizar as ordens, " +
                                                                       "verifique se as ordens realmente existem e se estao dentro da variacoes permitidas".
                                END.                                                                                                                        
                            END.
                            
                            /* -- Exibe Erros e Alertas --- */  

                            RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).

                            /* --- Gera hist«rico de item gerados no Recebimento --- */

                            FOR EACH tt-item-doc-est-aux NO-LOCK
                                BY tt-item-doc-est-aux.sequencia:
                                FIND FIRST nfe-his-it-nota-fis-rec WHERE 
                                           nfe-his-it-nota-fis-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe AND 
                                           nfe-his-it-nota-fis-rec.seq-item * 10    = tt-item-doc-est-aux.seq-item         AND 
                                           nfe-his-it-nota-fis-rec.sequencia        = tt-item-doc-est-aux.sequencia        AND 
                                           nfe-his-it-nota-fis-rec.nat-operacao     = tt-item-doc-est-aux.nat-operacao     NO-LOCK NO-ERROR.

                                IF NOT AVAIL nfe-his-it-nota-fis-rec THEN 
                                DO:
                                    CREATE nfe-his-it-nota-fis-rec.

                                    ASSIGN nfe-his-it-nota-fis-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe
                                           nfe-his-it-nota-fis-rec.seq-item         = tt-item-doc-est-aux.seq-item
                                           nfe-his-it-nota-fis-rec.sequencia        = tt-item-doc-est-aux.sequencia
                                           nfe-his-it-nota-fis-rec.nat-operacao     = tt-item-doc-est-aux.nat-operacao.
                                END.

                                /* --- Tratamento para Acabado --- */

                                FIND FIRST movto-pend WHERE 
                                           movto-pend.serie-docto  = tt-docum-est.serie-docto         AND 
                                           movto-pend.nro-docto    = tt-docum-est.nro-docto           AND 
                                           movto-pend.cod-emitente = tt-docum-est.cod-emitente        AND 
                                           movto-pend.nat-operacao = tt-item-doc-est-aux.nat-operacao AND 
                                           movto-pend.sequencia    = tt-item-doc-est-aux.sequencia    EXCLUSIVE-LOCK NO-ERROR.

                                IF AVAIL movto-pend THEN 
                                DO:
                                    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
                                               bf-nfe-it-nota-fisc-rec.chave-acesso-nfe   = nfe-nota-fiscal-rec.chave-acesso-nfe AND 
                                               bf-nfe-it-nota-fisc-rec.seq-item * 10      = tt-item-doc-est-aux.seq-item         NO-LOCK NO-ERROR.

                                    IF AVAIL bf-nfe-it-nota-fisc-rec THEN 
                                    DO:
                                        ASSIGN movto-pend.lote          = bf-nfe-it-nota-fisc-rec.item-ser-lote
                                               movto-pend.dt-vali-lote  = bf-nfe-it-nota-fisc-rec.item-dt-vali-lote.
                                    END.
                                END.
                            END. /* FOR EACH tt-item-doc-est-aux NO-LOCK.... */
                        
                            /* --- Recalcula Imposto da Nota no Recebimento Fiscal --- */
                            RUN pi_recalcula_imposto (INPUT TABLE tt-item-doc-est,
                                                      OUTPUT p_mensagem_erro).
            
                            /* --- Verifica Parametros Globais --- */

                            FIND FIRST nfe-param-rec WHERE 
                                       nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

                            IF AVAIL nfe-param-rec THEN DO:
                                /* --- Verifica Deposito Retorno Beneficiamento --- */

                                FIND FIRST nfe-it-param-rec WHERE 
                                           nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                                           nfe-it-param-rec.cod-item-parametro = "gera_duplicata"            NO-LOCK NO-ERROR.

                                IF AVAIL nfe-it-param-rec                                AND
                                   TRIM(nfe-it-param-rec.valor-1-item-parametro) = "SIM" THEN 
                                DO:
                                    
                                    FOR FIRST bf2-natur-oper WHERE 
                                              bf2-natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK:

                                        IF bf2-natur-oper.emite-duplic = YES AND bf2-natur-oper.tipo = 1 THEN DO:
                                            RUN rep/re9341.p (INPUT ROWID(docum-est), INPUT NO).
                                            
                                        END.
                                    END.
                                END.
                            END.

                            RUN pi_limpa_epc.

                            CREATE tt-epc.

                            ASSIGN tt-epc.cod-event     = "ItemDepoisRecebFiscal"
                                   tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
                                   tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

                            CREATE tt-epc.

                            ASSIGN tt-epc.cod-event     = "ItemDepoisRecebFiscal"
                                   tt-epc.cod-parameter = "tt-item-doc-est(HANDLE)"
                                   tt-epc.val-parameter = STRING(TEMP-TABLE tt-item-doc-est:HANDLE).

                            CREATE tt-epc.

                            ASSIGN tt-epc.cod-event     = "ItemDepoisRecebFiscal"
                                   tt-epc.cod-parameter = "tt-rat-lote(HANDLE)"
                                   tt-epc.val-parameter = STRING(TEMP-TABLE tt-rat-lote:HANDLE).

                            {include\i-epc201.i "ItemDepoisRecebFiscal"}
    
                            RUN pi_limpa_epc.

                            CREATE tt-epc.

                            ASSIGN tt-epc.cod-event     = "NotaDepoisRecebFiscal"
                                   tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
                                   tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

                            CREATE tt-epc.

                            ASSIGN tt-epc.cod-event     = "NotaDepoisRecebFiscal"
                                   tt-epc.cod-parameter = "tt-docum-est(HANDLE)"
                                   tt-epc.val-parameter = STRING(TEMP-TABLE tt-docum-est:HANDLE).

                            {include\i-epc201.i "NotaDepoisRecebFiscal"}
                                
                            CREATE nfe-his-nota-fis-rec.
                            ASSIGN nfe-his-nota-fis-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe
                                   nfe-his-nota-fis-rec.codigo           = 99
                                   nfe-his-nota-fis-rec.desc-status      = "Documento Implantado Recebimento Fiscal"
                                   nfe-his-nota-fis-rec.dt-retorno       = TODAY
                                   nfe-his-nota-fis-rec.hr-retorno       = STRING(TIME,"HH:MM:SS")
                                   nfe-his-nota-fis-rec.dt-Recbto        = TODAY                   
                                   nfe-his-nota-fis-rec.hr-Recbto        = STRING(TIME,"HH:MM:SS") 
                                   nfe-his-nota-fis-rec.nr-protocolo     = "".
                                   

                            EMPTY TEMP-TABLE tt-docum-est.
                            EMPTY TEMP-TABLE tt-item-doc-est.
                            EMPTY TEMP-TABLE tt-item-doc-est-aux.
                            EMPTY TEMP-TABLE tt-rat-lote.
                            EMPTY TEMP-TABLE tt-erro.
                            EMPTY TEMP-TABLE tt-mensagem-erro.
    
                            ASSIGN i-sequencia           = 0
                                   de-vl-total-prod-nat  = 0.
                                   
                        END. /* IF AVAIL docum-est.... */
                    END. /* ELSE do IF CAN-FIND(FIRST tt-erro).... */
                END. /* FOR EACH nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec..... */
            END.

            /* --- Atualiza Status da Nota --- */

            RUN pi_atualiza_status (INPUT p_rowid_danfe,
                                    INPUT 0,
                                    INPUT 2). /* --- NFE Implantada --- */
        END. /* loop-efetiva */
    END. /* IF AVAIL nfe-nota-fiscal-rec..... */

    IF VALID-HANDLE(h_boin176) THEN DELETE PROCEDURE h_boin176.
RUN pi-finalizar IN h-acomp-2.        
END PROCEDURE. /* pi_entrada_receb_fiscal */

/**************************************************************************************************************************/

PROCEDURE pi_entrada_receb_fisico:
    
    DEF INPUT  PARAM p_rowid_danfe   AS ROWID                            NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    DEF VAR h-doc-fisico AS HANDLE NO-UNDO.
    DEFINE VARIABLE h-acomp-3 AS HANDLE      NO-UNDO.
    DEFINE VARIABLE l-preco-bruto AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE l-erro AS LOGICAL     NO-UNDO.
    

    EMPTY TEMP-TABLE tt-doc-fisico.
    EMPTY TEMP-TABLE tt-it-doc-fisico.
    EMPTY TEMP-TABLE tt-rat-lote.
    EMPTY TEMP-TABLE tt-mensagem-erro.
    EMPTY TEMP-TABLE RowErrors.
    
    EMPTY TEMP-TABLE tt-nfe-nota-fiscal-rec-fisico.
    EMPTY TEMP-TABLE tt-nfe-it-nota-fisc-rec-fisico.
                                      
    FIND FIRST nfe-nota-fiscal-rec WHERE 
               ROWID(nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp-3.
    RUN pi-inicializar IN h-acomp-3 (INPUT "Carregando Entrada Recebimento Fisico").

    /*Carrega as notas em tt para serem separadas por tipo na implantacao*/

    RUN pi-valida-fifo(INPUT p_rowid_danfe,
                      OUTPUT l-erro).
    
    IF l-erro THEN RETURN "NOK".

    FOR EACH nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK:
        RUN pi-acompanhar IN h-acomp-3 (INPUT "ITEM: " + nfe-it-nota-fisc-rec.it-codigo + " seq. " + string(nfe-it-nota-fisc-rec.seq-item)).

        CREATE tt-nfe-it-nota-fisc-rec-fisico. 

        BUFFER-COPY nfe-it-nota-fisc-rec TO tt-nfe-it-nota-fisc-rec-fisico.

        RUN pi_valida_tipo_nota (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                 OUTPUT i-tipo-nfe,
                                 OUTPUT i-tp-oper-terc).

        /* Para substituir o i-tipo-nfe porque o tipo 3 da primeira procedure est? errada*/
        RUN pi_valida_tipo_nota_fisico (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                        OUTPUT i-tipo-nfe).

        ASSIGN tt-nfe-it-nota-fisc-rec-fisico.tipo-nota = i-tipo-nfe.

        FIND FIRST tt-nfe-nota-fiscal-rec-fisico 
            WHERE tt-nfe-nota-fiscal-rec-fisico.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe
            AND   tt-nfe-nota-fiscal-rec-fisico.tipo-nota        = i-tipo-nfe NO-ERROR.

        IF NOT AVAIL tt-nfe-nota-fiscal-rec-fisico THEN DO:
            CREATE tt-nfe-nota-fiscal-rec-fisico.          
            BUFFER-COPY nfe-nota-fiscal-rec TO tt-nfe-nota-fiscal-rec-fisico.
            ASSIGN tt-nfe-nota-fiscal-rec-fisico.tipo-nota = i-tipo-nfe
                   tt-nfe-nota-fiscal-rec-fisico.ide-dTrans = (IF tt-nfe-nota-fiscal-rec-fisico.ide-dTrans =  ? THEN TODAY ELSE tt-nfe-nota-fiscal-rec-fisico.ide-dTrans).
                   /* Passei atualizar data de transa?ao caso esteja em branco ch 7451*/
        END.                                                                            
    END.                                                                                

    IF AVAIL nfe-nota-fiscal-rec THEN DO:
        loop-efetiva :
        DO TRANSACTION :

            /* --- Gera Relacionamentos Item x Fornecedor / Emitente x CFOP x Natureza --- */

            RUN pi_valida_relacionamentos (INPUT  p_rowid_danfe,
                                           OUTPUT p_mensagem_erro).

            /* Gera Valores de Frete/Despesas/Desc */

            FOR EACH tt-nfe-nota-fiscal-rec-fisico :
                
                ASSIGN de-vFrete = 0
                       de-vSeg   = 0
                       de-vDesc  = 0
                       de-vOutro = 0.

                
                    
                FOR EACH tt-nfe-it-nota-fisc-rec-fisico 
                    WHERE tt-nfe-it-nota-fisc-rec-fisico.chave-acesso-nfe =  tt-nfe-nota-fiscal-rec-fisico.chave-acesso-nfe
                    AND   tt-nfe-it-nota-fisc-rec-fisico.tipo-nota        =  tt-nfe-nota-fiscal-rec-fisico.tipo-nota  :
                    
                    ASSIGN de-vFrete = de-vFrete + tt-nfe-it-nota-fisc-rec-fisico.item-vFrete 
                           de-vSeg   = de-vSeg   + tt-nfe-it-nota-fisc-rec-fisico.item-vSeg
                           de-vDesc  = de-vDesc  + tt-nfe-it-nota-fisc-rec-fisico.item-vDesc
                           de-vOutro = de-vOutro + tt-nfe-it-nota-fisc-rec-fisico.item-vOutro.

                END.
                
                /* --- Cria Tabela de Nota do Recebimento Fisico --- Renatooooooooooooooooooooooo*/
                
                RUN pi_nota_receb_fisico (INPUT ROWID(nfe-nota-fiscal-rec),
                                          INPUT-OUTPUT TABLE tt-doc-fisico,
                                          OUTPUT p_mensagem_erro).
                    
                RUN pi_limpa_epc.
                    
                CREATE tt-epc.
                    
                ASSIGN tt-epc.cod-event     = "NotaAntesRecebFisico"
                       tt-epc.cod-parameter = "tt-doc-fisico(HANDLE)"
                       tt-epc.val-parameter = STRING(TEMP-TABLE tt-doc-fisico:HANDLE).
                    
                CREATE tt-epc.
                    
                ASSIGN tt-epc.cod-event     = "NotaAntesRecebFisico"
                       tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
                       tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).
                    
                {include\i-epc201.i "NotaAntesRecebFisico"}
                    
                 RUN inbo\boin089.p PERSISTENT SET h-doc-fisico.
                    
                
                 RUN emptyRowErrors  IN h-doc-fisico.
                 RUN openQueryStatic IN h-doc-fisico (INPUT "Main":U).
                 RUN emptyRowErrors  IN h-doc-fisico.
                 RUN getRowErrors    IN h-doc-fisico (OUTPUT TABLE RowErrors).
                 RUN setRecord       IN h-doc-fisico (INPUT  TABLE tt-doc-fisico).
                 RUN createRecord    IN h-doc-fisico.
                 RUN getRowErrors    IN h-doc-fisico (OUTPUT TABLE RowErrors).
                     
                 IF CAN-FIND(FIRST rowerrors) THEN DO:
                     
                     ASSIGN i-erro = 0.
                         
                     FOR EACH rowerrors NO-LOCK:
                    /* --- Gera TABELA de ERRO --- */
                         
                         ASSIGN i-erro = i-erro + 1.
                             
                         CREATE tt-mensagem-erro.
                             
                         ASSIGN tt-mensagem-erro.i-sequen = i-erro
                                tt-mensagem-erro.cd-erro  = Rowerrors.ErrorNumber
                                tt-mensagem-erro.mensagem = Rowerrors.ErrorDescription.

                         
                     END.
                     
                     /* -- Exibe Erros e Alertas --- */
                     
                     RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).
                         
                     &if '{&bf_dis_versao_ems}' >= '2.08':U &then
                        
                            FOR EACH tt-mensagem-erro NO-LOCK:
                                FIND FIRST cadast_msg WHERE 
                                           cadast_msg.cdn_msg = tt-mensagem-erro.cd-erro NO-LOCK NO-ERROR.
                                    
                                IF AVAIL cadast_msg THEN DO:
                                    IF cadast_msg.idi_tip_msg = 1 THEN DO: /* --- ERRO --- */
                                        ASSIGN p_mensagem_erro = "1,"                               +
                                            STRING(tt-mensagem-erro.cd-erro)   + ","  +
                                            STRING(tt-mensagem-erro.mensagem).

                                        LEAVE.
                                    END.
                                END.
                            END.
                        &else
                        
                            FOR EACH tt-mensagem-erro NO-LOCK:
                                FIND FIRST cad-msgs WHERE 
                                           cad-msgs.cd-msg = tt-mensagem-erro.cd-erro NO-LOCK NO-ERROR.
                                    
                                IF AVAIL cad-msgs THEN DO:
                                    IF cad-msgs.tipo-msg = 1 THEN DO: /* --- ERRO --- */
                                        ASSIGN p_mensagem_erro = "1,"                               +
                                               STRING(tt-mensagem-erro.cd-erro)   + ","  +
                                               STRING(tt-mensagem-erro.mensagem).

                                        LEAVE.
                                    END.
                                END.
                            END.
                        &endif
                         
                     IF TRIM(p_mensagem_erro) <> "" THEN UNDO, LEAVE loop-efetiva.
                 END.
                 ELSE DO:
                     
                     RUN pi_limpa_epc.
                         
                     CREATE tt-epc.
                         
                     ASSIGN tt-epc.cod-event     = "NotaDepoisRecebFisico"
                            tt-epc.cod-parameter = "tt-doc-fisico(HANDLE)"
                            tt-epc.val-parameter = STRING(TEMP-TABLE tt-doc-fisico:HANDLE).
                         
                     CREATE tt-epc.
                         
                     ASSIGN tt-epc.cod-event     = "NotaDepoisRecebFisico"
                            tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
                            tt-epc.val-parameter = STRING(ROWID(nfe-nota-fiscal-rec)).
                         
                     {include\i-epc201.i "NotaDepoisRecebFisico"}
                    
                    
                    /* --- Itens --- */
                         
                     ASSIGN i-sequencia   = 0
                            l-preco-bruto = NO.
                         
                     FIND FIRST nfe-it-param-rec NO-LOCK
                         WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
                         AND   nfe-it-param-rec.cod-item-parametro     = 'soma_desconto_preco' 
                         AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
                         
                     IF AVAIL nfe-it-param-rec  THEN DO:

                         ASSIGN l-preco-bruto = YES.

                     END.
                         
                     FOR EACH tt-nfe-it-nota-fisc-rec-fisico
                         WHERE tt-nfe-it-nota-fisc-rec-fisico.chave-acesso-nfe = tt-nfe-nota-fiscal-rec-fisico.chave-acesso-nfe 
                         AND   tt-nfe-it-nota-fisc-rec-fisico.tipo-nota        = tt-nfe-nota-fiscal-rec-fisico.tipo-nota:

                         FIND FIRST nfe-it-nota-fisc-rec NO-LOCK
                             WHERE nfe-it-nota-fisc-rec.chave-acesso-nfe = tt-nfe-it-nota-fisc-rec-fisico.chave-acesso-nfe
                             AND   nfe-it-nota-fisc-rec.seq-item         = tt-nfe-it-nota-fisc-rec-fisico.seq-item   NO-ERROR.
                         
                         /* --- Itens a serem Implantados --- */
                         
                         RUN pi_item_recebimento (INPUT ROWID(nfe-it-nota-fisc-rec),
                                                  INPUT 1, /* --- Recebimento Fisico --- */
                                                  INPUT l-preco-bruto,
                                                  INPUT-OUTPUT i-sequencia,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                  INPUT-OUTPUT TABLE tt-it-doc-fisico,
                                                  INPUT-OUTPUT TABLE tt-rat-lote,
                                                  OUTPUT p_mensagem_erro).

                         /*Grava cod ean na item-mat Rontan*/
                         FIND FIRST nfe-it-param-rec NO-LOCK
                             WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
                             AND   nfe-it-param-rec.cod-item-parametro     = 'cod_ean' 
                             AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
                             
                         IF AVAIL nfe-it-param-rec THEN DO:
                             
                             RUN pi_grava_ean(INPUT nfe-it-nota-fisc-rec.it-codigo,
                                              INPUT nfe-it-nota-fisc-rec.item-cEAN).

                         END.
                             
                         IF TRIM(p_mensagem_erro) <> "" THEN UNDO, LEAVE loop-efetiva.

                     END.
                     
                     /* --- Atualiza Status da Nota --- */
                     
                     FOR EACH it-doc-fisico OF tt-doc-fisico NO-LOCK
                         WHERE it-doc-fisico.fifo-oc = YES:
                         
                         IF NOT CAN-FIND(FIRST rat-ordem OF it-doc-fisico) THEN DO:
                             
                             ASSIGN i-erro = i-erro + 1.
                                 
                             CREATE tt-mensagem-erro.
                                 
                             ASSIGN tt-mensagem-erro.i-sequen = i-erro
                                    tt-mensagem-erro.cd-erro  = 2
                                    tt-mensagem-erro.mensagem = "Nao foi possivel encontrar Ordem de Compra para o Item " + it-doc-fisico.it-codigo +
                                                                " Foi selecionado a opcao FIFO de ordem de compra, porem nao foi possivel localizar as ordens, " +
                                                                "verifique se as ordens realmente existem e se estao dentro da varia??es permitidas".

                         END.

                     END.
                     /* -- Exibe Erros e Alertas --- */  
                     
                     RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).
                     
                     CREATE nfe-his-nota-fis-rec.
                     ASSIGN nfe-his-nota-fis-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe
                            nfe-his-nota-fis-rec.codigo           = 99
                            nfe-his-nota-fis-rec.desc-status      = "Documento Implantado Recebimento Fisico"
                            nfe-his-nota-fis-rec.dt-retorno       = TODAY
                            nfe-his-nota-fis-rec.hr-retorno       = STRING(TIME,"HH:MM:SS")
                            nfe-his-nota-fis-rec.dt-Recbto        = TODAY                   
                            nfe-his-nota-fis-rec.hr-Recbto        = STRING(TIME,"HH:MM:SS") 
                            nfe-his-nota-fis-rec.nr-protocolo     = "".

                     
                     RUN pi_atualiza_status (INPUT p_rowid_danfe,
                                             INPUT 0,
                                             INPUT 2). /* --- NFE Implantada --- */
                 END.

            END.

        END. /* loop-efetiva */
        
        FIND CURRENT nfe-nota-fiscal-rec NO-LOCK NO-ERROR. /*renato-lock*/

    END. /* IF AVAIL nfe-nota-fiscal-rec.... */
RUN pi-finalizar IN h-acomp-3.        
END PROCEDURE. /* pi_entrada_receb_fisico */

/**************************************************************************************************************************/

PROCEDURE pi_gera_relac_emit_cfop_nat :
    
    DEF INPUT  PARAM p_rowid_danfe   AS ROWID NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR  NO-UNDO.

    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    IF AVAIL bfu-nfe-nota-fiscal-rec THEN 
    DO: 
        FIND emitente WHERE 
             emitente.nome-abrev = bfu-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

        IF AVAIL emitente THEN 
        DO:
            /* --- Busca Estabelecimento --- */

            FIND FIRST estabelec WHERE 
                       estabelec.cgc = bfu-nfe-nota-fiscal-rec.dest-cnpj NO-LOCK NO-ERROR.

            IF AVAIL estabelec THEN 
            DO:
                FOR EACH bf-nfe-it-nota-fisc-rec OF bfu-nfe-nota-fiscal-rec NO-LOCK:
                    /* --- Se nao existir o Relacionamento Emit x CFOP x Natureza cria registro --- */

                    FIND FIRST nfe-emit-cfop-nat WHERE 
                               nfe-emit-cfop-nat.cod-estabel  = estabelec.cod-estabel                     AND 
                               nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                     AND 
                               nfe-emit-cfop-nat.cod-cfop     = STRING(bf-nfe-it-nota-fisc-rec.item-cfop) AND 
                               nfe-emit-cfop-nat.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo         NO-LOCK NO-ERROR.

                    IF NOT AVAIL nfe-emit-cfop-nat THEN 
                    DO:
                        CREATE nfe-emit-cfop-nat.

                        ASSIGN nfe-emit-cfop-nat.cod-estabel  = estabelec.cod-estabel
                               nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente
                               nfe-emit-cfop-nat.cod-cfop     = STRING(bf-nfe-it-nota-fisc-rec.item-cfop)
                               nfe-emit-cfop-nat.nat-operacao = bf-nfe-it-nota-fisc-rec.item-nat-operacao
                               nfe-emit-cfop-nat.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo.
                    END.
                        
                    FIND FIRST nfe-emit-cfop-nat WHERE 
                               nfe-emit-cfop-nat.cod-estabel  = estabelec.cod-estabel                     AND 
                               nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                     AND 
                               nfe-emit-cfop-nat.cod-cfop     = STRING(bf-nfe-it-nota-fisc-rec.item-cfop) AND 
                               nfe-emit-cfop-nat.it-codigo    = "*"                                       NO-LOCK NO-ERROR.

                    IF NOT AVAIL nfe-emit-cfop-nat THEN 
                    DO:
                        CREATE nfe-emit-cfop-nat.

                        ASSIGN nfe-emit-cfop-nat.cod-estabel  = estabelec.cod-estabel
                               nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente
                               nfe-emit-cfop-nat.cod-cfop     = STRING(bf-nfe-it-nota-fisc-rec.item-cfop)
                               nfe-emit-cfop-nat.nat-operacao = bf-nfe-it-nota-fisc-rec.item-nat-operacao
                               nfe-emit-cfop-nat.it-codigo    = "*".
                    END.
                END. /* FOR EACH bf-nfe-it-nota-fisc-rec OF bfu-nfe-nota-fiscal-rec.... */
            END. /* IF AVAIL estabelec..... */
        END. /* IF AVAIL emitente.... */
    END. /* IF AVAIL bfu-nfe-nota-fiscal-rec.... */

END PROCEDURE. /* pi_gera_relac_emit_cfop_nat */

/**************************************************************************************************************************/

PROCEDURE pi_gera_relac_item_fornec:

    DEF INPUT  PARAM p_rowid_danfe   AS ROWID NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR  NO-UNDO.

    
    DEF VAR raw-item-fornec AS RAW NO-UNDO.
        
    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    
    IF AVAIL bfu-nfe-nota-fiscal-rec          THEN 
    DO:
        FIND emitente WHERE 
             emitente.nome-abrev = bfu-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

        IF AVAIL emitente THEN 
        DO:
            
            FOR EACH bf-nfe-it-nota-fisc-rec OF bfu-nfe-nota-fiscal-rec NO-LOCK
                WHERE  bf-nfe-it-nota-fisc-rec.item-qCom <> 0 
                AND    bf-nfe-it-nota-fisc-rec.item-qtde <> 0:

                /* --- Retona o Tipo de Nota de acordo com a Natureza --- */

                RUN pi_valida_tipo_nota (INPUT  bf-nfe-it-nota-fisc-rec.item-nat-operacao,
                                         OUTPUT i-tipo-nfe,
                                         OUTPUT i-tp-oper-terc).

                IF i-tipo-nfe = 0 THEN
                DO:
                    ASSIGN p_mensagem_erro = "1,"                                     +
                                             "Natureza de operacao nao encontrada !," +
                                             "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_GERA_RELAC_ITEM_FORNEC)".

                    RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                          OUTPUT i-acao).
                END.

                /* --- Nao Cria/Altera para Itens do Fornecedor maior do que 20 posi??es (padrao Datasul) --- */

                IF LENGTH(TRIM(bf-nfe-it-nota-fisc-rec.item-cprod)) > 20 THEN NEXT.

                /* --- Encontra o Fator de Conversao para o Item x Fornec --- */

                ASSIGN de-coeficiente = TRUNC((bf-nfe-it-nota-fisc-rec.item-qCom / bf-nfe-it-nota-fisc-rec.item-qtde),9)
                       i-num-casas    = LENGTH(SUBSTR(STRING(de-coeficiente), INDEX(STRING(de-coeficiente),",") + 1,
                                        LENGTH(STRING(de-coeficiente)) - INDEX(STRING(de-coeficiente),","))).

                ASSIGN i-fator = bf-nfe-it-nota-fisc-rec.item-qCom / EXP(10,(i-num-casas * (-1))) / bf-nfe-it-nota-fisc-rec.item-qtde.

                /* --- Nao Cria para Devolucao de Clientes --- */

                IF i-tipo-nfe = 2 THEN DO:
                    FIND FIRST nfe-it-param-rec 
                        WHERE nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro 
                          AND nfe-it-param-rec.cod-item-parametro = "Devol_Item_fornec"            
                          AND nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-LOCK NO-ERROR.
                    IF AVAIL nfe-it-param-rec  THEN
                        RUN pi_gera_nfe_item_fornec.
                END.
                ELSE DO:

                    EMPTY TEMP-TABLE tt-item-fornec.
                    ASSIGN raw-item-fornec = ?.
                    CREATE tt-item-fornec.
                    ASSIGN tt-item-fornec.im-if-cod-emitente = emitente.cod-emitente
                           tt-item-fornec.im-if-it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo
                           tt-item-fornec.im-if-item-do-forn = bf-nfe-it-nota-fisc-rec.item-cprod
                           tt-item-fornec.im-if-unid-med-for = bf-nfe-it-nota-fisc-rec.item-uCom
                           tt-item-fornec.im-if-fator-conver = i-fator
                           tt-item-fornec.im-if-num-casa-dec = i-num-casas
                           tt-item-fornec.im-if-lote-mul-for = 0
                           tt-item-fornec.im-if-tempo-ressup = 0
                           tt-item-fornec.im-if-classe-repro = 1
                           tt-item-fornec.im-if-narrativa    = "Recebimento Automatico - RA"
                           tt-item-fornec.im-if-perc-compra  = 0  
                           tt-item-fornec.im-if-lote-minimo  = 0  
                           tt-item-fornec.im-if-cod-cond-pag = 1
                           tt-item-fornec.im-if-cot-aut      = "no" 
                           tt-item-fornec.im-if-ativo        = "yes"
                           tt-item-fornec.im-if-conceito     = 0  
                           tt-item-fornec.im-if-observacao   = "Recebimento Automatico - RA"
                           tt-item-fornec.im-if-serie-nota   = ""
                           tt-item-fornec.im-if-num-nota     = ""
                           tt-item-fornec.im-if-horiz-fixo   = 0 
                           tt-item-fornec.im-if-ped-fornec   = 0 
                           tt-item-fornec.im-if-tp-inspecao  = 2 
                           tt-item-fornec.im-if-criticidade  = 1 
                           tt-item-fornec.im-if-niv-qua-ac   = 0   
                           tt-item-fornec.im-if-niv-inspecao = 0   
                           tt-item-fornec.im-if-concentracao = 0   
                           tt-item-fornec.im-if-rendimento   = 0   
                           tt-item-fornec.im-if-contr-forn   = "no"
                           tt-item-fornec.im-if-hora-ini     = 0   
                           tt-item-fornec.im-if-hora-fim     = 0   
                           tt-item-fornec.im-if-reaj-tabela  = "no"
                           tt-item-fornec.im-if-acao         = 0. /* --- 0 - Gera/Altera, 1 - Gera, 2 - Altera --- */
    
                    RUN pi_limpa_epc.
    
                    CREATE tt-epc.
    
                    ASSIGN tt-epc.cod-event     = "GeraItemFornec"
                           tt-epc.cod-parameter = "bf-nfe-it-nota-fisc-rec(ROWID)"
                           tt-epc.val-parameter = STRING(ROWID(bf-nfe-it-nota-fisc-rec)).
    
                    CREATE tt-epc.
    
                    ASSIGN tt-epc.cod-event     = "GeraItemFornec"
                           tt-epc.cod-parameter = "tt-item-fornec(HANDLE)"
                           tt-epc.val-parameter = STRING(TEMP-TABLE tt-item-fornec:HANDLE).
    
                    {include\i-epc201.i "GeraItemFornec"}
        
                    FIND FIRST tt-item-fornec NO-LOCK NO-ERROR.
    
                    RAW-TRANSFER tt-item-fornec TO raw-item-fornec.
    
                    /* --- Verifica Relacionamento Item x Fornecedor --- */
    
    
                    FIND FIRST ITEM NO-LOCK
                        WHERE ITEM.it-codigo = tt-item-fornec.im-if-it-codigo NO-ERROR.
    
                    IF ITEM.tipo-contr <> 4 THEN DO:
    
                    
                    
                        FIND FIRST item-fornec WHERE 
                                   item-fornec.cod-emitente = tt-item-fornec.im-if-cod-emitente AND 
                                   item-fornec.it-codigo    = tt-item-fornec.im-if-it-codigo    EXCLUSIVE-LOCK NO-ERROR.
        
                        IF NOT AVAIL item-fornec THEN DO:
                            /* --- 0 - Gera/Altera, 1 - Gera, 2 - Altera --- */
        
                            IF tt-item-fornec.im-if-acao = 0 OR
                               tt-item-fornec.im-if-acao = 1 THEN 
                            DO:
                                FIND FIRST bf2-item NO-LOCK WHERE 
                                           bf2-item.it-codigo  = tt-item-fornec.im-if-it-codigo AND   
                                           bf2-item.tipo-contr = 4                              NO-ERROR. /* D?bito direto - Sendo de Debito Direto nao cria item-fornec - V05 R00 */
        
                                IF AVAIL bf2-item THEN 
                                DO:
                                    FIND FIRST nfe-item-fornec WHERE
                                               nfe-item-fornec.it-codigo    = tt-item-fornec.im-if-it-codigo    AND
                                               nfe-item-fornec.cod-emitente = tt-item-fornec.im-if-cod-emitente NO-ERROR.
        
                                    IF NOT AVAIL nfe-item-fornec THEN DO:
                                        CREATE nfe-item-fornec.
        
                                        ASSIGN nfe-item-fornec.it-codigo    = tt-item-fornec.im-if-it-codigo 
                                               nfe-item-fornec.cod-emitente = tt-item-fornec.im-if-cod-emitente.
                                    END.
        
                                    
                                    ASSIGN nfe-item-fornec.item-do-forn   = tt-item-fornec.im-if-item-do-forn 
                                           nfe-item-fornec.unid-med-for   = tt-item-fornec.im-if-unid-med-for 
                                           nfe-item-fornec.fator-conver   = tt-item-fornec.im-if-fator-conver 
                                           nfe-item-fornec.num-casa-dec   = tt-item-fornec.im-if-num-casa-dec 
                                           nfe-item-fornec.tempo-ressup   = tt-item-fornec.im-if-tempo-ressup 
                                           nfe-item-fornec.ativo          = (IF tt-item-fornec.im-if-ativo = "yes" THEN YES ELSE NO)
                                           nfe-item-fornec.lote-minimo    = tt-item-fornec.im-if-lote-minimo 
                                           nfe-item-fornec.lote-mul-for   = tt-item-fornec.im-if-lote-mul-for 
                                           nfe-item-fornec.perc-compra    = tt-item-fornec.im-if-perc-compra 
                                           nfe-item-fornec.cot-aut        = (IF tt-item-fornec.im-if-cot-aut = "S" THEN YES ELSE NO)
                                           nfe-item-fornec.cod-cond-pag   = tt-item-fornec.im-if-cod-cond-pag 
                                           nfe-item-fornec.classe-repro   = tt-item-fornec.im-if-classe-repro
                                           nfe-item-fornec.perc-pont-forn = tt-item-fornec.im-perc-pont-forn
                                           nfe-item-fornec.ind-pont       = tt-item-fornec.im-ind-pont
                                           nfe-item-fornec.perc-dev-forn  = tt-item-fornec.im-perc-dev-forn
                                           nfe-item-fornec.concentracao   = tt-item-fornec.im-if-concentracao
                                           nfe-item-fornec.rendimento     = tt-item-fornec.im-if-rendimento
                                           nfe-item-fornec.cod-mensagem   = tt-item-fornec.im-if-cod-msg      
                                           nfe-item-fornec.contr-forn     = (IF tt-item-fornec.im-if-contr-forn = "S" THEN YES ELSE NO)
                                           nfe-item-fornec.hora-ini       = tt-item-fornec.im-if-hora-ini
                                           nfe-item-fornec.hora-fim       = tt-item-fornec.im-if-hora-fim
                                           nfe-item-fornec.reaj-tabela    = (IF tt-item-fornec.im-if-reaj-tabela = "y" THEN YES ELSE NO)
                                           nfe-item-fornec.conceito       = tt-item-fornec.im-if-conceito
                                           nfe-item-fornec.observacao     = tt-item-fornec.im-if-observacao
                                           nfe-item-fornec.serie-nota     = tt-item-fornec.im-if-serie-nota
                                           nfe-item-fornec.numero-nota    = tt-item-fornec.im-if-num-nota     
                                           nfe-item-fornec.horiz-fixo     = tt-item-fornec.im-if-horiz-fixo  
                                           nfe-item-fornec.ped-fornec     = tt-item-fornec.im-if-ped-fornec
                                           nfe-item-fornec.tp-inspecao    = tt-item-fornec.im-if-tp-inspecao
                                           nfe-item-fornec.criticidade    = tt-item-fornec.im-if-criticidade
                                           nfe-item-fornec.qt-max-ordem   = tt-item-fornec.im-if-qt-max-ord   
                                           nfe-item-fornec.niv-qua-ac     = tt-item-fornec.im-if-niv-qua-ac
                                           nfe-item-fornec.niv-inspecao   = tt-item-fornec.im-if-niv-inspecao.
                                END.
                                ELSE 
                                DO:
                                    FIND FIRST item-fornec WHERE
                                               item-fornec.it-codigo    = tt-item-fornec.im-if-it-codigo    AND
                                               item-fornec.cod-emitente = tt-item-fornec.im-if-cod-emitente NO-ERROR.
        
                                    IF NOT AVAIL item-fornec THEN DO:
                                        CREATE item-fornec.
        
                                        ASSIGN item-fornec.it-codigo    = tt-item-fornec.im-if-it-codigo 
                                               item-fornec.cod-emitente = tt-item-fornec.im-if-cod-emitente.
                                    END.                             
                                    
                                    ASSIGN item-fornec.item-do-forn   = tt-item-fornec.im-if-item-do-forn 
                                           item-fornec.unid-med-for   = tt-item-fornec.im-if-unid-med-for 
                                           item-fornec.fator-conver   = tt-item-fornec.im-if-fator-conver 
                                           item-fornec.num-casa-dec   = tt-item-fornec.im-if-num-casa-dec 
                                           item-fornec.tempo-ressup   = tt-item-fornec.im-if-tempo-ressup 
                                           item-fornec.ativo          = (IF tt-item-fornec.im-if-ativo = "yes" THEN YES ELSE NO)
                                           item-fornec.lote-minimo    = tt-item-fornec.im-if-lote-minimo 
                                           item-fornec.lote-mul-for   = tt-item-fornec.im-if-lote-mul-for 
                                           item-fornec.perc-compra    = tt-item-fornec.im-if-perc-compra 
                                           item-fornec.cot-aut        = (IF tt-item-fornec.im-if-cot-aut = "S" THEN YES ELSE NO)
                                           item-fornec.cod-cond-pag   = tt-item-fornec.im-if-cod-cond-pag 
                                           item-fornec.classe-repro   = tt-item-fornec.im-if-classe-repro
                                           item-fornec.perc-pont-forn = tt-item-fornec.im-perc-pont-forn
                                           item-fornec.ind-pont       = tt-item-fornec.im-ind-pont
                                           item-fornec.perc-dev-forn  = tt-item-fornec.im-perc-dev-forn
                                           item-fornec.concentracao   = tt-item-fornec.im-if-concentracao
                                           item-fornec.rendimento     = tt-item-fornec.im-if-rendimento
                                           item-fornec.cod-mensagem   = tt-item-fornec.im-if-cod-msg      
                                           item-fornec.contr-forn     = (IF tt-item-fornec.im-if-contr-forn = "S" THEN YES ELSE NO)
                                           item-fornec.hora-ini       = tt-item-fornec.im-if-hora-ini
                                           item-fornec.hora-fim       = tt-item-fornec.im-if-hora-fim
                                           item-fornec.reaj-tabela    = (IF tt-item-fornec.im-if-reaj-tabela = "y" THEN YES ELSE NO)
                                           item-fornec.conceito       = tt-item-fornec.im-if-conceito
                                           item-fornec.observacao     = tt-item-fornec.im-if-observacao
                                           item-fornec.serie-nota     = tt-item-fornec.im-if-serie-nota
                                           item-fornec.numero-nota    = tt-item-fornec.im-if-num-nota     
                                           item-fornec.horiz-fixo     = tt-item-fornec.im-if-horiz-fixo  
                                           item-fornec.ped-fornec     = tt-item-fornec.im-if-ped-fornec
                                           item-fornec.tp-inspecao    = tt-item-fornec.im-if-tp-inspecao
                                           item-fornec.criticidade    = tt-item-fornec.im-if-criticidade
                                           item-fornec.qt-max-ordem   = tt-item-fornec.im-if-qt-max-ord   
                                           item-fornec.niv-qua-ac     = tt-item-fornec.im-if-niv-qua-ac
                                           item-fornec.niv-inspecao   = tt-item-fornec.im-if-niv-inspecao.
        
                                    /*** MOD.01 - Baione - 07/06/2011 ***/
        
                                    IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN 
                                    DO:
                                        CREATE nfe-item-fornec.
        
                                        BUFFER-COPY item-fornec TO nfe-item-fornec. 
                                    END.
                                    
                                END. /* ELSE do IF AVAIL bf2-item THEN...... */
                            END. /* IF tt-item-fornec.im-if-acao = 0.... */
                        END. /* IF NOT AVAIL item-fornec.... */
                        ELSE DO:
                            /*** MOD.01 - Baione - 07/06/2011 ***/
                            IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN DO:
                                CREATE nfe-item-fornec.
        
                                BUFFER-COPY item-fornec TO nfe-item-fornec. 
                            END.
        
                            
                            FIND FIRST nfe-item-fornec OF item-fornec EXCLUSIVE-LOCK NO-ERROR.
        
                            /* --- 0 - Gera/Altera, 1 - Gera, 2 - Altera --- */
        
                            IF tt-item-fornec.im-if-acao = 0 OR
                               tt-item-fornec.im-if-acao = 2 THEN DO:
                                
                                /* --- Altera Item do Fornecedor --- */
                                IF item-fornec.item-do-forn <> tt-item-fornec.im-if-item-do-forn THEN 
                                    ASSIGN item-fornec.item-do-forn     = tt-item-fornec.im-if-item-do-forn
                                           nfe-item-fornec.item-do-forn = item-fornec.item-do-forn
                                           nfe-item-fornec.narrativa    = item-fornec.narrativa. 

                                /* --- Altera Fator de Conversao --- */
                                IF item-fornec.fator-conver <> tt-item-fornec.im-if-fator-conver THEN DO:
                                    ASSIGN nfe-item-fornec.fator-conver = tt-item-fornec.im-if-fator-conver
                                           nfe-item-fornec.narrativa    = item-fornec.narrativa                                       +
                                                                          (IF TRIM(item-fornec.narrativa) <> "" THEN CHR(10) ELSE "") +
                                                                          "Docto: " + STRING(bfu-nfe-nota-fiscal-rec.ide-nNF)          + " "   +
                                                                          "Serie: " + STRING(bfu-nfe-nota-fiscal-rec.ide-Serie)        + " - " +
                                                                          STRING("Alterado Fator de Conversao - Recebimento Automatico","x(55)") + CHR(10).
                                END.
        
                                /* --- Altera Casas Decimais --- */
                                IF item-fornec.num-casa-dec <> tt-item-fornec.im-if-num-casa-dec THEN
                                    ASSIGN nfe-item-fornec.num-casa-dec = tt-item-fornec.im-if-num-casa-dec
                                           nfe-item-fornec.narrativa    = item-fornec.narrativa                                        +
                                                                        (IF TRIM(item-fornec.narrativa) <> "" THEN CHR(10) ELSE "")    +
                                                                         "Docto: " + STRING(bfu-nfe-nota-fiscal-rec.ide-nNF)            + " "   +
                                                                         "Serie: " + STRING(bfu-nfe-nota-fiscal-rec.ide-Serie)          + " - " +
                                                                         STRING("Alterado Casas Decimais - Recebimento Automatico","x(55)") + CHR(10).
        
                                                                                 
                                /* --- Altera Unidade de Medida do Fornecedor --- */
        
                                IF item-fornec.unid-med-for <> tt-item-fornec.im-if-unid-med-for THEN
                                    ASSIGN nfe-item-fornec.unid-med-for = tt-item-fornec.im-if-unid-med-for
                                           nfe-item-fornec.narrativa    = item-fornec.narrativa                                         +
                                                                          (IF TRIM(item-fornec.narrativa) <> "" THEN CHR(10) ELSE "")   +
                                                                          "Docto: " + STRING(bfu-nfe-nota-fiscal-rec.ide-nNF)            + " "   +
                                                                          "Serie: " + STRING(bfu-nfe-nota-fiscal-rec.ide-Serie)          + " - " +
                                                                          STRING("Alterado UN do Fornecedor - Recebimento Automatico","x(55)") + CHR(10).
                            END. /* IF tt-item-fornec.im-if-acao = 0... */
                        END. /* ELSE do IF NOT AVAIL item-fornec THEN.... */
                    END.
                END.
            END. /* FOR EACH bf-nfe-it-nota-fisc-rec OF bfu-nfe-nota-fiscal-rec NO-LOCK.... */
        END. /* IF AVAIL emitente.... */
    END. /* IF AVAIL bfu-nfe-nota-fiscal-rec.... */
    

END PROCEDURE. /* pi_gera_relac_item_fornec */

/**************************************************************************************************************************/

PROCEDURE pi_gera_nfe_item_fornec:

    FIND FIRST nfe-item-fornec WHERE
               nfe-item-fornec.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo    AND
               nfe-item-fornec.cod-emitente = emitente.cod-emitente NO-ERROR.

    IF NOT AVAIL nfe-item-fornec THEN DO:
        CREATE nfe-item-fornec.

        ASSIGN nfe-item-fornec.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo    
               nfe-item-fornec.cod-emitente = emitente.cod-emitente.
    END.

    
    ASSIGN nfe-item-fornec.item-do-forn   = bf-nfe-it-nota-fisc-rec.item-cprod
           nfe-item-fornec.unid-med-for   = bf-nfe-it-nota-fisc-rec.item-un
           nfe-item-fornec.fator-conver   = i-fator 
           nfe-item-fornec.num-casa-dec   = i-num-casas.

END PROCEDURE. /* pi_gera_nfe_item_fornec */
/*************************************************************************************************************************************************************/

PROCEDURE pi_recalcula_imposto :
    
    DEF INPUT  PARAM TABLE FOR tt-item-doc-est.
    DEF OUTPUT PARAM p_mensagem_erro    AS CHAR FORMAT "x(200)" INITIAL "" NO-UNDO.
    DEFINE VARIABLE h-acomp-4           AS HANDLE                          NO-UNDO.
    DEF VAR c-pedagio AS CHAR.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp-4.
    RUN pi-inicializar IN h-acomp-4 (INPUT "Carregando Recalcula Imposto").

    RUN inbo/boin176.p PERSISTENT SET h_boin176.
    RUN inbo/boin090.p PERSISTENT SET h_boin090.

    FIND FIRST nfe-it-param-rec 
        WHERE nfe-it-param-rec.cod-item-parametro = "Pedagio_bo" NO-LOCK NO-ERROR.

    ASSIGN c-pedagio = nfe-it-param-rec.valor-1-item-parametro.

    IF NOT AVAIL nfe-it-param-rec THEN
        ASSIGN c-pedagio = "nao".

    FOR EACH tt-item-doc-est NO-LOCK,
        FIRST bf-docum-est WHERE 
              bf-docum-est.serie-docto     = tt-item-doc-est.serie-docto  AND 
              bf-docum-est.nro-docto       = tt-item-doc-est.nro-docto    AND 
              bf-docum-est.cod-emitente    = tt-item-doc-est.cod-emitente AND 
              bf-docum-est.nat-operacao    = tt-item-doc-est.nat-operacao EXCLUSIVE-LOCK,
        FIRST bf-item-doc-est WHERE 
              bf-item-doc-est.serie-docto  = tt-item-doc-est.serie-docto  AND 
              bf-item-doc-est.nro-docto    = tt-item-doc-est.nro-docto    AND 
              bf-item-doc-est.cod-emitente = tt-item-doc-est.cod-emitente AND 
              bf-item-doc-est.nat-operacao = tt-item-doc-est.nat-operacao AND 
              bf-item-doc-est.sequencia    = tt-item-doc-est.sequencia    EXCLUSIVE-LOCK 
        BY tt-item-doc-est.sequencia:

        RUN pi-acompanhar IN h-acomp-4 (INPUT "ITEM: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).


        RUN openQueryStatic IN h_boin176 ("Main":U).
        RUN openQueryStatic IN h_boin090 ("Main":U).
        RUN findDocumento   IN h_boin176 (bf-item-doc-est.cod-emitente,
                                          bf-item-doc-est.serie-docto,
                                          bf-item-doc-est.nro-docto,
                                          bf-item-doc-est.nat-operacao).

        /*Colocando as aliquotas que a BO eventualmente trocou*/
        ASSIGN bf-item-doc-est.aliquota-ipi        = tt-item-doc-est.aliquota-ipi
               bf-item-doc-est.aliquota-icm        = tt-item-doc-est.aliquota-icm
               bf-item-doc-est.val-aliq-pis        = tt-item-doc-est.val-aliq-pis     
               bf-item-doc-est.idi-tributac-pis    = tt-item-doc-est.idi-tributac-pis 
               bf-item-doc-est.val-aliq-cofins     = tt-item-doc-est.val-aliq-cofins    
               bf-item-doc-est.idi-tributac-cofins = tt-item-doc-est.idi-tributac-cofins.

        RUN setHandleDocumEst IN h_boin176 (input h_boin090).

        RUN goToKey           IN h_boin176 ( bf-item-doc-est.serie-docto ,
                                             bf-item-doc-est.nro-docto   ,
                                             bf-item-doc-est.cod-emitente,
                                             bf-item-doc-est.nat-operacao,
                                             bf-item-doc-est.sequencia   ).
        
        FIND FIRST natur-oper WHERE 
                   natur-oper.nat-operacao = bf-item-doc-est.nat-operacao NO-LOCK NO-ERROR.

        RUN findItem IN h_boin176 (INPUT bf-item-doc-est.it-codigo).
        run findNaturOper IN h_boin176 ( bf-item-doc-est.nat-operacao ).
                 
                        
        /*** Zera o valor de frete na it-docum-est para que seja recalculado***/
                          
        ASSIGN bf-item-doc-est.pr-total-cmi = 0.

        FIND FIRST estabelec NO-LOCK
            WHERE estabelec.cod-estabel = bf-docum-est.cod-estabel NO-ERROR.

        FIND FIRST item NO-LOCK
            WHERE item.it-codigo = bf-item-doc-est.it-codigo NO-ERROR.

        FIND FIRST emitente NO-LOCK
            WHERE emitente.cod-emitente = bf-docum-est.cod-emitente NO-ERROR.
        
        FIND FIRST natur-oper NO-LOCK
            WHERE natur-oper.nat-operacao = IF l-multi_natureza THEN bf-item-doc-est.nat-of ELSE bf-docum-est.nat-operacao NO-ERROR.
          
        RUN pi-acompanhar IN h-acomp-4 (INPUT "Recalculate Imposto" + "ITEM: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).

        CASE c-pedagio:
            WHEN "12.1.13" THEN
                RUN recalculateImposto IN h_boin176 (INPUT bf-item-doc-est.qt-do-forn,
                                                     INPUT bf-item-doc-est.preco-total[1],
                                                     INPUT bf-item-doc-est.desconto[1],
                                                     INPUT bf-item-doc-est.despesas[1],
                                                     INPUT bf-item-doc-est.pr-total-cmi,
                                                     INPUT bf-item-doc-est.peso-liquido,
                                                     INPUT 0, /*novo parametro pedagio*/
                                                     INPUT bf-item-doc-est.aliquota-ipi,
                                                     INPUT bf-item-doc-est.cd-trib-ipi,
                                                     INPUT bf-item-doc-est.aliquota-iss,
                                                     INPUT bf-item-doc-est.cd-trib-iss,
                                                     INPUT bf-item-doc-est.aliquota-icm,
                                                     INPUT bf-item-doc-est.cd-trib-icm,
                                                     INPUT DEC(SUBSTR(bf-item-doc-est.char-2,1,6)), /* perc ipi */
                                                     INPUT IF AVAIL natur-oper THEN natur-oper.perc-red-icm              ELSE 0, /* perc icm */
                                                     INPUT bf-item-doc-est.log-2,
                                                     INPUT bf-item-doc-est.idi-tributac-pis, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,86,1)) ELSE 2, /* cd-trib-pis  */*/
                                                     INPUT bf-item-doc-est.val-aliq-pis, /*IF AVAIL natur-oper THEN natur-oper.perc-pis[1]               ELSE 0*/  /* val-aliq-pis */
                                                     INPUT bf-item-doc-est.idi-tributac-cofins, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,87,1)) ELSE 2, /* cd-trib-cofins */*/
                                                     INPUT bf-item-doc-est.val-aliq-cofins, /*IF AVAIL natur-oper THEN DECI(natur-oper.per-fin-soc[1])      ELSE 0,*/ /* val-aliq-cofins */
                                                     INPUT NO). 
            WHEN "12.1.12" THEN
                RUN recalculateImposto IN h_boin176 (INPUT bf-item-doc-est.qt-do-forn,
                                                     INPUT bf-item-doc-est.preco-unit[1],
                                                     INPUT bf-item-doc-est.preco-total[1],
                                                     INPUT bf-item-doc-est.desconto[1],
                                                     INPUT bf-item-doc-est.despesas[1],
                                                     INPUT bf-item-doc-est.pr-total-cmi,
                                                     INPUT bf-item-doc-est.peso-liquido,
                                                     INPUT 0, /*novo parametro pedagio*/
                                                     INPUT bf-item-doc-est.aliquota-ipi,
                                                     INPUT bf-item-doc-est.cd-trib-ipi,
                                                     INPUT bf-item-doc-est.aliquota-iss,
                                                     INPUT bf-item-doc-est.cd-trib-iss,
                                                     INPUT bf-item-doc-est.aliquota-icm,
                                                     INPUT bf-item-doc-est.cd-trib-icm,
                                                     INPUT DEC(SUBSTR(bf-item-doc-est.char-2,1,6)), /* perc ipi */
                                                     INPUT IF AVAIL natur-oper THEN natur-oper.perc-red-icm              ELSE 0, /* perc icm */
                                                     INPUT bf-item-doc-est.log-2,
                                                     INPUT bf-item-doc-est.idi-tributac-pis, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,86,1)) ELSE 2, /* cd-trib-pis  */*/
                                                     INPUT bf-item-doc-est.val-aliq-pis, /*IF AVAIL natur-oper THEN natur-oper.perc-pis[1]               ELSE 0*/  /* val-aliq-pis */
                                                     INPUT bf-item-doc-est.idi-tributac-cofins, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,87,1)) ELSE 2, /* cd-trib-cofins */*/
                                                     INPUT bf-item-doc-est.val-aliq-cofins, /*IF AVAIL natur-oper THEN DECI(natur-oper.per-fin-soc[1])      ELSE 0,*/ /* val-aliq-cofins */
                                                     INPUT NO).  
            WHEN "sim" THEN
                RUN recalculateImposto IN h_boin176 (INPUT bf-item-doc-est.qt-do-forn,
                                                    INPUT bf-item-doc-est.preco-total[1],
                                                    INPUT bf-item-doc-est.desconto[1],
                                                    INPUT bf-item-doc-est.despesas[1],
                                                    INPUT bf-item-doc-est.pr-total-cmi,
                                                    INPUT bf-item-doc-est.peso-liquido,
                                                    INPUT 0, /*novo parametro pedagio*/
                                                    INPUT bf-item-doc-est.aliquota-ipi,
                                                    INPUT bf-item-doc-est.cd-trib-ipi,
                                                    INPUT bf-item-doc-est.aliquota-iss,
                                                    INPUT bf-item-doc-est.cd-trib-iss,
                                                    INPUT bf-item-doc-est.aliquota-icm,
                                                    INPUT bf-item-doc-est.cd-trib-icm,
                                                    INPUT DEC(SUBSTR(bf-item-doc-est.char-2,1,6)), /* perc ipi */
                                                    INPUT IF AVAIL natur-oper THEN natur-oper.perc-red-icm              ELSE 0, /* perc icm */
                                                    INPUT bf-item-doc-est.log-2,
                                                    INPUT bf-item-doc-est.idi-tributac-pis, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,86,1)) ELSE 2, /* cd-trib-pis  */*/
                                                    INPUT bf-item-doc-est.val-aliq-pis, /*IF AVAIL natur-oper THEN natur-oper.perc-pis[1]               ELSE 0*/  /* val-aliq-pis */
                                                    INPUT bf-item-doc-est.idi-tributac-cofins, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,87,1)) ELSE 2, /* cd-trib-cofins */*/
                                                    INPUT bf-item-doc-est.val-aliq-cofins, /*IF AVAIL natur-oper THEN DECI(natur-oper.per-fin-soc[1])      ELSE 0,*/ /* val-aliq-cofins */
                                                    INPUT NO).       
            WHEN "nao" THEN
                RUN recalculateImposto IN h_boin176 (INPUT bf-item-doc-est.qt-do-forn,
                                                     INPUT bf-item-doc-est.preco-total[1],
                                                     INPUT bf-item-doc-est.desconto[1],
                                                     INPUT bf-item-doc-est.despesas[1],
                                                     INPUT bf-item-doc-est.pr-total-cmi,
                                                     INPUT bf-item-doc-est.peso-liquido,
                                                     INPUT bf-item-doc-est.aliquota-ipi,
                                                     INPUT bf-item-doc-est.cd-trib-ipi,
                                                     INPUT bf-item-doc-est.aliquota-iss,
                                                     INPUT bf-item-doc-est.cd-trib-iss,
                                                     INPUT bf-item-doc-est.aliquota-icm,
                                                     INPUT bf-item-doc-est.cd-trib-icm,
                                                     INPUT DEC(SUBSTR(bf-item-doc-est.char-2,1,6)), /* perc ipi */
                                                     INPUT IF AVAIL natur-oper THEN natur-oper.perc-red-icm              ELSE 0, /* perc icm */
                                                     INPUT bf-item-doc-est.log-2,
                                                     INPUT bf-item-doc-est.idi-tributac-pis, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,86,1)) ELSE 2, /* cd-trib-pis  */*/
                                                     INPUT bf-item-doc-est.val-aliq-pis, /*IF AVAIL natur-oper THEN natur-oper.perc-pis[1]               ELSE 0*/  /* val-aliq-pis */
                                                     INPUT bf-item-doc-est.idi-tributac-cofins, /*IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,87,1)) ELSE 2, /* cd-trib-cofins */*/
                                                     INPUT bf-item-doc-est.val-aliq-cofins, /*IF AVAIL natur-oper THEN DECI(natur-oper.per-fin-soc[1])      ELSE 0,*/ /* val-aliq-cofins */
                                                     INPUT NO).
        END CASE.

        RUN getDecField IN h_boin176 (INPUT "aliquota-icm":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.aliquota-icm     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "valor-icm[1]":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.valor-icm[1]    = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "aliquota-ipi":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.aliquota-ipi     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "ipi-ntrib[1]":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.ipi-ntrib[1]     = DECI(c-aux).      

        /*Rontan Nota de comercio que divide o IPI por 50%*/

        IF natur-oper.log-2 = YES THEN DO: /*Nota Comercio*/
             RUN getDecField IN h_boin176 (INPUT "base-ipi[1]":U,   OUTPUT c-aux).
             ASSIGN bf-item-doc-est.base-ipi[1]      = DECI(c-aux) / 2.
             ASSIGN bf-item-doc-est.ipi-outras[1]    = DECI(c-aux) - bf-item-doc-est.base-ipi[1].

             RUN getDecField IN h_boin176 (INPUT "valor-ipi[1]":U,  OUTPUT c-aux).
             ASSIGN bf-item-doc-est.valor-ipi[1]     = DECI(c-aux) / 2.
        END.
        ELSE DO:
            RUN getDecField IN h_boin176 (INPUT "ipi-outras[1]":U, OUTPUT c-aux).
            ASSIGN bf-item-doc-est.ipi-outras[1]    = DECI(c-aux).
    
            RUN getDecField IN h_boin176 (INPUT "base-ipi[1]":U,   OUTPUT c-aux).
            ASSIGN bf-item-doc-est.base-ipi[1]      = DECI(c-aux).

            RUN getDecField IN h_boin176 (INPUT "valor-ipi[1]":U,  OUTPUT c-aux).
            ASSIGN bf-item-doc-est.valor-ipi[1]     = DECI(c-aux).
        END.
        
        RUN getDecField IN h_boin176 (INPUT "preco-unit[1]":U, OUTPUT c-aux).
        ASSIGN bf-item-doc-est.preco-unit[1]    = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "pr-total-cmi":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.pr-total-cmi     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "base-iss[1]":U,   OUTPUT c-aux).
        ASSIGN bf-item-doc-est.base-iss[1]      = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "valor-iss[1]":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.valor-iss[1]     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "iss-ntrib[1]":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.iss-ntrib[1]     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "iss-outras[1]":U, OUTPUT c-aux).
        ASSIGN bf-item-doc-est.iss-outras[1]    = DECI(c-aux).

        /* --- Recalcula Imposto da Nota --- */
        RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula PIS/COFINS. " + "ITEM: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).

        ASSIGN bf-item-doc-est.valor-icm[1] = bf-item-doc-est.base-icm[1] * (tt-item-doc-est.aliquota-icm / 100).
        
        &IF '{&bf_dis_versao_ems}' >= '2.06' &THEN
        
            ASSIGN bf-item-doc-est.valor-pis    = round(bf-item-doc-est.base-pis * (bf-item-doc-est.val-aliq-pis / 100),2)
                   bf-item-doc-est.val-cofins   = round(bf-item-doc-est.val-base-calc-cofins * (bf-item-doc-est.val-aliq-cofins / 100),2).
        &ELSE
            RUN getCharField IN h_boin176 (INPUT "char-2":U, OUTPUT c-aux).
            
            /* Tributacao PIS */

            IF SUBSTR(c-aux,21,1) = "" THEN ASSIGN SUBSTR(bf-item-doc-est.char-2,21,1) = "2".
                                       ELSE ASSIGN SUBSTR(bf-item-doc-est.char-2,21,1) = SUBSTR(c-aux,21,1).

            /* Tributacao COFINS */

            IF SUBSTR(c-aux,83,1) = "" THEN ASSIGN SUBSTR(bf-item-doc-est.char-2,83,1) = "2".
                                       ELSE ASSIGN SUBSTR(bf-item-doc-est.char-2,83,1) = SUBSTR(c-aux,83,1).
            
            ASSIGN SUBSTR(bf-item-doc-est.char-2,22,5)   = STRING(DECI(SUBSTR(c-aux,22,5)),">9.99":U)               /* aliquota pis     */
                   SUBSTR(bf-item-doc-est.char-2,27,14)  = STRING(DECI(SUBSTR(c-aux,27,14)),">>>,>>>,>>9.99":U)     /* base pis         */
                   SUBSTR(bf-item-doc-est.char-2,41,14)  = STRING(DECI(SUBSTR(c-aux,41,14)),">>>,>>>,>>9.99":U)     /* valor pis        */
                   SUBSTR(bf-item-doc-est.char-2,84,5)   = STRING(DECI(SUBSTR(c-aux,84,5)),">9.99":U)               /* aliquota cofins  */
                   SUBSTR(bf-item-doc-est.char-2,89,14)  = STRING(DECI(SUBSTR(c-aux,89,14)),">>>,>>>,>>9.99":U)     /* base cofins      */
                   SUBSTR(bf-item-doc-est.char-2,103,14) = STRING(DECI(SUBSTR(c-aux,103,14)),">>>,>>>,>>9.99":U).   /* valor cofins     */
        &ENDIF
        
        IF i-tipo-nfe = 2 THEN DO: /*devolucao*/
            FIND FIRST bf5-nfe-nota-fiscal-rec
                WHERE bf5-nfe-nota-fiscal-rec.ide-Serie   = tt-item-doc-est.serie-docto
                  AND bf5-nfe-nota-fiscal-rec.ide-nNF     = tt-item-doc-est.nro-docto
                  AND bf5-nfe-nota-fiscal-rec.nome-abrev  = emitente.nome-abrev NO-LOCK NO-ERROR.

            IF AVAIL bf5-nfe-nota-fiscal-rec THEN DO:
                FIND FIRST bf5-nfe-it-nota-fisc-rec
                    WHERE bf5-nfe-it-nota-fisc-rec.chave-acesso  = bf5-nfe-nota-fiscal-rec.chave-acesso-nfe
                      AND bf5-nfe-it-nota-fisc-rec.seq-item * 10 = tt-item-doc-est.sequencia        NO-ERROR.
                IF AVAIL bf5-nfe-it-nota-fisc-rec THEN DO:
                
                    FIND FIRST nfe-it-imposto-alt NO-LOCK
                        WHERE nfe-it-imposto-alt.chave-acesso-nfe = bf5-nfe-nota-fiscal-rec.chave-acesso-nfe
                          AND nfe-it-imposto-alt.seq-item * 10    = tt-item-doc-est.sequencia        NO-ERROR.
    
                    RUN pi-acompanhar IN h-acomp-4 (INPUT "ITEM Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
        
                    /*IPI Devolucao - TOTAL BASE IPI*/
                    IF l-IPI-dev-xml THEN DO:
                        RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula IPI Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                        ASSIGN tt-item-doc-est.aliquota-IPI = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE bf5-nfe-it-nota-fisc-rec.imp-ipi-pIPI
                               tt-item-doc-est.valor-IPI[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-valor ELSE bf5-nfe-it-nota-fisc-rec.imp-ipi-vIPI
                               tt-item-doc-est.base-ipi[1]  = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE bf5-nfe-it-nota-fisc-rec.imp-ipi-vBC.

                        ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + tt-item-doc-est.base-ipi[1]   /*variavel serÿ usada para criar a docum-est.baseipi*/
                               de-vl-total-vipi             = de-vl-total-vipi  + tt-item-doc-est.valor-IPI[1]. /*variavel serÿ usada para criar a docum-est.ipi-deb-cre*/
                    END.
        
                    /*ICMS Devolucao*/
                    IF l-ICMS-dev-xml THEN DO:
                        RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula ICMS Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                        ASSIGN bf-item-doc-est.aliquota-icm           = tt-item-doc-est.aliquota-icm        
                               bf-item-doc-est.base-icm[1]            = tt-item-doc-est.base-icm[1]          
                               bf-item-doc-est.valor-icm[1]           = tt-item-doc-est.valor-icm[1]   
                               bf-item-doc-est.val-perc-red-icms      = tt-item-doc-est.val-perc-red-icms.

                        CASE bf-item-doc-est.cd-trib-icm :
                            /*WHEN 2 THEN DO: /*Isento*/
                                ASSIGN bf-item-doc-est.base-icm[1]    = 0
                                       de-vl-total-vbicms             = 0.
                            END.*/
                            WHEN 3 THEN DO:
                                ASSIGN bf-item-doc-est.icm-outras[1]  = bf5-nfe-it-nota-fisc-rec.imp-vBC
                                       bf-item-doc-est.base-icm[1]    = 0.
                                ASSIGN de-vl-total-vicms              = de-vl-total-vicms  + tt-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms             = de-vl-total-vbicms + bf-item-doc-est.icm-outras[1].
                            END.
                            OTHERWISE
                                ASSIGN de-vl-total-vicms                   = de-vl-total-vicms  + tt-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms                  = de-vl-total-vbicms + tt-item-doc-est.base-icm[1].
                        END CASE.
                        
                        /*CST ICMS*/
                        IF l-CST-ICMS-xml THEN DO:
                            RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula CST ICMS Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                            IF bf5-nfe-nota-fiscal-rec.emit-crt = 1 THEN /*cst simples nacional*/
                                ASSIGN SUBSTR(bf-item-doc-est.char-2,502,3) = string(string(bf5-nfe-it-nota-fisc-rec.imp-orig) + "99") . /*CST ICMS*/
                            ELSE
                                ASSIGN SUBSTR(bf-item-doc-est.char-2,502,3) = string(string(bf5-nfe-it-nota-fisc-rec.imp-orig) + STRING(bf5-nfe-it-nota-fisc-rec.imp-CST,"99")) . /*CST ICMS*/
                        END.
                    END. 

                    /*ICMS ST Devolucao*/
                    IF l-ICMS-ST-dev-xml THEN DO:
                        RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula ICMS ST Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                        ASSIGN de-vl-total-vbicmssub     = de-vl-total-vbicmssub + tt-item-doc-est.BASE-SUBS[1]
                               de-vl-total-vicmssub      = de-vl-total-vicmssub + tt-item-doc-est.VL-SUBS[1]
                               bf-item-doc-est.VL-SUBS[1]   = tt-item-doc-est.VL-SUBS[1]
                               bf-item-doc-est.BASE-SUBS[1] = tt-item-doc-est.BASE-SUBS[1]
                               de-vl-total               = de-vl-total + tt-item-doc-est.VL-SUBS[1] + tt-item-doc-est.valor-ipi[1] + tt-item-doc-est.preco-total[1].
                    END.

                    /*CST ICMS do ERP*/
                    IF l-CST-ICMS-erp THEN DO:
                        IF bf5-nfe-nota-fiscal-rec.emit-crt <> 1 THEN DO: /*cst simples nacional*/
                            ASSIGN c-orig-cst = string(nfe-it-nota-fisc-rec.imp-orig).
                            
                            CASE bf-item-doc-est.cd-trib-icm :
                                WHEN 1 THEN DO: /*tributado*/
                                    IF bf-item-doc-est.log-icm-retido THEN
                                        ASSIGN c-cst-icms = "10". 
                                    ELSE
                                        ASSIGN c-cst-icms = "00". 
                                END.
                                WHEN 2 THEN DO: /*Isento*/
                                    IF bf-item-doc-est.log-icm-retido THEN
                                        ASSIGN c-cst-icms = "30". 
                                    IF bf-item-doc-est.log-icm-retido = NO
                                        AND natur-oper.ind-tipo-vat = NO THEN
                                        ASSIGN c-cst-icms = "40". 
                                    IF natur-oper.ind-tipo-vat THEN
                                        ASSIGN c-cst-icms = "41". 
                                END.
                                WHEN 3 THEN  /*Outros*/
                                    ASSIGN c-cst-icms = "90". 
                                WHEN 4 THEN DO: /*Reduzido*/
                                    IF bf-item-doc-est.log-icm-retido THEN
                                        ASSIGN c-cst-icms = "70". 
                                    ELSE
                                        ASSIGN c-cst-icms = "20". 
                                END.
                                OTHERWISE
                                    ASSIGN c-cst-icms = "90". 
                            END CASE.
                                
                            IF natur-oper.log-icms-substto-antecip THEN
                                ASSIGN c-cst-icms = "60". 
                            IF natur-oper.ind-it-sub-dif THEN
                                ASSIGN c-cst-icms = "50". 
                            IF natur-oper.ind-it-sub-dif = ? THEN
                                ASSIGN c-cst-icms = "51". 
                            
                            ASSIGN SUBSTR(bf-item-doc-est.char-2,502,3) = c-orig-cst + c-cst-icms.
                        END.
                    END.
                END.
            END.
        
            /*ICMS Devolucao*/
            IF l-ICMS-dev-xml THEN DO:
                RUN pi-acompanhar IN h-acomp-4 (INPUT "Total ICMS Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                ASSIGN bf-docum-est.icm-deb-cre = de-vl-total-vicms
                       bf-docum-est.base-icm    = de-vl-total-vbicms.
            END.

            /*IPI Devolucao - TOTAL BASE IPI*/
            IF l-IPI-dev-xml THEN DO:
                RUN pi-acompanhar IN h-acomp-4 (INPUT "Total IPI Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                ASSIGN bf-docum-est.base-ipi    = de-vl-total-vbipi
                       bf-docum-est.ipi-deb-cre = de-vl-total-vipi.
            END.
    
            /*ICMS ST Devolucao*/
            IF l-ICMS-ST-dev-xml THEN DO:
                RUN pi-acompanhar IN h-acomp-4 (INPUT "Total ICMS ST Devolucao: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                ASSIGN bf-docum-est.VL-SUBS   = de-vl-total-vicmssub
                       bf-docum-est.BASE-SUBS = de-vl-total-vbicmssub
                       bf-docum-est.tot-valor = de-vl-total + bf-docum-est.despesa-nota.
                       
            END.
        END.
    
        /*Outras notas diferente de devolucao*/
        ELSE DO:
            
            FIND FIRST bf5-nfe-nota-fiscal-rec
                WHERE bf5-nfe-nota-fiscal-rec.ide-Serie   = tt-item-doc-est.serie-docto
                  AND bf5-nfe-nota-fiscal-rec.ide-nNF     = tt-item-doc-est.nro-docto
                  AND bf5-nfe-nota-fiscal-rec.nome-abrev  = emitente.nome-abrev NO-LOCK NO-ERROR.
            
            
            IF AVAIL bf5-nfe-nota-fiscal-rec THEN DO:
                FIND FIRST bf5-nfe-it-nota-fisc-rec
                    WHERE bf5-nfe-it-nota-fisc-rec.chave-acesso = bf5-nfe-nota-fiscal-rec.chave-acesso-nfe
                      AND bf5-nfe-it-nota-fisc-rec.seq-item * 10 = tt-item-doc-est.sequencia        NO-ERROR.
                     
                IF AVAIL bf5-nfe-it-nota-fisc-rec THEN DO:
                    FIND FIRST nfe-it-imposto-alt NO-LOCK
                        WHERE nfe-it-imposto-alt.chave-acesso-nfe = bf5-nfe-nota-fiscal-rec.chave-acesso-nfe
                          AND nfe-it-imposto-alt.seq-item * 10    = tt-item-doc-est.sequencia        NO-ERROR.
                              
                    RUN pi-acompanhar IN h-acomp-4 (INPUT "ITEM Compra: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
        
                    /*IPI - TOTAL BASE IPI*/
                    IF l-IPI-xml THEN DO:
                        RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula IPI: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                        ASSIGN tt-item-doc-est.aliquota-IPI = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE bf5-nfe-it-nota-fisc-rec.imp-ipi-pIPI
                               tt-item-doc-est.valor-IPI[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-valor ELSE bf5-nfe-it-nota-fisc-rec.imp-ipi-vIPI
                               tt-item-doc-est.base-ipi[1]  = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE bf5-nfe-it-nota-fisc-rec.imp-ipi-vBC.

                        ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + tt-item-doc-est.base-ipi[1] /*variavel serÿ usada para criar a docum-est.baseipi*/
                               de-vl-total-vipi             = de-vl-total-vipi  + tt-item-doc-est.valor-IPI[1]. /*variavel serÿ usada para criar a docum-est.ipi-deb-cre*/
                    END.
        
                    
                    /*ICMS*/
                    IF l-ICMS-xml THEN DO:                                                      
                        RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula ICMS: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                        ASSIGN bf-item-doc-est.aliquota-icm           = tt-item-doc-est.aliquota-icm        
                               bf-item-doc-est.base-icm[1]            = tt-item-doc-est.base-icm[1]          
                               bf-item-doc-est.valor-icm[1]           = tt-item-doc-est.valor-icm[1]   
                               bf-item-doc-est.val-perc-red-icms      = tt-item-doc-est.val-perc-red-icms.

                        /*Bauducco Impostos caso os valores do xml Forem Zerados devemos aplicar a regra do ERP*/

/*                         IF bf-item-doc-est.cd-trib-icm = 1 OR        /*Tributado*/                                                            */
/*                            bf-item-doc-est.cd-trib-icm = 3 OR        /*outros*/                                                               */
/*                            bf-item-doc-est.cd-trib-icm = 4 THEN DO:  /*Reduzido*/                                                             */
/*                                                                                                                                               */
/*                                                                                                                                               */
/*                             IF bf-item-doc-est.aliquota-icm = 0 AND                                                                           */
/*                                bf-item-doc-est.base-icm[1]  = 0 AND                                                                           */
/*                                bf-item-doc-est.valor-icm[1] = 0 THEN DO:                                                                      */
/*                                                                                                                                               */
/*                                                                                                                                               */
/*                                 RUN pi_aliquota_icm(INPUT emitente.contrib-icms     ,                                                         */
/*                                                     INPUT emitente.natureza         ,                                                         */
/*                                                     INPUT emitente.estado              ,                                                      */
/*                                                     INPUT emitente.pais                ,                                                      */
/*                                                     INPUT estabelec.estado             ,                                                      */
/*                                                     INPUT bf-item-doc-est.it-codigo    ,                                                      */
/*                                                     INPUT IF l-multi_natureza THEN bf-item-doc-est.nat-of ELSE bf-item-doc-est.nat-operacao , */
/*                                                     OUTPUT bf-item-doc-est.aliquota-icm).                                                     */
/*                                                                                                                                               */
/*                             END.                                                                                                              */
/*                                                                                                                                               */
/*                         END.                                                                                                                  */



                        CASE bf-item-doc-est.cd-trib-icm :
                            /*WHEN 2 THEN DO: /*Isento*/
                                ASSIGN bf-item-doc-est.base-icm[1]    = 0
                                       de-vl-total-vbicms             = 0.
                            END.*/
                            WHEN 3 THEN DO:
                                ASSIGN bf-item-doc-est.icm-outras[1]  = bf5-nfe-it-nota-fisc-rec.imp-vBC
                                       bf-item-doc-est.base-icm[1]    = 0.
                                ASSIGN de-vl-total-vicms              = de-vl-total-vicms  + tt-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms             = de-vl-total-vbicms + bf-item-doc-est.icm-outras[1].
                            END.
                            OTHERWISE
                                ASSIGN de-vl-total-vicms                   = de-vl-total-vicms  + tt-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms                  = de-vl-total-vbicms + tt-item-doc-est.base-icm[1].
                        END CASE.

                        /*CST ICMS*/
                        IF l-CST-ICMS-xml THEN DO:
                            RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula CST ICMS: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                            IF bf5-nfe-nota-fiscal-rec.emit-crt = 1 THEN
                                ASSIGN SUBSTR(bf-item-doc-est.char-2,502,3) = string(string(bf5-nfe-it-nota-fisc-rec.imp-orig) + "99") . /*CST ICMS*/
                            ELSE
                                ASSIGN SUBSTR(bf-item-doc-est.char-2,502,3) = string(string(bf5-nfe-it-nota-fisc-rec.imp-orig) + STRING(bf5-nfe-it-nota-fisc-rec.imp-CST,"99")) . /*CST ICMS*/
                        END.


                        



                    END.                                              
            
                    /*ICMS ST*/
                    IF l-ICMS-ST-xml THEN DO:
                        RUN pi-acompanhar IN h-acomp-4 (INPUT "Calcula ICMS ST: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                        ASSIGN de-vl-total-vbicmssub     = de-vl-total-vbicmssub + tt-item-doc-est.BASE-SUBS[1]
                               de-vl-total-vicmssub      = de-vl-total-vicmssub + tt-item-doc-est.VL-SUBS[1]
                               bf-item-doc-est.VL-SUBS[1]   = tt-item-doc-est.VL-SUBS[1]
                               bf-item-doc-est.BASE-SUBS[1] = tt-item-doc-est.BASE-SUBS[1]
                               de-vl-total               = de-vl-total + tt-item-doc-est.VL-SUBS[1] + tt-item-doc-est.valor-ipi[1] + tt-item-doc-est.preco-total[1].
                    END.

                    /*CST ICMS do ERP*/
                    IF l-CST-ICMS-erp THEN DO:
                        IF bf5-nfe-nota-fiscal-rec.emit-crt <> 1 THEN DO: /*cst simples nacional*/
                            ASSIGN c-orig-cst = string(bf5-nfe-it-nota-fisc-rec.imp-orig).
                            
                            CASE bf-item-doc-est.cd-trib-icm :
                                WHEN 1 THEN DO: /*tributado*/
                                    IF bf-item-doc-est.log-icm-retido THEN
                                        ASSIGN c-cst-icms = "10". 
                                    ELSE
                                        ASSIGN c-cst-icms = "00". 
                                END.
                                WHEN 2 THEN DO: /*Isento*/
                                    IF bf-item-doc-est.log-icm-retido THEN
                                        ASSIGN c-cst-icms = "30". 
                                    IF bf-item-doc-est.log-icm-retido = NO
                                        AND natur-oper.ind-tipo-vat = NO THEN
                                        ASSIGN c-cst-icms = "40". 
                                    IF natur-oper.ind-tipo-vat THEN
                                        ASSIGN c-cst-icms = "41". 
                                END.
                                WHEN 3 THEN  /*Outros*/
                                    ASSIGN c-cst-icms = "90". 
                                WHEN 4 THEN DO: /*Reduzido*/
                                    IF bf-item-doc-est.log-icm-retido THEN
                                        ASSIGN c-cst-icms = "70". 
                                    ELSE
                                        ASSIGN c-cst-icms = "20". 
                                END.
                                OTHERWISE
                                    ASSIGN c-cst-icms = "90". 
                            END CASE.
                                
                            IF natur-oper.log-icms-substto-antecip THEN
                                ASSIGN c-cst-icms = "60". 
                            IF natur-oper.ind-it-sub-dif THEN
                                ASSIGN c-cst-icms = "50". 
                            IF natur-oper.ind-it-sub-dif = ? THEN
                                ASSIGN c-cst-icms = "51". 
                            
                            ASSIGN SUBSTR(bf-item-doc-est.char-2,502,3) = c-orig-cst + c-cst-icms.
                        END.
                    END.
                END.
            END.
        
            /*ICMS*/
            IF l-ICMS-xml THEN DO:
                RUN pi-acompanhar IN h-acomp-4 (INPUT "Total ICMS: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                ASSIGN bf-docum-est.icm-deb-cre = de-vl-total-vicms
                       bf-docum-est.base-icm    = de-vl-total-vbicms.
            END.

            /*IPI - TOTAL BASE IPI*/
            IF l-IPI-xml THEN DO:
                RUN pi-acompanhar IN h-acomp-4 (INPUT "Total IPI: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                ASSIGN bf-docum-est.base-ipi = de-vl-total-vbipi
                       bf-docum-est.ipi-deb-cre = de-vl-total-vipi.
            END.
    
            /*ICMS ST*/
            IF l-ICMS-ST-xml THEN DO:
                RUN pi-acompanhar IN h-acomp-4 (INPUT "Total ICMS ST: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
                ASSIGN bf-docum-est.VL-SUBS   = de-vl-total-vicmssub
                       bf-docum-est.BASE-SUBS = de-vl-total-vbicmssub
                       bf-docum-est.tot-valor = de-vl-total + bf-docum-est.despesa-nota.
                            END.
        END.
    END. 

    FOR EACH consist-nota EXCLUSIVE-LOCK
         where consist-nota.cod-emitente = tt-item-doc-est.cod-emitente
           and consist-nota.nat-operacao = tt-item-doc-est.nat-operacao
           and consist-nota.serie-docto  = tt-item-doc-est.serie-docto
           and consist-nota.nro-docto    = tt-item-doc-est.nro-docto
           AND consist-nota.mensagem = 5506:

        RUN pi-acompanhar IN h-acomp-4 (INPUT "Deleta consist-nota. " + " ITEM: " + tt-item-doc-est.it-codigo + " seq. " + string(tt-item-doc-est.sequencia)).
        DELETE consist-nota.                                                                                                                              
    END.
                               
    IF AVAIL tt-item-doc-est THEN DO:
        FIND FIRST bf-docum-est WHERE 
                   bf-docum-est.serie-docto    = tt-item-doc-est.serie-docto  AND 
                   bf-docum-est.nro-docto      = tt-item-doc-est.nro-docto    AND 
                   bf-docum-est.cod-emitente   = tt-item-doc-est.cod-emitente AND 
                   bf-docum-est.nat-operacao   = tt-item-doc-est.nat-operacao NO-LOCK NO-ERROR.

        IF AVAIL bf-docum-est THEN DO:
            RUN setHandleDocumEst      IN h_boin176 (INPUT h_boin090).
            RUN findDocumento          IN h_boin176 (bf-item-doc-est.cod-emitente,
                                                     bf-item-doc-est.serie-docto,
                                                     bf-item-doc-est.nro-docto,
                                                     bf-item-doc-est.nat-operacao).

            RUN getTotalizaNota        IN h_boin176 (INPUT 0).
            RUN transferTotalItensNota IN h_boin176 (INPUT bf-docum-est.cod-emitente,
                                                     INPUT bf-docum-est.serie-docto,
                                                     INPUT bf-docum-est.nro-docto,
                                                     INPUT bf-docum-est.nat-operacao).

            RUN validateValues IN h_boin090.
        END.
    END.

    IF VALID-HANDLE(h_boin176) THEN DELETE PROCEDURE h_boin176.
    IF VALID-HANDLE(h_boin090) THEN DELETE PROCEDURE h_boin090.
RUN pi-finalizar IN h-acomp-4.
END PROCEDURE. /* pi_recalcula_imposto */

/**************************************************************************************************************************/

PROCEDURE pi_nfe_1_compra:
    
    /* --- Ordem de Compra --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "Oc=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux, 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = INTE(c-aux).
    END.

    /* --- Serie/Lote --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "ltP=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux, 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-ser-lote = c-aux.
    END.
        
    /* --- Validade Lote --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "dVal=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux, 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-dt-vali-lote = DATE(c-aux).
    END.

    /* --- Refer?ncia --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "cRef=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux, 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-cod-refer = c-aux.
    END.
    
    /* --- Ordem de Producao --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "Os=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux, 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-nr-ord-produ = INTE(c-aux).
    END.

    /* --- Busca Ordem de Compra --- */

/*     IF nfe-it-nota-fisc-rec.item-num-ordem = 0 THEN           metalfrio                                */
/*     DO:                                                                                       */
/*         FIND FIRST ordem-compra WHERE                                                         */
/*                    ordem-compra.it-codigo  = nfe-it-nota-fisc-rec.it-codigo AND               */
/*                    ordem-compra.num-pedido = nfe-it-nota-fisc-rec.item-xPed AND               */
/*                    ordem-compra.situacao   = 2 /* --- Confirmada --- */     NO-LOCK NO-ERROR. */
/*                                                                                               */
/*         IF AVAIL ordem-compra THEN                                                            */
/*             ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = ordem-compra.numero-ordem.           */
/*     END.                                                                                      */

    
    /*Nova regra de Busca Ordem/Parcela */

    /*Leia - 07-04-14*/
    ASSIGN xped-char-aux = ''.

    DO i-contador = 1 TO LENGTH(nfe-it-nota-fisc-rec.item-xped-char):

       IF (ASC(SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) >= 48) AND
          (ASC(SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) <= 57) THEN 
             ASSIGN xped-char-aux = xped-char-aux + (SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)).
    END.

    FIND FIRST prazo-compra NO-LOCK
        WHERE prazo-compra.numero-ordem = int(xped-char-aux) /*int(nfe-it-nota-fisc-rec.item-xPed-Char)*/
        AND   prazo-compra.parcela      = nfe-it-nota-fisc-rec.item-nItemPEd  NO-ERROR.

    IF AVAIL prazo-compra  THEN DO:

        ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = prazo-compra.numero-ordem
               nfe-it-nota-fisc-rec.item-parcela   = prazo-compra.parcela     .
    END.
    ELSE DO:

        FIND FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = int(xped-char-aux)/*int(nfe-it-nota-fisc-rec.item-xPed-Char)*/ NO-ERROR.
        IF AVAIL ordem-compra THEN
            ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = ordem-compra.numero-ordem.


    END.


    
    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "NotaDeCompra"
           tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

    {include\i-epc201.i "NotaDeCompra"}

END PROCEDURE. /* pi_nfe_1_compra */

/**************************************************************************************************************************/

PROCEDURE pi_nfe_2_devol_cli :
    /*
    FIND FIRST emitente WHERE 
               emitente.cgc = nfe-nota-fiscal-rec.cgc NO-LOCK NO-ERROR.*/

    FIND emitente WHERE 
         emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

    FIND FIRST item-cli WHERE 
               item-cli.nome-abrev  = emitente.nome-abrev             AND 
               item-cli.item-do-cli = nfe-it-nota-fisc-rec.item-cprod NO-LOCK NO-ERROR.

    IF AVAIL item-cli THEN 
        ASSIGN nfe-it-nota-fisc-rec.it-codigo = item-cli.it-codigo.
    ELSE 
        ASSIGN nfe-it-nota-fisc-rec.it-codigo = "".
    
    /* --- Numero da Nota de Venda Original --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "nNFO=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-nr-nota-fis = FILL("0",7 - LENGTH(TRIM(c-aux))) + TRIM(c-aux).
    END.
        
    /* --- Serie da Nota de Venda Original --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "sNFO=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-serie-nf = c-aux.
    END.

    FIND FIRST ITEM WHERE 
               ITEM.it-codigo = nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.

    IF AVAIL ITEM THEN DO:
        IF ITEM.tipo-con-est = 1 OR       /* --- Serial       --- */
           ITEM.tipo-con-est = 2 OR       /* --- Numero Serie --- */
           ITEM.tipo-con-est = 3 OR       /* --- Por Lote     --- */
           ITEM.tipo-con-est = 4 THEN DO:    /* --- Referencia   --- */

            FIND FIRST nota-fiscal WHERE 
                       nota-fiscal.cod-estabel  = nfe-nota-fiscal-rec.cod-estabel       AND 
                       nota-fiscal.serie        = nfe-it-nota-fisc-rec.item-serie-nf    AND 
                       nota-fiscal.nr-nota-fis  = nfe-it-nota-fisc-rec.item-nr-nota-fis NO-LOCK NO-ERROR.

            IF AVAIL nota-fiscal THEN DO:
                FIND FIRST it-nota-fisc WHERE 
                           it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel        AND 
                           it-nota-fisc.serie       = nota-fiscal.serie              AND 
                           it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis        AND 
                           it-nota-fisc.it-codigo   = nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.

                IF AVAIL it-nota-fisc THEN DO:
                    ASSIGN nfe-it-nota-fisc-rec.item-num-ordem      = it-nota-fisc.nr-ordem    
                           nfe-it-nota-fisc-rec.item-xPed           = it-nota-fisc.nr-pedido   
                           nfe-it-nota-fisc-rec.item-nr-ord-prod    = it-nota-fisc.nr-ord-produ
                           nfe-it-nota-fisc-rec.item-ncm            = it-nota-fisc.class-fiscal
                           nfe-it-nota-fisc-rec.item-cod-refer      = it-nota-fisc.cod-refer   
                           nfe-it-nota-fisc-rec.item-cod-depos      = it-nota-fisc.cod-depos. 
    
                    FIND FIRST fat-ser-lote OF it-nota-fisc NO-LOCK NO-ERROR.

                    IF AVAIL fat-ser-lote THEN
                        ASSIGN nfe-it-nota-fisc-rec.item-ser-lote     = fat-ser-lote.nr-serlote
                               nfe-it-nota-fisc-rec.item-dt-vali-lote = fat-ser-lote.dt-vali-lote.
                END.
            END.
        END.
    END. /* IF AVAIL ITEM..... */

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "NotaDevolucaoCliente"
           tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

    {include\i-epc201.i "NotaDevolucaoCliente"}

END PROCEDURE. /* pi_nfe_2_devol_cli */

/**************************************************************************************************************************/

PROCEDURE pi_nfe_3_retorno_terc:
    
    /* --- 2 - Retorno Beneficiamento           --- */
    /* --- 4 - Faturamento Consignacao          --- */
    /* --- 5 - Retorno / Devolucao Consignacao  --- */

    /* --- N?mero do Documento de Remessa Original --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "nNFO=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-nro-docto = FILL("0",7 - LENGTH(TRIM(c-aux))) + TRIM(c-aux).
    END.
    
    /* --- N?mero de Serie de Remessa Original --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "sNFO=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-serie-docto = c-aux.
    END.
    
    /* --- Ordem de Producao --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "Os=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').

                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-nr-ord-produ = INTE(c-aux).
    END.

    /* --- Faturamento Consignacao --- */

    IF i-tp-oper-terc = 4 THEN DO:
        /* --- Serie/Lote --- */

        ASSIGN i-aux2 = 0
               c-aux  = ""
               c-tag  = "ltP=".

        IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
        DO:
            DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
                IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                    ASSIGN i-aux2 = i-aux2 + 1.

                ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

                IF i-aux2 = 2 THEN 
                DO:
                    ASSIGN i-aux2 = 0
                           c-aux  = REPLACE(c-aux,'"','').
                    LEAVE.
                END.
            END.

            ASSIGN nfe-it-nota-fisc-rec.item-ser-lote = c-aux.
        END.

        /* --- Validade Lote --- */

        ASSIGN i-aux2 = 0
               c-aux  = ""
               c-tag  = "dVal=".

        IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
        DO:
            DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
                IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                    ASSIGN i-aux2 = i-aux2 + 1.

                ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

                IF i-aux2 = 2 THEN 
                DO:
                    ASSIGN i-aux2 = 0
                           c-aux  = REPLACE(c-aux,'"','').

                    LEAVE.
                END.
            END.

            ASSIGN nfe-it-nota-fisc-rec.item-dt-vali-lote = DATE(c-aux).
        END.
        
        /* --- Refer?ncia --- */

        ASSIGN i-aux2 = 0
               c-aux  = ""
               c-tag  = "cRef=".

        IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
        DO:
            DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
                IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                    ASSIGN i-aux2 = i-aux2 + 1.

                ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

                IF i-aux2 = 2 THEN 
                DO:
                    ASSIGN i-aux2 = 0
                           c-aux  = REPLACE(c-aux,'"','').
                    LEAVE.
                END.
            END.

            ASSIGN nfe-it-nota-fisc-rec.item-cod-refer = c-aux.
        END.

    /*Leia - 07-04-14*/
    ASSIGN xped-char-aux = ''.

    DO i-contador = 1 TO LENGTH(nfe-it-nota-fisc-rec.item-xped-char):

       IF (ASC(SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) >= 48) AND
          (ASC(SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) <= 57) THEN 
             ASSIGN xped-char-aux = xped-char-aux + (SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)).
    END.

    FIND FIRST prazo-compra NO-LOCK
        WHERE prazo-compra.numero-ordem = int(xped-char-aux) /*int(nfe-it-nota-fisc-rec.item-xPed-Char)*/
        AND   prazo-compra.parcela      = nfe-it-nota-fisc-rec.item-nItemPEd  NO-ERROR.

    IF AVAIL prazo-compra  THEN DO:

        ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = prazo-compra.numero-ordem
               nfe-it-nota-fisc-rec.item-parcela   = prazo-compra.parcela     .
    END.
    ELSE DO:

        FIND FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = int(xped-char-aux) /*int(nfe-it-nota-fisc-rec.item-xPed-Char)*/ NO-ERROR.
        IF AVAIL ordem-compra THEN
            ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = ordem-compra.numero-ordem.


    END.

        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "NotaRetornoTerceiros(FatConsig)"
               tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

        {include\i-epc201.i "NotaRetornoTerceiros(FatConsig)"}
    END.

    FIND emitente WHERE 
         emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

    IF AVAIL emitente THEN DO:
        /* --- Busca Nota de Remessa --- */

        ASSIGN c-item-nro-docto-ini     = IF TRIM(nfe-it-nota-fisc-rec.item-nro-docto) <> "" THEN 
                                              nfe-it-nota-fisc-rec.item-nro-docto 
                                          ELSE 
                                              ""
               c-item-nro-docto-fim     = IF TRIM(nfe-it-nota-fisc-rec.item-nro-docto) <> "" THEN 
                                              nfe-it-nota-fisc-rec.item-nro-docto 
                                          ELSE 
                                              "ZZZZZZZZZZZZZZZZ"
               c-item-serie-docto-ini   = IF TRIM(nfe-it-nota-fisc-rec.item-serie-docto) <> "" THEN 
                                              nfe-it-nota-fisc-rec.item-serie-docto 
                                          ELSE 
                                              ""
               c-item-serie-docto-fim   = IF TRIM(nfe-it-nota-fisc-rec.item-serie-docto) <> "" THEN 
                                              nfe-it-nota-fisc-rec.item-serie-docto 
                                          ELSE 
                                              "ZZZZZ".
        FOR EACH saldo-terc WHERE 
                 saldo-terc.cod-emitente    = emitente.cod-emitente           AND 
                 saldo-terc.it-codigo       = nfe-it-nota-fisc-rec.it-codigo  AND 
                 saldo-terc.cod-estabel     = nfe-nota-fiscal-rec.cod-estabel AND 
                 saldo-terc.serie-docto    >= c-item-serie-docto-ini          AND 
                 saldo-terc.serie-docto    <= c-item-serie-docto-fim          AND 
                 saldo-terc.nro-docto      >= c-item-nro-docto-ini            AND 
                 saldo-terc.nro-docto      <= c-item-nro-docto-fim            NO-LOCK 
            BY saldo-terc.sequencia:
            
            IF (saldo-terc.quantidade - saldo-terc.dec-1) <= 0 THEN NEXT.

            ASSIGN de-qtde-it-terc = 0.

            FOR EACH bf-nfe-it-nota-fisc-rec WHERE 
                     bf-nfe-it-nota-fisc-rec.chave-acesso    = nfe-it-nota-fisc-rec.chave-acesso AND 
                     bf-nfe-it-nota-fisc-rec.it-codigo       = nfe-it-nota-fisc-rec.it-codigo    AND 
                     bf-nfe-it-nota-fisc-rec.item-serie-comp = saldo-terc.serie-docto            AND 
                     bf-nfe-it-nota-fisc-rec.item-nro-comp   = saldo-terc.nro-docto              AND 
                     bf-nfe-it-nota-fisc-rec.item-nat-comp   = saldo-terc.nat-operacao           AND 
                     bf-nfe-it-nota-fisc-rec.item-seq-comp   = saldo-terc.sequencia              AND 
                     bf-nfe-it-nota-fisc-rec.item-data-comp  = saldo-terc.dt-retorno             NO-LOCK:
                ASSIGN de-qtde-it-terc = de-qtde-it-terc + bf-nfe-it-nota-fisc-rec.item-qtde.
            END.

            IF (nfe-it-nota-fisc-rec.item-qtde + de-qtde-it-terc) > (saldo-terc.quantidade - saldo-terc.dec-1) THEN NEXT.

            ASSIGN nfe-it-nota-fisc-rec.item-cod-refer    = saldo-terc.cod-refer
                   nfe-it-nota-fisc-rec.item-cod-depos    = saldo-terc.cod-depos
                   nfe-it-nota-fisc-rec.item-cod-localiz  = saldo-terc.cod-localiz
                   nfe-it-nota-fisc-rec.item-ser-lote     = saldo-terc.lote
                   nfe-it-nota-fisc-rec.item-nr-ord-produ = saldo-terc.nr-ord-produ
                   nfe-it-nota-fisc-rec.item-serie-comp   = saldo-terc.serie-docto
                   nfe-it-nota-fisc-rec.item-nro-comp     = saldo-terc.nro-docto
                   nfe-it-nota-fisc-rec.item-nat-comp     = saldo-terc.nat-operacao
                   nfe-it-nota-fisc-rec.item-seq-comp     = saldo-terc.sequencia
                   nfe-it-nota-fisc-rec.item-data-comp    = saldo-terc.dt-retorno
                   nfe-it-nota-fisc-rec.item-serie-docto  = saldo-terc.serie-docto
                   nfe-it-nota-fisc-rec.item-nro-docto    = saldo-terc.nro-docto.

            FIND FIRST saldo-estoq WHERE 
                       saldo-estoq.it-codigo = saldo-terc.it-codigo AND 
                       saldo-estoq.lote      = saldo-terc.lote      NO-LOCK NO-ERROR.

            IF AVAIL saldo-estoq THEN 
                ASSIGN nfe-it-nota-fisc-rec.item-dt-vali-lote = saldo-estoq.dt-vali-lote.

            ASSIGN rw-saldo-terc = ROWID(saldo-terc).
        END.
    END.

    /* --- Retorno Beneficiamento --- */

    IF i-tp-oper-terc  = 2 THEN DO:
        FIND FIRST saldo-terc WHERE 
                   ROWID(saldo-terc) = rw-saldo-terc NO-LOCK NO-ERROR.

        FIND FIRST bf-ord-prod WHERE 
                   bf-ord-prod.nr-ord-produ = nfe-it-nota-fisc-rec.item-nr-ord-produ NO-LOCK NO-ERROR.

        IF NOT AVAIL bf-ord-prod AND
           AVAIL saldo-terc      THEN 
            ASSIGN nfe-it-nota-fisc-rec.item-nr-ord-produ = saldo-terc.nr-ord-produ.

        FIND FIRST ord-prod WHERE 
                   ord-prod.nr-ord-produ = nfe-it-nota-fisc-rec.item-nr-ord-produ NO-LOCK NO-ERROR.

        IF AVAIL ord-prod THEN
        DO:
            &IF '{&bf_dis_versao_ems}' <= '2.07' &THEN 
               ASSIGN nfe-it-nota-fisc-rec.item-nr-ord-produ   = ord-prod.nr-ord-produ
                      nfe-it-nota-fisc-rec.item-conta-contabil = ord-prod.conta-ordem.
            &ELSE
               ASSIGN nfe-it-nota-fisc-rec.item-conta-contabil = ord-prod.ct-codigo.
            &ENDIF
        END.
        ELSE
            ASSIGN nfe-it-nota-fisc-rec.item-nr-ord-produ   = 0.

        /* --- Verifica Parametros Globais --- */

        FIND FIRST nfe-param-rec WHERE 
                   nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

        IF AVAIL nfe-param-rec THEN DO:
            /* --- Verifica Dep®sito Retorno Beneficiamento --- */

            FIND FIRST nfe-it-param-rec WHERE 
                       nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                       nfe-it-param-rec.cod-item-parametro = "depos_retorno_benef"       NO-LOCK NO-ERROR.

            IF AVAIL nfe-it-param-rec                              AND
               TRIM(nfe-it-param-rec.valor-1-item-parametro) <> "" THEN
                ASSIGN nfe-it-nota-fisc-rec.item-cod-depos = nfe-it-param-rec.valor-1-item-parametro.
        END.

        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "NotaRetornoTerceiros(RetBenef)"
               tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

        {include\i-epc201.i "NotaRetornoTerceiros(RetBenef)"}
    END.
        
    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "NotaRetornoTerceiros"
           tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

    {include\i-epc201.i "NotaRetornoTerceiros"}

END PROCEDURE. /* pi_nfe_3_retorno_terc */

/**************************************************************************************************************************/

PROCEDURE pi_nfe_4_remessa_terc:
    
    /* --- Serie/Lote --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "ltP=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').
                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-ser-lote = c-aux.
    END.
    
    /* --- Validade Lote --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "dVal=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').
                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-dt-vali-lote = DATE(c-aux).
    END.

    /* --- Refer?ncia --- */

    ASSIGN i-aux2 = 0
           c-aux  = ""
           c-tag  = "cRef=".

    IF INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) <> 0 THEN 
    DO:
        DO i-aux = (INDEX(nfe-it-nota-fisc-rec.inf-infAdProd,c-tag) + LENGTH(c-tag)) TO LENGTH(nfe-it-nota-fisc-rec.inf-infAdProd):
            IF SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd, i-aux, 1) = '"' THEN 
                ASSIGN i-aux2 = i-aux2 + 1.

            ASSIGN c-aux = c-aux + SUBSTR(nfe-it-nota-fisc-rec.inf-infAdProd,i-aux , 1).

            IF i-aux2 = 2 THEN 
            DO:
                ASSIGN i-aux2 = 0
                       c-aux  = REPLACE(c-aux,'"','').
                LEAVE.
            END.
        END.

        ASSIGN nfe-it-nota-fisc-rec.item-cod-refer = c-aux.
    END.

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "NotaRemessaTerceiros"
           tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).

    {include\i-epc201.i "NotaRemessaTerceiros"}

END PROCEDURE. /* pi_nfe_4_remessa_terc */

/**************************************************************************************************************************/

PROCEDURE pi_retorna_fornecedor:
    
    DEF INPUT  PARAM p_origem        AS INTE                             NO-UNDO.
    DEF INPUT  PARAM p_rowid_danfe   AS ROWID                            NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    /** p_origem(1) - PAINEL NFE                               **
     ** p_origem(2) - TELAS RECEBIMENTO (UPC) FISICO E FISCAL  **/

    /* --- Valida Diret®rios do Recebimento --- */

    RUN pi_valida_diretorios (OUTPUT p_mensagem_erro).

    IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.
    
    FIND FIRST nfe-nota-fiscal-rec WHERE 
               ROWID(nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    IF p_origem = 1 THEN 
    DO:
        /* --- Status Recebimento para Fornecedor --- */

        RUN dsc\ra\esp\esnfe200c.w (INPUT nfe-nota-fiscal-rec.cgc,
                                    INPUT nfe-nota-fiscal-rec.chave-acesso-nfe,
                                    OUTPUT rw-retorna-fornec).
    END.
    ELSE 
    DO:
        /* --- Pela UPC do Recebimento RE1001 e RE2001 Retorna Fornecedor Automatico --- */

        FIND FIRST nfe-retorna-forn-rec WHERE 
                   nfe-retorna-forn-rec.codigo = 1 /* --- Aceita sem Restri??es --- */ NO-LOCK NO-ERROR.

        ASSIGN rw-retorna-fornec = IF AVAIL nfe-retorna-forn-rec THEN ROWID(nfe-retorna-forn-rec) ELSE ?.
    END.
        
    IF rw-retorna-fornec <> ? THEN 
    DO:
        ASSIGN c-diretorio = nfe-it-param-rec.valor-4-item-parametro /* --- Retorna Fornecedor --- */
               c-arquivo   = nfe-nota-fiscal-rec.chave-acesso-nfe.

        RUN pi_valida_barra (INPUT-OUTPUT c-diretorio).
    
        /* --- Verifica se Diret®rio Retorna Fornecedor e por ESTAB --- */

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "dir_retorno_por_estab"     NO-LOCK NO-ERROR.

        IF AVAIL nfe-it-param-rec                           AND
           nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN 
        DO:
            ASSIGN c-aux = c-diretorio + nfe-nota-fiscal-rec.cod-estabel + "\".

            /* --- Verifica se Diret®rio Existe --- */

            FILE-INFO:FILE-NAME = c-aux.

            IF FILE-INFO:FULL-PATHNAME = ?     AND
               FILE-INFO:FILE-TYPE    <> "DRW" THEN 
            DO:
                OS-CREATE-DIR VALUE(c-aux).

                ASSIGN i-erro = OS-ERROR.

                IF i-erro <> 0 THEN 
                DO:
                    MESSAGE "Diretorio " nfe-nota-fiscal-rec.cod-estabel " nao foi criado. Erro do sistema #" i-erro
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.

                    LEAVE.
                END.
            END.

            ASSIGN c-diretorio = c-aux.
        END.

        /* --- Cabe?alho --- */

        hXML:CREATE-NODE(hRoot, "reqNFeRetRecebimento", "ELEMENT":U).
        hXML:APPEND-CHILD(hRoot).
        hRoot:SET-ATTRIBUTE ("tpDocumento", "NFE").
        hRoot:SET-ATTRIBUTE ("versao", "1.10").
            
        FIND FIRST nfe-retorna-forn-rec WHERE 
                   ROWID(nfe-retorna-forn-rec) = rw-retorna-fornec NO-LOCK NO-ERROR.

        CASE nfe-retorna-forn-rec.codigo :
            WHEN 1 THEN ASSIGN c-situacao = "ACEITA".
            WHEN 2 THEN ASSIGN c-situacao = "ACEITA_RESTRICAO".
            WHEN 3 THEN ASSIGN c-situacao = "REJEITADA".
        END CASE.
            
        ASSIGN c-lista = "situacao|codMotivo|descMotivo|chNFe"
               c-valor = TRIM(c-situacao," ")                       + '|' +
                         TRIM(nfe-retorna-forn-rec.cod-status," ")  + '|' +
                         TRIM(nfe-retorna-forn-rec.desc-status," ") + '|' +
                         nfe-nota-fiscal-rec.chave-acesso-nfe       + '|'.

        REPEAT i-cont = 1 to 4:
            hXML:CREATE-NODE(hRecordAux, ENTRY(i-cont,c-lista,'|'), "ELEMENT":U).

            hRoot:APPEND-CHILD(hRecordAux).
                
            hXML:CREATE-NODE(hRecordAuxValue, "reqNFeRetRecebimento":U, "Text":U).

            hRecordAux:APPEND-CHILD(hRecordAuxValue).

            hRecordAuxValue:NODE-VALUE = ENTRY(i-cont,c-valor,'|').
        END.
            
        hXML:SAVE("FILE":U, c-diretorio + "REC_" + c-arquivo + ".xml.txt").

        /* ---- Atualiza Situacao da NFE e Hist®rico --- */
/*Nao existe mais a funcao*/
/*         IF nfe-retorna-forn-rec.codigo = 1 OR                                                      */
/*            nfe-retorna-forn-rec.codigo = 2 THEN                                                    */
/*             RUN pi_atualiza_status (INPUT ROWID(nfe-nota-fiscal-rec),                              */
/*                                     INPUT 2). /* --- Retorna Aceita ou Aceita com Resticoes --- */ */
/*         ELSE                                                                                       */
/*             RUN pi_atualiza_status (INPUT ROWID(nfe-nota-fiscal-rec),                              */
/*                                     INPUT 3). /* --- Retorna Rejeita --- */                        */

        CREATE nfe-his-nota-fis-rec.

        ASSIGN nfe-his-nota-fis-rec.chave-acesso-nfe = nfe-nota-fiscal-rec.chave-acesso-nfe
               nfe-his-nota-fis-rec.codigo           = nfe-retorna-forn-rec.codigo
               nfe-his-nota-fis-rec.cod-status       = nfe-retorna-forn-rec.cod-status
               nfe-his-nota-fis-rec.desc-status      = nfe-retorna-forn-rec.desc-status
               nfe-his-nota-fis-rec.desc-situacao    = c-situacao
               nfe-his-nota-fis-rec.dt-retorno       = TODAY
               nfe-his-nota-fis-rec.hr-retorno       = STRING(TIME,"HH:MM:SS")
               nfe-his-nota-fis-rec.dt-Recbto        = TODAY
               nfe-his-nota-fis-rec.hr-Recbto        = STRING(TIME,"HH:MM:SS")
               nfe-his-nota-fis-rec.ind-origem       = 1. /* --- 1 - Retorna Fornecedor / 2 - Sefaz --- */
    END.
            
END PROCEDURE. /* pi_retorna_fornecedor */

/**************************************************************************************************************************/

PROCEDURE pi_retorno_sefaz_dscrc :
    
    DEF INPUT  PARAM p_rowid_danfe   AS ROWID                            NO-UNDO.
    DEF OUTPUT PARAM p_l_encontrou   AS LOG                  INITIAL NO  NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    /* --- Valida Diret®rios do Recebimento Sefaz --- */

    FIND FIRST nfe-param-rec WHERE 
               nfe-param-rec.cod-parametro = "diretorios_recebimento_nfe" NO-LOCK NO-ERROR.

    FIND FIRST nfe-it-param-rec WHERE 
               nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
               nfe-it-param-rec.cod-item-parametro = "diretorios"                NO-LOCK NO-ERROR.

    IF AVAIL nfe-it-param-rec THEN
        ASSIGN c-nome-dir-rec            = nfe-it-param-rec.valor-1-item-parametro  /* --- Diretorio de Entrada do Arquivo Retorno Sefaz  --- */
               c-nome-dir-rec-sefaz-dest = nfe-it-param-rec.valor-3-item-parametro. /* --- Diretorio de Lidos Retorno Sefaz               --- */
               c-nome-dir-erro           = nfe-it-param-rec.valor-5-item-parametro. /* --- NFE Erro                                       --- */

    RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec).

    RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-rec-sefaz-dest).

    RUN pi_valida_barra (INPUT-OUTPUT c-nome-dir-erro).

    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    IF AVAIL bfu-nfe-nota-fiscal-rec THEN 
    DO:
        INPUT FROM OS-DIR(c-nome-dir-rec).
            REPEAT:
                IMPORT c-file.

                IF INDEX(c-file,"xml") = 0 THEN NEXT.
    
                /* --- Se tiver Espaco em Branco no Nome XML retira espacos --- */

                IF INDEX(c-file," ") <> 0 THEN 
                DO:
                    /* --- Renomeia para nome sem Espa?o --- */

                    OS-RENAME VALUE(c-nome-dir-rec + c-file) VALUE(c-nome-dir-rec + REPLACE(c-file," ","")).

                    /* --- Indica nome Novo sem Espa?os --- */

                    ASSIGN c-file = REPLACE(c-file," ","").
                END.
    
                IF SEARCH(c-nome-dir-rec + c-file) <> ? THEN 
                DO:
                    /* --- Leitura de XML do Retorno Sefaz --- */

                    ASSIGN c-chave-acesso-nfe = bfu-nfe-nota-fiscal-rec.chave-acesso-nfe.

/*                     RUN dsc\ra\esp\esnfe201b.p (INPUT c-nome-dir-rec,              /* --- Origem    --- */ */
/*                                                 INPUT c-nome-dir-rec-sefaz-dest,   /* --- Destino   --- */ */
/*                                                 INPUT c-nome-dir-erro,             /* --- Erro      --- */ */
/*                                                 INPUT c-file,                      /* --- Arquivo   --- */ */
/*                                                 INPUT-OUTPUT c-chave-acesso-nfe).  /* --- Chave NFe --- */ */


                    ASSIGN p_l_encontrou = YES. /*renato falha*/
                    /* --- Encontrou o XML de Retorno --- */

                    IF TRIM(c-chave-acesso-nfe) <> "" THEN 
                    DO:
                        ASSIGN p_l_encontrou = YES.

                        LEAVE.
                    END.
                END.
            END. /* REPEAT */
        INPUT CLOSE.
    END. /* IF AVAIL bfu-nfe-nota-fiscal-rec.... */

END PROCEDURE. /* pi_retorno_sefaz_dscrc */

/**************************************************************************************************************************/

PROCEDURE pi_valida_barra:
    
    DEF INPUT-OUTPUT PARAM p_diretorio_receb  AS CHAR FORMAT "x(256)" INITIAL "" NO-UNDO.
    
    /* --- Inverte as Barras e Retira Espa?os --- */

    ASSIGN p_diretorio_receb = REPLACE(p_diretorio_receb,"/","\")
           p_diretorio_receb = RIGHT-TRIM(LEFT-TRIM(TRIM(p_diretorio_receb,' '),' '),' ').
        
    /* --- Adiciona Barra no diret®rio se nao existir --- */

    IF SUBSTR(p_diretorio_receb,LENGTH(p_diretorio_receb) ,1) <> "\" THEN DO:
    

        ASSIGN p_diretorio_receb = p_diretorio_receb + "\".


    END.

END PROCEDURE. /* pi_valida_barra */

/**************************************************************************************************************************/

PROCEDURE pi_valida_dados :

    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes: A procedure Valida Dados e utilizada em todas as rotinas de validacao
      **     de Itens da Danfe, tanto pelas rotinas do Recebimento FISICO e FISCAL
      **     (Leitora), quanto pelo Painel de NFE na manutencao do Item, e a
      **     procedure e executada diretamente de telas, por esse motivo, deve-se
      **     informar o Numero do Campo no final da mensagem do tipo ERRO(1), pois
      **     ira posicionar no campo com Erro.
      ------------------------------------------------------------------------------*/
    
    DEF INPUT  PARAM p_rowid_item_danfe AS ROWID                            NO-UNDO.
    DEF INPUT  PARAM p_seq_item         AS INTE FORMAT ">>>>9"              NO-UNDO. /* 01 */
    DEF INPUT  PARAM p_item_fornec      AS CHAR FORMAT "x(060)"             NO-UNDO. /* 02 */
    DEF INPUT  PARAM p_it_codigo        AS CHAR FORMAT "x(016)"             NO-UNDO. /* 03 */
    DEF INPUT  PARAM p_nr_pedido        AS INTE FORMAT "->,>>>,>>9"         NO-UNDO. /* 04 */
    DEF INPUT  PARAM p_nr_ordem         AS INTE FORMAT "zzzzz999"           NO-UNDO. /* 05 */
    DEF INPUT  PARAM p_parcela_oc       AS INTE FORMAT ">>>>9"              NO-UNDO. /* 06 */
    DEF INPUT  PARAM p_fifo_oc          AS LOG                              NO-UNDO. /* 07 */
    DEF INPUT  PARAM p_qtd_fornec       AS DECI FORMAT ">>>>,>>>,>>9.9999"  NO-UNDO. /* 08 */
    DEF INPUT  PARAM p_un_forn          AS CHAR FORMAT "XX"                 NO-UNDO. /* 09 */
    DEF INPUT  PARAM p_quantidade       AS DECI FORMAT ">>>>>,>>9.9999"     NO-UNDO. /* 10 */
    DEF INPUT  PARAM p_un               AS CHAR FORMAT "XX"                 NO-UNDO. /* 11 */
    DEF INPUT  PARAM p_pr_unit          AS DECI FORMAT ">>>>,>>>,>>9.99999" NO-UNDO. /* 12 */
    DEF INPUT  PARAM p_pr_total         AS DECI FORMAT ">>>>,>>>,>>9.99"    NO-UNDO. /* 13 */
    DEF INPUT  PARAM p_cfop             AS INTE FORMAT ">>>9"               NO-UNDO. /* 14 */
    DEF INPUT  PARAM p_natur_oper       AS CHAR FORMAT "x(008)"             NO-UNDO. /* 15 */
    DEF INPUT  PARAM p_esp_nat          AS CHAR FORMAT "x(005)"             NO-UNDO. /* 16 */
    DEF INPUT  PARAM p_class_fisc       AS CHAR FORMAT "9999.99.99"         NO-UNDO. /* 17 */
    DEF INPUT  PARAM p_conta_contabil   AS CHAR FORMAT "x(017)"             NO-UNDO. /* 18 */
    DEF INPUT  PARAM p_cod_refer        AS CHAR FORMAT "x(008)"             NO-UNDO. /* 19 */
    DEF INPUT  PARAM p_cod_depos        AS CHAR FORMAT "x(003)"             NO-UNDO. /* 20 */
    DEF INPUT  PARAM p_cod_localiz      AS CHAR FORMAT "x(010)"             NO-UNDO. /* 21 */
    DEF INPUT  PARAM p_lote_serie       AS CHAR FORMAT "x(010)"             NO-UNDO. /* 22 */
    DEF INPUT  PARAM p_dt_vali_lote     AS DATE FORMAT "99/99/9999"         NO-UNDO. /* 23 */
    DEF INPUT  PARAM p_nr_ord_produ     AS INTE FORMAT ">>>,>>>,>>9"        NO-UNDO. /* 24 */
    DEF INPUT  PARAM p_nro_docto        AS CHAR FORMAT "x(016)"             NO-UNDO. /* 25 */
    DEF INPUT  PARAM p_serie_docto      AS CHAR FORMAT "x(005)"             NO-UNDO. /* 26 */
    DEF INPUT  PARAM p_seq_docto        AS INTE FORMAT ">>>>9"              NO-UNDO. /* 27 */
    DEF OUTPUT PARAM p_mensagem_erro    AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

     DEFINE VARIABLE de-var-val-re-maior AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE de-var-qtd-re       AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE de-var-max-perc     AS DECIMAL  FORMAT ">>>,>>>,>>9.999999999"   NO-UNDO.
     DEFINE VARIABLE de-valor-orig       AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE de-valor-min        AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE de-var-max-lim      AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE de-lim-var-valor    AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE de-vl-merc-liq-aux  AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE de-lim-var-qtd      AS DECIMAL     NO-UNDO.

    /* --- (I-TIPO-NFE) 1 - Compra / 2 - Devol Cli / 3 - Retorno Terceiro / 4 - Remessa Terceiro --- */

    DEFINE VARIABLE c-chave-mla AS CHARACTER FORMAT "x(40)"  NO-UNDO.

    /* --- Busca Registro de Item --- */

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item_danfe NO-LOCK NO-ERROR.

    /* --- Busca Registro de NFE --- */

    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               bfu-nfe-nota-fiscal-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso NO-LOCK NO-ERROR.

    /* --- Busca Item para validacao de item, conta cont?bil, refer?ncia e outros --- */

    FIND FIRST ITEM WHERE 
               ITEM.it-codigo = p_it_codigo NO-LOCK NO-ERROR.

    FIND emitente WHERE 
         emitente.nome-abrev = bfu-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

    /* --- Include Validacoes --- */
    {dsc\ra\esp\bonfe001.i}         
    /*variacao*/

     FIND FIRST param-re NO-LOCK
        WHERE param-re.usuario = c-seg-usuario NO-ERROR.

    
    IF param-re.aceita-var = NO then do:

        {rep/re1001.i10 bfu-nfe-nota-fiscal-rec.cod-estabel}
    
        FIND FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = p_nr_ordem NO-ERROR.
    
        IF AVAIL ordem-compra THEN DO:                         
            ASSIGN de-valor-orig      = ordem-compra.preco-unit /*- ordem-compra.valor-descto*/
                   de-vl-merc-liq-aux = p_pr_unit.
            
            /* 22/07/14 - Rontan*/
            IF ordem-compra.aliquota-ipi <> 0 then
               de-valor-orig = ((ordem-compra.preco-unit * 100) / (100 + ordem-compra.aliquota-ipi)).
          
    
            IF param-re.aceita-var then do:
                
                assign de-var-max-perc = round(de-valor-orig +
                                              (de-valor-orig * de-var-val-re-maior / 100), 2)
                       de-valor-min    = round(de-valor-orig -
                                              (de-valor-orig * de-var-val-re-maior / 100), 2)
                       de-var-max-lim  = de-valor-orig + de-lim-var-valor. /*verificar parametro de valor*/
        
            END. 
            ELSE 
                assign de-var-max-perc = de-valor-orig
                       de-var-max-lim  = de-valor-orig
                       de-valor-min    = de-valor-orig.
        
            assign l-erro = no.
                
            IF de-var-val-re-maior = 0 AND
               de-lim-var-valor    = 0 then do:
        
                if  round(de-vl-merc-liq-aux,2) <> de-var-max-perc THEN DO:

                    IF  (round(de-vl-merc-liq-aux,2) - de-var-max-perc) > 0.01 OR
                        (round(de-vl-merc-liq-aux,2) - de-var-max-perc) < - 0.01 THEN
                        assign l-erro = yes.
                    
                END.

                if  de-vl-merc-liq-aux > de-var-max-lim THEN DO:

                    IF (de-vl-merc-liq-aux - de-var-max-lim) > 0.01 THEN
                        assign l-erro = yes.
                    
                END.
            END.



            ELSE DO:
        
                IF de-vl-merc-liq-aux <= de-var-max-perc or  
                   de-var-val-re-maior = 0               THEN DO:
                
                    IF  de-vl-merc-liq-aux > de-var-max-lim and 
                        de-lim-var-valor   > 0              THEN DO:
                             
                        assign l-erro = yes.
                    END.
                    ELSE 
                        assign l-erro = no.
                END.
                ELSE DO:
                    
                    assign l-erro = yes.
                    
                END.
            END.
        
            IF l-erro = YES THEN DO:

                FIND FIRST nfe-it-param-rec NO-LOCK
                    WHERE  nfe-it-param-rec.cod-parametro      = "param_global"
                      AND  nfe-it-param-rec.cod-item-parametro = "valida_variacao" NO-ERROR.
                    
                IF NOT AVAIL nfe-it-param-rec
                    OR nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN DO: /*se n’o houver o parametro ou estiver como SIM (Valida)*/
                    
                    ASSIGN p_mensagem_erro = "1,"     +
                                             "Valor do Item " + p_it_codigo + " Fora da Variacao Permitida"   + "," +
                                             "Os Valores enviados no XML estao fora da Variacao Permitida conforme regras do ERP.".
                    LEAVE.
                END.
            END. 
        END.
    END.
   /*FIm Variacao*/

    /*Recebe de outro fornec*/

    IF param-re.rec-out-for = NO THEN DO:

        FIND FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = p_nr_ordem NO-ERROR.
    
        IF AVAIL ordem-compra THEN DO:

           IF ordem-compra.cod-emitente <> emitente.cod-emitente THEN DO:

               ASSIGN p_mensagem_erro = "1,"     +
                                         "Ordem de Compra DO ITEM " + p_it_codigo + " Nao pertence ao Fornecedor da Nota fiscal"   + "," +
                                         "Parametrizacao DO ERP nao permite que o usuario receba ordens para notas fiscais de fornecedores distintos. .".


           END.
        END.   
    END.       

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "ValidaDados"
           tt-epc.cod-parameter = "bf-nfe-it-nota-fisc-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(bf-nfe-it-nota-fisc-rec)).

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "ValidaDados"
           tt-epc.cod-parameter = "dados"
           tt-epc.val-parameter = STRING(STRING(p_it_codigo)      + ";" +
                                         STRING(p_nr_pedido)      + ";" +
                                         STRING(p_nr_ordem)       + ";" +
                                         STRING(p_parcela_oc)     + ";" +
                                         STRING(p_fifo_oc)        + ";" +
                                         STRING(p_qtd_fornec)     + ";" +
                                         STRING(p_un_forn)        + ";" +
                                         STRING(p_quantidade)     + ";" +
                                         STRING(p_un)             + ";" +
                                         STRING(p_pr_unit)        + ";" +
                                         STRING(p_pr_total)       + ";" +
                                         STRING(p_cfop)           + ";" +
                                         STRING(p_natur_oper)     + ";" +
                                         STRING(p_esp_nat)        + ";" +
                                         STRING(p_class_fisc)     + ";" +
                                         STRING(p_conta_contabil) + ";" +
                                         STRING(p_cod_refer)      + ";" +
                                         STRING(p_cod_depos)      + ";" +
                                         STRING(p_cod_localiz)    + ";" +
                                         STRING(p_lote_serie)).

    ASSIGN tt-epc.val-parameter = (IF p_dt_vali_lote = ? THEN (tt-epc.val-parameter + ";" + "")
                                                         ELSE (tt-epc.val-parameter + ";" + STRING(p_dt_vali_lote))).

    ASSIGN tt-epc.val-parameter = (tt-epc.val-parameter           + ";" +
                                         STRING(p_nr_ord_produ)   + ";" +
                                         STRING(p_nro_docto)      + ";" +
                                         STRING(p_serie_docto)    + ";" +
                                         STRING(p_seq_docto)).

    {include\i-epc201.i "ValidaDados"}

        

    /* --- Verifica Erro de Validacao --- */

    IF RETURN-VALUE <> ""    AND
       RETURN-VALUE <> "OK"  AND
       RETURN-VALUE <> "YES" AND
       RETURN-VALUE <> "NO"  AND
       RETURN-VALUE <> "NOK" THEN ASSIGN p_mensagem_erro = RETURN-VALUE.
   
END PROCEDURE. /* pi_valida_dados */

/**************************************************************************************************************************/
PROCEDURE pi_valida_diretorios:

    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL "" NO-UNDO.

    /* --- Verifica Parametros Globais --- */

    FIND FIRST nfe-param-rec WHERE 
               nfe-param-rec.cod-parametro = "diretorios_recebimento_nfe" NO-LOCK NO-ERROR.

    IF NOT AVAIL nfe-param-rec THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                                                           +
                                 "Parametros Globais do Recebimento NFE nao encontrado !,"      +
                                 "Registro de Parametros Globais (diretorios_recebimento_nfe) " + 
                                 "precisa ser cadastrado corretamente em Parametros do Recebimento NFE (esnfe400).".
        LEAVE.
    END.

    /* --- Verifica Diret®rios do Recebimento --- */

    FIND FIRST nfe-it-param-rec WHERE 
               nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
               nfe-it-param-rec.cod-item-parametro = "diretorios"                NO-LOCK NO-ERROR.

    IF NOT AVAIL nfe-it-param-rec THEN 
    DO:
        ASSIGN p_mensagem_erro = "1,"                                                                               +
                                 "Parametros Globais do Recebimento NFE nao encontrado !,"                          +
                                 "Registro de Parametros Globais (diretorios) precisa ser cadastrado corretamente " + 
                                 "em Parametros do Recebimento NFE (esnfe400).".
        LEAVE.
    END.

END PROCEDURE. /* pi_valida_diretorios */

/**************************************************************************************************************************/

PROCEDURE pi_valida_entrada:

    DEF INPUT  PARAM p_rowid_danfe    AS ROWID                            NO-UNDO.
    DEF INPUT  PARAM p_rs_recebimento AS INTEGER                          NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro  AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    DEF VAR l-erro  AS LOG INIT NO   NO-UNDO.
    DEF VAR l-canc  AS LOG           NO-UNDO.
    
    FIND FIRST nfe-nota-fiscal-rec WHERE 
               ROWID(nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    FOR EACH nfe-it-nota-fisc-rec WHERE 
             nfe-it-nota-fisc-rec.chave-acesso-nfe  = nfe-nota-fiscal-rec.chave-acesso-nfe NO-LOCK:

        /* --- Retona o Tipo de Nota de acordo com a Natureza --- */

        RUN pi_valida_tipo_nota (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                 OUTPUT i-tipo-nfe,
                                 OUTPUT i-tp-oper-terc).
        
        IF i-tipo-nfe = 0 THEN
        DO:
            ASSIGN p_mensagem_erro = "1,"                                     +
                                     "Natureza de operacao nao encontrada !," +
                                     "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_VALIDA_ENTRADA)".

            RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                  OUTPUT i-acao).
        END.

        /* --- Validacao de Itens da Nota Fiscal Eletronica --- */

        RUN pi_valida_dados (INPUT  ROWID(nfe-it-nota-fisc-rec),
                             INPUT  nfe-it-nota-fisc-rec.seq-item,
                             INPUT  nfe-it-nota-fisc-rec.item-cprod,
                             INPUT  nfe-it-nota-fisc-rec.it-codigo,
                             INPUT  nfe-it-nota-fisc-rec.item-xPed,
                             INPUT  nfe-it-nota-fisc-rec.item-num-ordem,
                             INPUT  nfe-it-nota-fisc-rec.item-parcela-oc,
                             INPUT  nfe-it-nota-fisc-rec.item-fifo-oc,
                             INPUT  nfe-it-nota-fisc-rec.item-qCom,
                             INPUT  nfe-it-nota-fisc-rec.item-uCom,
                             INPUT  nfe-it-nota-fisc-rec.item-qtde,
                             INPUT  nfe-it-nota-fisc-rec.item-un,
                             INPUT  nfe-it-nota-fisc-rec.item-vUnCom,
                             INPUT  nfe-it-nota-fisc-rec.item-vProd,
                             INPUT  nfe-it-nota-fisc-rec.item-cfop,
                             INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                             INPUT  nfe-it-nota-fisc-rec.item-esp-nat,
                             INPUT  nfe-it-nota-fisc-rec.item-ncm,
                             INPUT  (&IF '{&bf_dis_versao_ems}' <= '2.07' &THEN nfe-it-nota-fisc-rec.item-conta-contabil
                                                                          &ELSE nfe-it-nota-fisc-rec.ct-codigo
                                                                          &ENDIF),
                             INPUT  nfe-it-nota-fisc-rec.item-cod-refer,
                             INPUT  nfe-it-nota-fisc-rec.item-cod-depos,
                             INPUT  nfe-it-nota-fisc-rec.item-cod-localiz,
                             INPUT  nfe-it-nota-fisc-rec.item-ser-lote,
                             INPUT  nfe-it-nota-fisc-rec.item-dt-vali-lote,
                             INPUT  nfe-it-nota-fisc-rec.item-nr-ord-produ,
                             INPUT  (IF i-tipo-nfe = 2 THEN nfe-it-nota-fisc-rec.item-nr-nota-fis    /* --- Devolucao de Clientes    --- */
                                                       ELSE nfe-it-nota-fisc-rec.item-nro-docto),    /* --- Retorno Terceiros        --- */
                             INPUT  (IF i-tipo-nfe = 2 THEN nfe-it-nota-fisc-rec.item-serie-nf       /* --- Devolucao de Clientes    --- */
                                                       ELSE nfe-it-nota-fisc-rec.item-serie-docto),  /* --- Retorno Terceiros        --- */    
                             INPUT  nfe-it-nota-fisc-rec.item-seq-comp,
                             OUTPUT p_mensagem_erro).
        /* --- Verifica ERRO --- */

        IF p_mensagem_erro <> "" THEN 
        DO:
            ASSIGN l-erro = YES.

            LEAVE.
        END.
    END.

    
    IF l-erro = YES THEN 
    DO:

        
        RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                              OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.

        /* --- Executa tela de Itens da Nota por Natureza --- */

        RUN dsc\ra\esp\esnfe200d.w (INPUT  p_rowid_danfe,
                                    INPUT  p_rs_recebimento,
                                    OUTPUT l-canc).
        
        IF l-canc THEN DO:
        
            RETURN NO-APPLY.
        END.
         ELSE 
             ASSIGN p_mensagem_erro = "".
    END.

END PROCEDURE. /* pi_valida_entrada */

/**************************************************************************************************************************/

PROCEDURE pi_valida_relacionamentos:

    DEF INPUT  PARAM p_rowid_danfe   AS ROWID NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR  NO-UNDO.

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "RelacEmitCfopNat"
           tt-epc.cod-parameter = "p_rowid_danfe"
           tt-epc.val-parameter = STRING(p_rowid_danfe).

    {include\i-epc201.i "RelacEmitCfopNat"}

    /* --- Se nao efetivar entrada via EPC retorna NOK para executar rotina Padrao --- */

    IF c-executa-bonfe001 = "PADRAO" THEN 
    DO:
        /* --- Atualiza Relacionamento Emitente x CFOP x Natureza --- */

        RUN pi_gera_relac_emit_cfop_nat (INPUT  p_rowid_danfe,
                                         OUTPUT p_mensagem_erro).
    END.
    ELSE 
    DO:
        /* --- Verifica Erro de Validacao --- */

        IF RETURN-VALUE <> ""   AND
           RETURN-VALUE <> "OK" THEN 
            ASSIGN p_mensagem_erro = RETURN-VALUE.
    END.

    /* --- Verifica Parametros Globais --- */

    FIND FIRST nfe-param-rec WHERE 
               nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

    IF AVAIL nfe-param-rec THEN 
    DO:
        /* --- Verifica Permissao de Gera Item x Fornecedor --- */

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "gera_item_fornec"          NO-LOCK NO-ERROR.

        IF AVAIL nfe-it-param-rec                           AND
           nfe-it-param-rec.valor-1-item-parametro = "SIM"  THEN 
        DO:
            RUN pi_limpa_epc.

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "ItemFornecedor"
                   tt-epc.cod-parameter = "p_rowid_danfe"
                   tt-epc.val-parameter = STRING(p_rowid_danfe).

            {include\i-epc201.i "ItemFornecedor"}

            /* --- Se nao efetivar entrada via EPC retorna NOK para executar rotina Padrao --- */

            IF c-executa-bonfe001 = "PADRAO" THEN 
            DO:
                /* --- Gera Relacionamento Item x Fornecedor --- */

                RUN pi_gera_relac_item_fornec (INPUT  p_rowid_danfe,
                                               OUTPUT p_mensagem_erro).
            END.
            ELSE 
            DO:
                /* --- Verifica Erro de Validacao --- */

                IF RETURN-VALUE <> ""   AND
                   RETURN-VALUE <> "OK" THEN 
                    ASSIGN p_mensagem_erro = RETURN-VALUE.
            END.
        END.
    END.

END PROCEDURE. /* pi_valida_relacionamentos */
    
/**************************************************************************************************************************/

PROCEDURE pi_valida_retorno_sefaz:
    
    DEF INPUT  PARAM p_rowid_danfe   AS ROWID                            NO-UNDO.
    DEF OUTPUT PARAM p_l_encontrou   AS LOG                              NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    RUN pi_limpa_epc.

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "RetornoSefaz"
           tt-epc.cod-parameter = "p_rowid_danfe(ROWID)"
           tt-epc.val-parameter = STRING(p_rowid_danfe).

    {include\i-epc201.i "RetornoSefaz"}
            
    /* --- Se Nao Verificar Retorno Sefaz via EPC executa Rotina Padrao de Mensageria DSCRC --- */

    IF c-executa-bonfe001 = "PADRAO" THEN 
    DO:
        /* --- Parametros do Sistema --- */

        FIND FIRST nfe-param-rec WHERE 
                   nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

        FIND FIRST nfe-it-param-rec WHERE 
                   nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                   nfe-it-param-rec.cod-item-parametro = "mensageria"                NO-LOCK NO-ERROR.

        IF AVAIL nfe-it-param-rec THEN 
        DO:
            /* --- Retorno Sefaz Modelo DSCRC --- */

            IF nfe-it-param-rec.valor-1-item-parametro = "DSCRC" THEN 
            DO:
                RUN pi_retorno_sefaz_dscrc (INPUT  p_rowid_danfe,
                                            OUTPUT p_l_encontrou,
                                            OUTPUT p_mensagem_erro).

                IF TRIM(p_mensagem_erro) <> "" AND NOT l_permite_usu THEN LEAVE.
            END.
            ELSE 
            DO:
                /* --- Retorno Sefaz Modelo PADRAO --- */
            END.
        END.
    END.
    ELSE 
    DO:
        /* --- Verifica Erro de Validacao --- */

        IF RETURN-VALUE <> ""   AND
           RETURN-VALUE <> "OK" AND
           NOT l_permite_usu THEN 
            ASSIGN p_mensagem_erro = RETURN-VALUE.
    END.
        
END PROCEDURE. /* pi_valida_retorno_sefaz */

/**************************************************************************************************************************/

PROCEDURE pi_valida_tipo_nota:



    DEF INPUT   PARAM p_nat_operacao AS CHAR FORMAT "x(06)" NO-UNDO.
    DEF OUTPUT  PARAM p_tipo_nfe     AS INTE                NO-UNDO.
    DEF OUTPUT  PARAM p_tp_oper_terc AS INTE                NO-UNDO.

    /* Inicializa as vari?veis de retorno caso nao encontre a natureza */

    ASSIGN p_tipo_nfe     = 1 /* NFE de Compra */
           p_tp_oper_terc = 0.

    FIND FIRST natur-oper WHERE 
               natur-oper.nat-operacao = p_nat_operacao NO-LOCK NO-ERROR.

    IF AVAIL natur-oper THEN DO:
        /* --- 1 - NFE de COMPRA --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                        --- */
           natur-oper.tipo-compra  <> 3     AND  /* --- Devolucao Cliente              --- */
           natur-oper.terceiros     = NO    AND  /* --- Operacao com Terceiros         --- */
           natur-oper.transf        = NO    THEN /* --- Transferencia entre Estab      --- */
            ASSIGN p_tipo_nfe       = 1
                   p_tp_oper_terc   = 0.
        
        /* --- 2 - NFE de DEVOLUCAO CLIENTE --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                        --- */
           natur-oper.tipo-compra   = 3     AND  /* --- Devolucao Cliente              --- */
           natur-oper.terceiros     = NO    AND  /* --- Operacao com Terceiros         --- */
           natur-oper.transf        = NO    THEN /* --- Transferencia entre Estab      --- */
            ASSIGN p_tipo_nfe       = 2
                   p_tp_oper_terc   = 0.
        
        /* --- 3 - NFE de RETORNO TERCEIROS --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.transf        = NO    AND  /* --- Transferencia entre Estab       --- */
           natur-oper.terceiros     = YES   AND  /* --- Operacao com Terceiros          --- */
          (natur-oper.tp-oper-terc  = 2     OR   /* --- Retorno Beneficiamento          --- */
           natur-oper.tp-oper-terc  = 4     OR   /* --- Faturamento Consignacao         --- */
           natur-oper.tp-oper-terc  = 5)    THEN /* --- Retorno / Devolucao Consignacao --- */
            ASSIGN p_tipo_nfe       = 3
                   p_tp_oper_terc   = natur-oper.tp-oper-terc.

        /* --- 4 - NFE de REMESSA TERCEIROS --- */


        IF natur-oper.tipo          = 1     AND  /* --- Entrada                     --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                      --- */
           natur-oper.transf        = NO    AND  /* --- Transferencia entre Estab   --- */
           natur-oper.terceiros     = YES   AND  /* --- Operacao com Terceiros      --- */
          (natur-oper.tp-oper-terc  = 1     OR   /* --- Remessa Beneficiamento      --- */
           natur-oper.tp-oper-terc  = 3)    THEN /* --- Remessa Consignacao         --- */
            ASSIGN p_tipo_nfe       = 4
                   p_tp_oper_terc   = natur-oper.tp-oper-terc.

        /* --- 5 - NFE de TRANSFERENCIA --- */

        IF natur-oper.transf        = YES   THEN 
            ASSIGN p_tipo_nfe       = 5
                   p_tp_oper_terc   = 0.

         /* --- 6 - NFE de ENTRADA PARA CONSIGNA?›O --- 30-06-14 - Atender chamado 6531 - Rontan*/

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.transf        = NO    AND  /* --- Transfer?ncia entre Estab       --- */
           natur-oper.terceiros     = YES   AND  /* --- Opera?ão com Terceiros          --- */
           natur-oper.tp-oper-terc  = 3     THEN /* --- Remessa de consigna?ão          --- */
        DO:
            ASSIGN p_tipo_nfe       = 6
                   p_tp_oper_terc   = natur-oper.tp-oper-terc.
        END. 

    END. /* IF AVAIL natur-oper..... */

    

END PROCEDURE. /* pi_valida_tipo_nota */

/**************************************************************************************************************************/

PROCEDURE pi_valida_tipo_nota_fisico:

    DEF INPUT  PARAM p_nat_operacao AS CHAR FORMAT "x(06)" NO-UNDO.
    DEF OUTPUT PARAM p_tipo_nfe     AS INTE                NO-UNDO.

    /* F?bio Guedes - 08/11/2011 - Versao : 1.04.024 */
    
    ASSIGN p_tipo_nfe = 1.  /* Inicializa com 1 (Compra) sob orientacao do Elias */

    FIND FIRST natur-oper WHERE 
               natur-oper.nat-operacao = p_nat_operacao NO-LOCK NO-ERROR.

    IF AVAIL natur-oper THEN DO:
        /* --- 1 - NFE de COMPRA --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.terceiros     = NO    AND  /* --- Operacao com Terceiros          --- */
           natur-oper.transf        = NO    AND  /* --- Transfer?ncia entre Estab       --- */
           natur-oper.especie-doc  <> "NFD" THEN /* --- NF de devolucao                 --- */ 
        DO:
            ASSIGN p_tipo_nfe       = 1.
        END.
        
        /* --- 2 - NFE de DEVOLU??O CLIENTE --- */

        IF natur-oper.especie-doc   = "NFD" AND  /* --- NF de devolucao                 --- */ 
           natur-oper.transf        = NO    THEN /* --- Transfer?ncia entre Estab       --- */
        DO:
            ASSIGN p_tipo_nfe       = 2.
        END.

        /* --- 3 - NFE de TRANSFER’NCIA --- */

        IF natur-oper.especie-doc   = "NFT" AND  /* --- NF de transfer?ncia             --- */ 
           natur-oper.transf        = YES   THEN /* --- Transfer?ncia entre Estab       --- */
        DO:
            ASSIGN p_tipo_nfe       = 3.
        END.

        /* --- 4 - NFE de ENTRADA PARA BENEFICIAMENTO --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.transf        = NO    AND  /* --- Transfer?ncia entre Estab       --- */
           natur-oper.terceiros     = YES   AND  /* --- Operacao com Terceiros          --- */
           natur-oper.tp-oper-terc  = 1     THEN /* --- Remessa de beneficiamento       --- */
        DO:
            ASSIGN p_tipo_nfe       = 4.
        END.

        /* --- 5 - NFE de RETORNO DE BENEFICIAMENTO --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.transf        = NO    AND  /* --- Transfer?ncia entre Estab       --- */
           natur-oper.terceiros     = YES   AND  /* --- Operacao com Terceiros          --- */
           natur-oper.tp-oper-terc  = 2     THEN /* --- Retorno de beneficiamento       --- */
        DO:
            ASSIGN p_tipo_nfe       = 5.
        END.

        /* --- 6 - NFE de ENTRADA PARA CONSIGNA??O --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.transf        = NO    AND  /* --- Transfer?ncia entre Estab       --- */
           natur-oper.terceiros     = YES   AND  /* --- Operacao com Terceiros          --- */
           natur-oper.tp-oper-terc  = 3     THEN /* --- Remessa de consignacao          --- */
        DO:
            ASSIGN p_tipo_nfe       = 6.
        END.

        /* --- 7 - NFE de FATURAMENTO DE CONSIGNA??O --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.transf        = NO    AND  /* --- Transfer?ncia entre Estab       --- */
           natur-oper.terceiros     = YES   AND  /* --- Operacao com Terceiros          --- */
           natur-oper.tp-oper-terc  = 4     THEN /* --- Faturamento de Consignacao      --- */
        DO:
            ASSIGN p_tipo_nfe       = 7.
        END.

        /* --- 8 - NFE de DEVOLU??O DE CONSIGNA??O --- */

        IF natur-oper.tipo          = 1     AND  /* --- Entrada                         --- */
           natur-oper.tipo-compra   = 1     AND  /* --- Normal                          --- */
           natur-oper.transf        = NO    AND  /* --- Transfer?ncia entre Estab       --- */
           natur-oper.terceiros     = YES   AND  /* --- Operacao com Terceiros          --- */
           natur-oper.tp-oper-terc  = 5     THEN /* --- Devolucao Consignacao           --- */
        DO:
            ASSIGN p_tipo_nfe       = 8.
        END.

        /* --- 9 - NFE de RATEIO --- */

        IF natur-oper.terceiros     = NO    AND  /* --- Operacao com Terceiros          --- */
           natur-oper.tipo-compra   = 2     AND  /* --- Frete                           --- */
           natur-oper.especie-doc  <> "NFD" THEN /* --- NF de devolucao                 --- */
        DO:
            ASSIGN p_tipo_nfe       = 9.
        END.

    END. /* IF AVAIL natur-oper.... */

END PROCEDURE. /* pi_valida_tipo_nota_fisico */

/**************************************************************************************************************************/

PROCEDURE pi_exclui_registro:

    DEF INPUT PARAM p_rowid_danfe AS ROWID NO-UNDO.
    /*Verificar aqui Renato lock tabela*/
    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_danfe EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL bfu-nfe-nota-fiscal-rec THEN 
    DO:
        FOR EACH bf-nfe-it-nota-fisc-rec OF bfu-nfe-nota-fiscal-rec EXCLUSIVE-LOCK :
            DELETE bf-nfe-it-nota-fisc-rec.
        END.

        FOR EACH nfe-his-it-nota-fis-rec OF bfu-nfe-nota-fiscal-rec EXCLUSIVE-LOCK :
            DELETE nfe-his-it-nota-fis-rec.
        END.

        FOR EACH bf-nfe-his-nota-fis-rec OF bfu-nfe-nota-fiscal-rec EXCLUSIVE-LOCK :
            DELETE bf-nfe-his-nota-fis-rec.
        END.
            
        FOR EACH nfe-relac-ordem-rec OF bfu-nfe-nota-fiscal-rec EXCLUSIVE-LOCK :
            DELETE nfe-relac-ordem-rec.
        END.
            
        FOR EACH nfe-relac-ser-lote-rec OF bfu-nfe-nota-fiscal-rec EXCLUSIVE-LOCK :
            DELETE nfe-relac-ser-lote-rec.
        END.
            
        FOR EACH nfe-relac-doc-nota-rec OF bfu-nfe-nota-fiscal-rec EXCLUSIVE-LOCK :
            DELETE nfe-relac-doc-nota-rec.
        END.

        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "ExcluiRegistro"
               tt-epc.cod-parameter = "p_rowid_danfe(ROWID)"
               tt-epc.val-parameter = STRING(p_rowid_danfe).

        {include\i-epc201.i "ExcluiRegistro"}

        DELETE bfu-nfe-nota-fiscal-rec.
    END.
    ELSE DO:
        FIND FIRST nfe-cte-inf WHERE 
               ROWID(nfe-cte-inf) = p_rowid_danfe EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL nfe-cte-inf THEN DO:

            FOR EACH nfe-cte-inf-nf  EXCLUSIVE-LOCK
                WHERE  nfe-cte-inf-nf.chave-acesso =  nfe-cte-inf.chave-acesso:
                DELETE  nfe-cte-inf-nf.
            END.  
            FOR EACH nfe-cte-inf-nfe  EXCLUSIVE-LOCK
                WHERE  nfe-cte-inf-nfe.chave-acesso =  nfe-cte-inf.chave-acesso:
                DELETE  nfe-cte-inf-nfe.
            END. 
            FOR EACH nfe-cte-inf-outros  EXCLUSIVE-LOCK
                WHERE  nfe-cte-inf-outros.chave-acesso =  nfe-cte-inf.chave-acesso:
                DELETE  nfe-cte-inf-outros.
            END. 
                                                
            RUN pi_limpa_epc.

        CREATE tt-epc.
            ASSIGN tt-epc.cod-event     = "ExcluiRegistroCTE"
                   tt-epc.cod-parameter = "p_rowid_danfe(ROWID)"
                   tt-epc.val-parameter = STRING(p_rowid_danfe).
                  
                  {include\i-epc201.i "ExcluiRegistro"}
                      
            DELETE nfe-cte-inf.


        END.                   
    END.
END PROCEDURE. /* pi_exclui_registro */

/**************************************************************************************************************************/

PROCEDURE pi_atualiza_status :

    DEF INPUT PARAM p_rowid_danfe  AS ROWID NO-UNDO.
    DEF INPUT PARAM p_cod_sefaz    AS INTE  NO-UNDO.
    DEF INPUT PARAM p_cod_erp    AS INTE  NO-UNDO.
                                     /*renato-lock*/
    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_danfe NO-LOCK NO-ERROR.

    IF AVAIL bfu-nfe-nota-fiscal-rec THEN DO: 
        FIND FIRST nfe-dfe EXCLUSIVE-LOCK
            WHERE nfe-dfe.chave-acesso = bfu-nfe-nota-fiscal-rec.chave-acesso-nfe NO-ERROR.

        IF p_cod_sefaz <> 0 THEN
            ASSIGN nfe-dfe.sit-sefaz = p_cod_sefaz.

        IF p_cod_erp <> 0 THEN
            ASSIGN nfe-dfe.sit-erp = p_cod_erp.
    
    END.

    

END PROCEDURE. /* pi_atualiza_status */

/**************************************************************************************************************************/

PROCEDURE pi_relaciona_ordem :

    DEF INPUT   PARAM p_rowid_item          AS ROWID                                NO-UNDO.
    DEF INPUT   PARAM p_it_codigo           AS CHAR FORMAT "x(016)"                 NO-UNDO.
    DEF INPUT   PARAM p_un                  AS CHAR FORMAT "XX"                     NO-UNDO.
    DEF INPUT   PARAM p_quantidade          AS DECI FORMAT ">>>>>,>>9.9999"         NO-UNDO.
    DEF INPUT   PARAM p_natur_oper          AS CHAR FORMAT "x(008)"                 NO-UNDO.
    DEF INPUT   PARAM p_pr_total            AS DECI FORMAT ">>>>,>>>,>>9.99"        NO-UNDO.
    DEF INPUT   PARAM p_num_pedido          AS INTE FORMAT ">>>,>>>,>>9"            NO-UNDO.
    DEF INPUT   PARAM p_num_ordem           AS INTE FORMAT "zzzzz999"               NO-UNDO.
    DEF INPUT   PARAM p_parcela_oc          AS INTE FORMAT ">>>>9"                  NO-UNDO.
    DEF OUTPUT  PARAM p_mensagem_erro       AS CHAR FORMAT "x(200)" INITIAL ""      NO-UNDO.

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-LOCK NO-ERROR.

    IF AVAIL bf-nfe-it-nota-fisc-rec THEN 
    DO:
        /* --- Valida Quantidade --- */

        IF p_quantidade = 0 THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                       +
                                     "Quantidade invalida !,"   +
                                     "Preencha a Quantidade corretamente.".

            LEAVE.
        END.

        /* --- Verifica Natureza de Operacao --- */

        IF NOT CAN-FIND(FIRST natur-oper WHERE 
                              natur-oper.nat-operacao = p_natur_oper NO-LOCK) THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                               +
                                     "Natureza de Operacao invalida !," +
                                     "Preencha a Natureza de Operacao corretamente.".

            LEAVE.
        END.

        /* --- Retona o Tipo de Nota de acordo com a Natureza --- */

        RUN pi_valida_tipo_nota (INPUT  p_natur_oper,
                                 OUTPUT i-tipo-nfe,
                                 OUTPUT i-tp-oper-terc).

        IF i-tipo-nfe = 0 THEN
        DO:
            ASSIGN p_mensagem_erro = "1,"                                     +
                                     "Natureza de operacao nao encontrada !," +
                                     "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_RELACIONA_ORDEM)".

            RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                  OUTPUT i-acao).
        END.

        IF (i-tipo-nfe      <> 1  AND       /* --- 1 - NFE de COMPRA            --- */
            i-tipo-nfe      <> 4  AND       /* --- 4 - NFE de REMESSA TERCEIROS --- */
            i-tipo-nfe      <> 6) OR        
          ((i-tipo-nfe       = 4 OR         /* --- 4 - NFE de REMESSA TERCEIROS --- */
            i-tipo-nfe       = 6)  AND       
            i-tp-oper-terc  <> 3) THEN      /* --- Remessa Consignacao          --- */
        DO:
          
            ASSIGN p_mensagem_erro = "1,"                                            +
                                     "Nao ì possÐvel Relacionar Ordens de Compra !," +
                                     "Somente permitido relacionar Ordens de Compra para Notas de Compra ou Remessa Consignaîao.".

            LEAVE.
        END.

        /* --- Verifica se tem Relacionamento de Docto/Nota --- */

        IF CAN-FIND(FIRST tt-nfe-relac-doc-nota-rec WHERE 
                          tt-nfe-relac-doc-nota-rec.chave-acesso   = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                          tt-nfe-relac-doc-nota-rec.seq-item       = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK) THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                                               +
                                     "Existem Documentos e Notas Fiscais Referenciadas Relacionadas !," +
                                     "Nao eh possivel relacionar Ordens de Compra para o Item com relacionamento de Documentos ou Notas Referenciadadas.".

            LEAVE.
        END.

        /* --- Carrega Ordens ja relacionadas --- */

        EMPTY TEMP-TABLE tt-nfe-relac-ordem-rec.

        FOR EACH nfe-relac-ordem-rec WHERE 
                 nfe-relac-ordem-rec.chave-acesso   = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                 nfe-relac-ordem-rec.seq-item       = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK:
            CREATE tt-nfe-relac-ordem-rec.

            BUFFER-COPY nfe-relac-ordem-rec TO tt-nfe-relac-ordem-rec.
        END.

        /* --- Executa tela de Relacionar Ordens de Compra --- */

        RUN dsc\ra\esp\esnfe200e01.w (INPUT p_rowid_item,
                                      INPUT p_it_codigo,
                                      INPUT p_un,
                                      INPUT p_quantidade,
                                      INPUT p_pr_total,
                                      INPUT-OUTPUT TABLE tt-nfe-relac-ordem-rec,
                                      OUTPUT l-aux).

        /* --- Confirmado pelo botao de OK --- */

        IF l-aux = YES THEN 
        DO:
            /* --- Deleta registros --- */

            FOR EACH tt-nfe-relac-ordem-rec WHERE 
                     tt-nfe-relac-ordem-rec.l-retirar = YES:
                FIND FIRST nfe-relac-ordem-rec WHERE 
                           nfe-relac-ordem-rec.chave-acesso   = tt-nfe-relac-ordem-rec.chave-acesso AND 
                           nfe-relac-ordem-rec.seq-item       = tt-nfe-relac-ordem-rec.seq-item     AND 
                           nfe-relac-ordem-rec.num-pedido     = tt-nfe-relac-ordem-rec.num-pedido   AND 
                           nfe-relac-ordem-rec.numero-ordem   = tt-nfe-relac-ordem-rec.numero-ordem AND 
                           nfe-relac-ordem-rec.parcela-oc     = tt-nfe-relac-ordem-rec.parcela-oc   EXCLUSIVE-LOCK NO-ERROR.

                IF AVAIL nfe-relac-ordem-rec THEN 
                DO:
                    DELETE nfe-relac-ordem-rec.

                    RELEASE nfe-relac-ordem-rec.
                END.

                DELETE tt-nfe-relac-ordem-rec.

                RELEASE tt-nfe-relac-ordem-rec.
            END.
    
            /* --- Gera tabela de Controle --- */

            FOR EACH tt-nfe-relac-ordem-rec NO-LOCK :
                /* --- Se existe Relacionamento de Ordem retira FIFO --- */

                FIND FIRST bf2-nfe-it-nota-fisc-rec WHERE 
                           ROWID(bf2-nfe-it-nota-fisc-rec)       = p_rowid_item AND 
                           bf2-nfe-it-nota-fisc-rec.item-fifo-oc = YES          EXCLUSIVE-LOCK NO-ERROR.

                IF AVAIL bf2-nfe-it-nota-fisc-rec THEN 
                    ASSIGN bf2-nfe-it-nota-fisc-rec.item-fifo-oc = NO.
                
                FIND FIRST nfe-relac-ordem-rec WHERE 
                           nfe-relac-ordem-rec.chave-acesso   = tt-nfe-relac-ordem-rec.chave-acesso AND 
                           nfe-relac-ordem-rec.seq-item       = tt-nfe-relac-ordem-rec.seq-item     AND 
                           nfe-relac-ordem-rec.num-pedido     = tt-nfe-relac-ordem-rec.num-pedido   AND 
                           nfe-relac-ordem-rec.numero-ordem   = tt-nfe-relac-ordem-rec.numero-ordem AND 
                           nfe-relac-ordem-rec.parcela-oc     = tt-nfe-relac-ordem-rec.parcela-oc   EXCLUSIVE-LOCK NO-ERROR.
                
                IF NOT AVAIL nfe-relac-ordem-rec THEN 
                DO:
                    CREATE nfe-relac-ordem-rec.

                    ASSIGN nfe-relac-ordem-rec.chave-acesso   = tt-nfe-relac-ordem-rec.chave-acesso
                           nfe-relac-ordem-rec.seq-item       = tt-nfe-relac-ordem-rec.seq-item
                           nfe-relac-ordem-rec.num-pedido     = tt-nfe-relac-ordem-rec.num-pedido
                           nfe-relac-ordem-rec.numero-ordem   = tt-nfe-relac-ordem-rec.numero-ordem
                           nfe-relac-ordem-rec.parcela-oc     = tt-nfe-relac-ordem-rec.parcela-oc.
                END.
        
                ASSIGN nfe-relac-ordem-rec.quantidade       = tt-nfe-relac-ordem-rec.quantidade
                       nfe-relac-ordem-rec.vl-unitario      = tt-nfe-relac-ordem-rec.vl-unitario
                       nfe-relac-ordem-rec.nr-ord-produ     = tt-nfe-relac-ordem-rec.nr-ord-produ.

                /*Unificaîao de conceitos Totvs11 Conta Contabil*/
                &IF '{&bf_dis_versao_ems}' <= '2.07' &THEN 
                   ASSIGN nfe-relac-ordem-rec.conta-contabil   = tt-nfe-relac-ordem-rec.conta-contabil.

                &ELSE
                   ASSIGN nfe-relac-ordem-rec.ct-codigo       = tt-nfe-relac-ordem-rec.ct-codigo
                          nfe-relac-ordem-rec.sc-codigo       = tt-nfe-relac-ordem-rec.sc-codigo
                          nfe-relac-ordem-rec.cod-unid-negoc  = tt-nfe-relac-ordem-rec.cod-unid-negoc.
                &ENDIF
                
            END.
        END.

        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "ConfirmaRelacOrdem"
               tt-epc.cod-parameter = "p_rowid_item(ROWID)"
               tt-epc.val-parameter = STRING(p_rowid_item).

        {include\i-epc201.i "ConfirmaRelacOrdem"}
    END.
    
END PROCEDURE. /* pi_relaciona_ordem */

/**************************************************************************************************************************/

PROCEDURE pi_relaciona_ser_lote :

    DEF INPUT   PARAM p_rowid_item          AS ROWID                                NO-UNDO.
    DEF INPUT   PARAM p_it_codigo           AS CHAR FORMAT "x(016)"                 NO-UNDO.
    DEF INPUT   PARAM p_un                  AS CHAR FORMAT "XX"                     NO-UNDO.
    DEF INPUT   PARAM p_quantidade          AS DECI FORMAT ">>>>>,>>9.9999"         NO-UNDO.
    DEF INPUT   PARAM p_lote_serie          AS CHAR FORMAT "x(010)"                 NO-UNDO.
    DEF INPUT   PARAM p_dt_vali_lote        AS DATE FORMAT "99/99/9999"             NO-UNDO.
    DEF OUTPUT  PARAM p_mensagem_erro       AS CHAR FORMAT "x(200)" INITIAL ""      NO-UNDO.

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-LOCK NO-ERROR.

    IF AVAIL bf-nfe-it-nota-fisc-rec THEN 
    DO:
        /* --- Valida Quantidade --- */

        IF p_quantidade = 0 THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                       +
                                     "Quantidade invalida !,"   +
                                     "Preencha a Quantidade corretamente.".
            LEAVE.
        END.

        /* --- Carrega Lote/Serie ja relacionadas --- */

        EMPTY TEMP-TABLE tt-nfe-relac-ser-lote-rec.

        FOR EACH nfe-relac-ser-lote-rec WHERE 
                 nfe-relac-ser-lote-rec.chave-acesso   = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                 nfe-relac-ser-lote-rec.seq-item       = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK:
            CREATE tt-nfe-relac-ser-lote-rec.

            BUFFER-COPY nfe-relac-ser-lote-rec TO tt-nfe-relac-ser-lote-rec.
        END.

        /* --- Executa tela de Relacionar Lote/Serie --- */

        RUN dsc\ra\esp\esnfe200e02.w (INPUT p_rowid_item,
                                      INPUT p_it_codigo,
                                      INPUT p_un,
                                      INPUT p_quantidade,
                                      INPUT-OUTPUT TABLE tt-nfe-relac-ser-lote-rec,
                                      OUTPUT l-aux).

        /* --- Confirmado pelo botao de OK --- */

        IF l-aux = YES THEN 
        DO:
            /* --- Deleta registros --- */

            FOR EACH tt-nfe-relac-ser-lote-rec WHERE 
                     tt-nfe-relac-ser-lote-rec.l-retirar = YES NO-LOCK :
                FIND FIRST nfe-relac-ser-lote-rec WHERE 
                           nfe-relac-ser-lote-rec.chave-acesso     = tt-nfe-relac-ser-lote-rec.chave-acesso AND 
                           nfe-relac-ser-lote-rec.seq-item         = tt-nfe-relac-ser-lote-rec.seq-item     AND 
                           nfe-relac-ser-lote-rec.ser-lote         = tt-nfe-relac-ser-lote-rec.ser-lote     AND 
                           nfe-relac-ser-lote-rec.cod-depos        = tt-nfe-relac-ser-lote-rec.cod-depos    AND 
                           nfe-relac-ser-lote-rec.cod-localiz      = tt-nfe-relac-ser-lote-rec.cod-localiz  EXCLUSIVE-LOCK NO-ERROR.

                IF AVAIL nfe-relac-ser-lote-rec THEN 
                DO:
                    DELETE nfe-relac-ser-lote-rec.

                    RELEASE nfe-relac-ser-lote-rec.
                END.

                DELETE tt-nfe-relac-ser-lote-rec.

                RELEASE tt-nfe-relac-ser-lote-rec.
            END.
                
            /* --- Gera tabela de Controle --- */

            FOR EACH tt-nfe-relac-ser-lote-rec NO-LOCK :
                FIND FIRST nfe-relac-ser-lote-rec WHERE 
                           nfe-relac-ser-lote-rec.chave-acesso     = tt-nfe-relac-ser-lote-rec.chave-acesso AND 
                           nfe-relac-ser-lote-rec.seq-item         = tt-nfe-relac-ser-lote-rec.seq-item     AND 
                           nfe-relac-ser-lote-rec.ser-lote         = tt-nfe-relac-ser-lote-rec.ser-lote     AND 
                           nfe-relac-ser-lote-rec.cod-depos        = tt-nfe-relac-ser-lote-rec.cod-depos    AND 
                           nfe-relac-ser-lote-rec.cod-localiz      = tt-nfe-relac-ser-lote-rec.cod-localiz  EXCLUSIVE-LOCK NO-ERROR.

                IF NOT AVAIL nfe-relac-ser-lote-rec THEN 
                DO:
                    CREATE nfe-relac-ser-lote-rec.

                    ASSIGN nfe-relac-ser-lote-rec.chave-acesso  = tt-nfe-relac-ser-lote-rec.chave-acesso
                           nfe-relac-ser-lote-rec.seq-item      = tt-nfe-relac-ser-lote-rec.seq-item
                           nfe-relac-ser-lote-rec.ser-lote      = tt-nfe-relac-ser-lote-rec.ser-lote
                           nfe-relac-ser-lote-rec.cod-depos     = tt-nfe-relac-ser-lote-rec.cod-depos
                           nfe-relac-ser-lote-rec.cod-localiz   = tt-nfe-relac-ser-lote-rec.cod-localiz.
                END.
                    
                ASSIGN nfe-relac-ser-lote-rec.quantidade    = tt-nfe-relac-ser-lote-rec.quantidade
                       nfe-relac-ser-lote-rec.dt-vali-lote  = tt-nfe-relac-ser-lote-rec.dt-vali-lote.
            END.
        END.
            
        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "ConfirmaRelacSerLote"
               tt-epc.cod-parameter = "p_rowid_item(ROWID)"
               tt-epc.val-parameter = STRING(p_rowid_item).

        {include\i-epc201.i "ConfirmaRelacSerLote"}
    END.
        
END PROCEDURE. /* pi_relaciona_ser_lote */

/**************************************************************************************************************************/

PROCEDURE pi_relaciona_doc_nota :

    DEF INPUT   PARAM p_rowid_item          AS ROWID                                NO-UNDO.
    DEF INPUT   PARAM p_it_codigo           AS CHAR FORMAT "x(016)"                 NO-UNDO.
    DEF INPUT   PARAM p_un                  AS CHAR FORMAT "XX"                     NO-UNDO.
    DEF INPUT   PARAM p_quantidade          AS DECI FORMAT ">>>>>,>>9.9999"         NO-UNDO.
    DEF INPUT   PARAM p_natur_oper          AS CHAR FORMAT "x(008)"                 NO-UNDO.
    DEF INPUT   PARAM p_nro_docto           AS CHAR FORMAT "x(016)"                 NO-UNDO.
    DEF INPUT   PARAM p_serie_docto         AS CHAR FORMAT "x(005)"                 NO-UNDO.
    DEF INPUT   PARAM p_seq_docto           AS INTE FORMAT ">>>>9"                  NO-UNDO.
    DEF INPUT   PARAM p_preco_unit          AS DEC  FORMAT ">,>>>,>>>,>>9.99999"    NO-UNDO.
    DEF INPUT   PARAM p_preco_unit-xml      AS DEC  FORMAT ">,>>>,>>>,>>9.99999"    NO-UNDO.
    DEF INPUT   PARAM p_tipo_relac          AS INTE FORMAT "9"                      NO-UNDO. /* 1- Relac Normal / 2 - Relac Estrutura */

    DEF OUTPUT  PARAM p_mensagem_erro       AS CHAR FORMAT "x(200)" INITIAL ""      NO-UNDO.

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-LOCK NO-ERROR.

    IF AVAIL bf-nfe-it-nota-fisc-rec THEN 
    DO:
        /* --- Valida Quantidade --- */

        IF p_quantidade = 0 THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                       +
                                     "Quantidade invalida !,"   +
                                     "Preencha a Quantidade corretamente.".
            LEAVE.
        END.

        /* --- Verifica Natureza de Operacao --- */

        IF NOT CAN-FIND(FIRST natur-oper WHERE 
                              natur-oper.nat-operacao = p_natur_oper NO-LOCK) THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                               +
                                     "Natureza de Operacao invalida !," +
                                     "Preencha a Natureza de Operacao corretamente.".

            LEAVE.
        END.

        /* --- Verifica se tem Relacionamento de Ordens --- */

        IF CAN-FIND(FIRST nfe-relac-ordem-rec WHERE 
                          nfe-relac-ordem-rec.chave-acesso   = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                          nfe-relac-ordem-rec.seq-item       = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK) THEN 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                       +
                                     "Existem Ordens de Compra Relacionadas !," +
                                     "Nao eh possivel relacionar Documentos ou Notas Referenciadadas para o Item com relacionamento de Ordens de Compra.".

            LEAVE.
        END.

        IF p_tipo_relac <> 1 THEN 
        DO:
            IF CAN-FIND(FIRST nfe-nota-fiscal-rec OF bf-nfe-it-nota-fisc-rec WHERE 
                              nfe-nota-fiscal-rec.ide-dTrans = ? NO-LOCK) THEN 
            DO:
                ASSIGN p_mensagem_erro = "1,"                            +
                                         "Data de Transacao Invalida !," +
                                         "Nao eh possivel relacionar Documentos ou Notas Referenciadadas quando a data de transacao nao for informada.".
                LEAVE.
            END.
        END.

        /* --- Carrega Docto/Nota ja relacionadas --- */

        EMPTY TEMP-TABLE tt-nfe-relac-doc-nota-rec.

        FOR EACH nfe-relac-doc-nota-rec WHERE 
                 nfe-relac-doc-nota-rec.chave-acesso   = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                 nfe-relac-doc-nota-rec.seq-item       = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK:
            CREATE tt-nfe-relac-doc-nota-rec.

            BUFFER-COPY nfe-relac-doc-nota-rec TO tt-nfe-relac-doc-nota-rec.
        END.

        /* --- Executa tela de Relacionar Docto/Nota --- */

        IF p_tipo_relac = 1 THEN
            RUN dsc\ra\esp\esnfe200e03.w (INPUT p_rowid_item,
                                          INPUT p_it_codigo,
                                          INPUT p_un,
                                          INPUT p_quantidade,
                                          INPUT p_natur_oper,
                                          INPUT p_preco_unit,
                                          INPUT p_preco_unit-xml,
                                          INPUT-OUTPUT TABLE tt-nfe-relac-doc-nota-rec,
                                          OUTPUT l-aux).
        ELSE
            RUN dsc\ra\esp\esnfe200e03a.w (INPUT p_rowid_item,
                                           INPUT p_it_codigo,
                                           INPUT p_un,
                                           INPUT p_quantidade,
                                           INPUT p_natur_oper,
                                           INPUT p_preco_unit,
                                           INPUT p_preco_unit-xml,
                                           INPUT-OUTPUT TABLE tt-nfe-relac-doc-nota-rec,
                                           OUTPUT l-aux).


        /* --- Confirmado pelo botao de OK --- */

        IF l-aux = YES THEN 
        DO:
            /* --- Retorna o Tipo de Nota de acordo com a Natureza --- */

            RUN pi_valida_tipo_nota (INPUT  p_natur_oper,
                                     OUTPUT i-tipo-nfe,
                                     OUTPUT i-tp-oper-terc).
    
            IF i-tipo-nfe = 0 THEN
            DO:
                ASSIGN p_mensagem_erro = "1,"                                     +
                                         "Natureza de operacao nao encontrada !," +
                                         "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_RELACIONA_DOC_NOTA)".

                RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                      OUTPUT i-acao).
            END.

            /* --- Deleta registros --- */

            FOR EACH tt-nfe-relac-doc-nota-rec WHERE 
                     tt-nfe-relac-doc-nota-rec.l-retirar = YES NO-LOCK:

                /* --- Devolucao de Clientes --- */

                IF i-tipo-nfe = 2 THEN /* 2 - NF-e de devolucao */
                DO:
                    FIND FIRST nfe-relac-doc-nota-rec WHERE 
                               nfe-relac-doc-nota-rec.chave-acesso  = tt-nfe-relac-doc-nota-rec.chave-acesso  AND 
                               nfe-relac-doc-nota-rec.seq-item      = tt-nfe-relac-doc-nota-rec.seq-item      AND 
                               nfe-relac-doc-nota-rec.cod-estabel   = tt-nfe-relac-doc-nota-rec.cod-estabel   AND 
                               nfe-relac-doc-nota-rec.serie         = tt-nfe-relac-doc-nota-rec.serie         AND 
                               nfe-relac-doc-nota-rec.nr-docto-nota = tt-nfe-relac-doc-nota-rec.nr-docto-nota AND 
                               nfe-relac-doc-nota-rec.sequencia     = tt-nfe-relac-doc-nota-rec.sequencia     AND 
                               nfe-relac-doc-nota-rec.it-codigo     = tt-nfe-relac-doc-nota-rec.it-codigo     EXCLUSIVE-LOCK NO-ERROR.
                END.

                /* --- Retorno Terceiros --- */

                IF i-tipo-nfe = 3 THEN /* 3 - NF-e de Transfer?ncia */
                DO:
                    FIND FIRST nfe-relac-doc-nota-rec WHERE 
                               nfe-relac-doc-nota-rec.chave-acesso  = tt-nfe-relac-doc-nota-rec.chave-acesso  AND 
                               nfe-relac-doc-nota-rec.seq-item      = tt-nfe-relac-doc-nota-rec.seq-item      AND 
                               nfe-relac-doc-nota-rec.serie         = tt-nfe-relac-doc-nota-rec.serie         AND 
                               nfe-relac-doc-nota-rec.nr-docto-nota = tt-nfe-relac-doc-nota-rec.nr-docto-nota AND 
                               nfe-relac-doc-nota-rec.cod-emitente  = tt-nfe-relac-doc-nota-rec.cod-emitente  AND 
                               nfe-relac-doc-nota-rec.nat-operacao  = tt-nfe-relac-doc-nota-rec.nat-operacao  AND 
                               nfe-relac-doc-nota-rec.it-codigo     = tt-nfe-relac-doc-nota-rec.it-codigo     AND 
                               nfe-relac-doc-nota-rec.cod-refer     = tt-nfe-relac-doc-nota-rec.cod-refer     AND 
                               nfe-relac-doc-nota-rec.sequencia     = tt-nfe-relac-doc-nota-rec.sequencia     EXCLUSIVE-LOCK NO-ERROR.
                END.
                    
                IF AVAIL nfe-relac-doc-nota-rec THEN 
                DO:
                    DELETE nfe-relac-doc-nota-rec.

                    RELEASE nfe-relac-doc-nota-rec.
                END.

                DELETE tt-nfe-relac-doc-nota-rec.

                RELEASE tt-nfe-relac-doc-nota-rec.
            END.
    
            /* --- Gera tabela de Controle --- */

            FOR EACH tt-nfe-relac-doc-nota-rec NO-LOCK:

                /* --- Devolucao de Clientes --- */

                IF i-tipo-nfe = 2 THEN /* 2 - NF-e de devolucao */
                DO:
                    FIND FIRST nfe-relac-doc-nota-rec WHERE 
                               nfe-relac-doc-nota-rec.chave-acesso  = tt-nfe-relac-doc-nota-rec.chave-acesso  AND 
                               nfe-relac-doc-nota-rec.seq-item      = tt-nfe-relac-doc-nota-rec.seq-item      AND 
                               nfe-relac-doc-nota-rec.cod-estabel   = tt-nfe-relac-doc-nota-rec.cod-estabel   AND 
                               nfe-relac-doc-nota-rec.serie         = tt-nfe-relac-doc-nota-rec.serie         AND 
                               nfe-relac-doc-nota-rec.nr-docto-nota = tt-nfe-relac-doc-nota-rec.nr-docto-nota AND 
                               nfe-relac-doc-nota-rec.sequencia     = tt-nfe-relac-doc-nota-rec.sequencia     AND 
                               nfe-relac-doc-nota-rec.it-codigo     = tt-nfe-relac-doc-nota-rec.it-codigo     EXCLUSIVE-LOCK NO-ERROR.

                    IF NOT AVAIL nfe-relac-doc-nota-rec THEN 
                    DO:
                        CREATE nfe-relac-doc-nota-rec.

                        ASSIGN nfe-relac-doc-nota-rec.chave-acesso  = tt-nfe-relac-doc-nota-rec.chave-acesso
                               nfe-relac-doc-nota-rec.seq-item      = tt-nfe-relac-doc-nota-rec.seq-item
                               nfe-relac-doc-nota-rec.cod-estabel   = tt-nfe-relac-doc-nota-rec.cod-estabel
                               nfe-relac-doc-nota-rec.serie         = tt-nfe-relac-doc-nota-rec.serie
                               nfe-relac-doc-nota-rec.nr-docto-nota = tt-nfe-relac-doc-nota-rec.nr-docto-nota
                               nfe-relac-doc-nota-rec.sequencia     = tt-nfe-relac-doc-nota-rec.sequencia
                               nfe-relac-doc-nota-rec.it-codigo     = tt-nfe-relac-doc-nota-rec.it-codigo.
                    END.
                END.

                /* --- Retorno Terceiros --- */

                IF i-tipo-nfe = 3 THEN /* 3 - NF-e de Transfer?ncia */
                DO:
                    FIND FIRST nfe-relac-doc-nota-rec WHERE 
                               nfe-relac-doc-nota-rec.chave-acesso  = tt-nfe-relac-doc-nota-rec.chave-acesso  AND 
                               nfe-relac-doc-nota-rec.seq-item      = tt-nfe-relac-doc-nota-rec.seq-item      AND 
                               nfe-relac-doc-nota-rec.serie         = tt-nfe-relac-doc-nota-rec.serie         AND 
                               nfe-relac-doc-nota-rec.nr-docto-nota = tt-nfe-relac-doc-nota-rec.nr-docto-nota AND 
                               nfe-relac-doc-nota-rec.cod-emitente  = tt-nfe-relac-doc-nota-rec.cod-emitente  AND 
                               nfe-relac-doc-nota-rec.nat-operacao  = tt-nfe-relac-doc-nota-rec.nat-operacao  AND 
                               nfe-relac-doc-nota-rec.it-codigo     = tt-nfe-relac-doc-nota-rec.it-codigo     AND 
                               nfe-relac-doc-nota-rec.cod-refer     = tt-nfe-relac-doc-nota-rec.cod-refer     AND 
                               nfe-relac-doc-nota-rec.sequencia     = tt-nfe-relac-doc-nota-rec.sequencia     EXCLUSIVE-LOCK NO-ERROR.

                    IF NOT AVAIL nfe-relac-doc-nota-rec THEN 
                    DO:
                        CREATE nfe-relac-doc-nota-rec.

                        ASSIGN nfe-relac-doc-nota-rec.chave-acesso  = tt-nfe-relac-doc-nota-rec.chave-acesso
                               nfe-relac-doc-nota-rec.seq-item      = tt-nfe-relac-doc-nota-rec.seq-item
                               nfe-relac-doc-nota-rec.serie         = tt-nfe-relac-doc-nota-rec.serie
                               nfe-relac-doc-nota-rec.nr-docto-nota = tt-nfe-relac-doc-nota-rec.nr-docto-nota
                               nfe-relac-doc-nota-rec.cod-emitente  = tt-nfe-relac-doc-nota-rec.cod-emitente
                               nfe-relac-doc-nota-rec.nat-operacao  = tt-nfe-relac-doc-nota-rec.nat-operacao
                               nfe-relac-doc-nota-rec.it-codigo     = tt-nfe-relac-doc-nota-rec.it-codigo
                               nfe-relac-doc-nota-rec.cod-refer     = tt-nfe-relac-doc-nota-rec.cod-refer
                               nfe-relac-doc-nota-rec.sequencia     = tt-nfe-relac-doc-nota-rec.sequencia.
                    END.
                END.
    
                IF i-tipo-nfe = 2 OR   /* 2 - NF-e de devolucao     */
                   i-tipo-nfe = 3 THEN /* 3 - NF-e de Transfer?ncia */
                    ASSIGN nfe-relac-doc-nota-rec.nr-ord-produ = tt-nfe-relac-doc-nota-rec.nr-ord-produ
                           nfe-relac-doc-nota-rec.quantidade   = tt-nfe-relac-doc-nota-rec.quantidade
                           nfe-relac-doc-nota-rec.cod-depos    = tt-nfe-relac-doc-nota-rec.cod-depos.
            END. /* FOR EACH tt-nfe-relac-doc-nota-rec NO-LOCK....*/
        END. /* IF l-aux = YES THEN .... */

        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "ConfirmaRelacDoctoNota"
               tt-epc.cod-parameter = "p_rowid_item(ROWID)"
               tt-epc.val-parameter = STRING(p_rowid_item).

        {include\i-epc201.i "ConfirmaRelacDoctoNota"}
    END.

END PROCEDURE. /* pi_relaciona_doc_nota */

/**************************************************************************************************************************/

PROCEDURE pi_nota_receb_fiscal :
    
    DEF INPUT  PARAM p_rowid_nota       AS ROWID                                NO-UNDO.
    DEF INPUT  PARAM p_natur_oper       AS CHAR FORMAT "x(008)"                 NO-UNDO.
    DEF INPUT  PARAM p_vl_tot_prod      AS DECI                                 NO-UNDO.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-docum-est.
    DEF OUTPUT PARAM p_mensagem_erro    AS CHAR FORMAT "x(200)" INITIAL ""      NO-UNDO.

    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_nota NO-LOCK NO-ERROR.

    IF AVAIL bfu-nfe-nota-fiscal-rec THEN 
    DO:
        FIND emitente WHERE 
             emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

        FIND FIRST estab-mat WHERE 
                   estab-mat.cod-estabel = nfe-nota-fiscal-rec.cod-estabel NO-LOCK NO-ERROR.


        /* Passei atualizar data de transa?ao caso esteja em branco ch 7451*/

        FIND CURRENT bfu-nfe-nota-fiscal-rec EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL bfu-nfe-nota-fiscal-rec THEN
            IF bfu-nfe-nota-fiscal-rec.ide-dTrans = ? THEN ASSIGN bfu-nfe-nota-fiscal-rec.ide-dTrans = TODAY.

        FIND CURRENT bfu-nfe-nota-fiscal-rec NO-LOCK NO-ERROR.


        /* --- Documento --- */

        CREATE tt-docum-est.

        ASSIGN tt-docum-est.serie-docto  = STRING(bfu-nfe-nota-fiscal-rec.ide-Serie)
               tt-docum-est.nro-docto    = bfu-nfe-nota-fiscal-rec.ide-nNF
               tt-docum-est.cod-emitente = IF AVAIL emitente THEN emitente.cod-emitente ELSE 0
               tt-docum-est.cotacao-dia  = 1
               tt-docum-est.uf           = bfu-nfe-nota-fiscal-rec.emit-UF
               tt-docum-est.nat-operacao = p_natur_oper
               tt-docum-est.cod-estabel  = bfu-nfe-nota-fiscal-rec.cod-estabel
               tt-docum-est.estab-fisc   = bfu-nfe-nota-fiscal-rec.cod-estabel
               tt-docum-est.dt-emissao   = DATE(INTE(SUBSTR(bfu-nfe-nota-fiscal-rec.ide-dEmi,5,2)),
                                                INTE(SUBSTR(bfu-nfe-nota-fiscal-rec.ide-dEmi,7,2)),
                                                INTE(SUBSTR(bfu-nfe-nota-fiscal-rec.ide-dEmi,1,4)))
               tt-docum-est.dt-trans     = bfu-nfe-nota-fiscal-rec.ide-dTrans
               /*tt-docum-est.tot-valor    = p_vl_tot_prod Renato Total*/
               tt-docum-est.i-sequencia  = 10
               tt-docum-est.usuario      = c-seg-usuario
               tt-docum-est.cod-observa  = bfu-nfe-nota-fiscal-rec.cod-observa.

        &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN 
           ASSIGN tt-docum-est.cod-chave-aces-nf-eletro =  bfu-nfe-nota-fiscal-rec.chave-acesso-nfe.
        &ENDIF
            
        ASSIGN SUBSTR(tt-docum-est.char-1,113,02) = "55"
               SUBSTR(tt-docum-est.char-1,093,44) = bfu-nfe-nota-fiscal-rec.chave-acesso-nfe.

        /* --- Nota com Item Tipo servico --- */         
        IF CAN-FIND(FIRST bf-nfe-it-nota-fisc-rec OF bfu-nfe-nota-fiscal-rec WHERE 
                          bf-nfe-it-nota-fisc-rec.item-nat-operacao = p_natur_oper AND 
                          bf-nfe-it-nota-fisc-rec.imp-iss-vAliq     > 0) THEN 
            ASSIGN tt-docum-est.cod-observa = 4. /* --- servico --- */
        
        /* --- Verifica Se Importa Observacao da Nota --- */

        FIND FIRST nfe-param-rec WHERE 
                   nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

        IF AVAIL nfe-param-rec THEN 
        DO:
            FIND FIRST nfe-it-param-rec WHERE 
                       nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                       nfe-it-param-rec.cod-item-parametro = "importa_observacao"        NO-LOCK NO-ERROR.

            IF AVAIL nfe-it-param-rec                          AND
               nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN 
                ASSIGN tt-docum-est.observacao = bfu-nfe-nota-fiscal-rec.inf-infCpl.
        END.
    
        IF nfe-it-nota-fisc-rec.item-esp-nat = "NFE" THEN
            ASSIGN tt-docum-est.esp-docto = 21. /* --- ENTRADA   --- */

        IF nfe-it-nota-fisc-rec.item-esp-nat = "NFD" THEN
            ASSIGN tt-docum-est.esp-docto = 20. /* --- DEVOLUCAO --- */
    
        IF AVAIL estab-mat THEN 
        DO:
            CASE i-tipo-nfe:
                /* --- 1 - NFE de COMPRA --- */

                WHEN 1 THEN ASSIGN tt-docum-est.conta-transit = STRING(estab-mat.conta-fornec).

                /* --- 2 - NFE de DEVOLUCAO CLIENTE --- */

                WHEN 2 THEN ASSIGN tt-docum-est.conta-transit = STRING(estab-mat.conta-dev-cli).

                /* --- 3 - NFE de RETORNO TERCEIROS --- */

                WHEN 3 THEN 
                DO: 
                    /* --- 2 - Retorno Beneficiamento --- */

                    IF i-tp-oper-terc = 2 THEN ASSIGN tt-docum-est.conta-transit = STRING(estab-mat.conta-sai-benef).

                    /* --- Faturamento Consignacao --- */

                    IF i-tp-oper-terc = 4 THEN ASSIGN tt-docum-est.conta-transit = STRING(estab-mat.conta-fornec).

                    /* --- 5 - Retorno / Devolucao Consignacao --- */

                    IF i-tp-oper-terc = 5 THEN ASSIGN tt-docum-est.conta-transit = STRING(estab-mat.conta-sai-consig).
                END.

                /* --- 4 - NFE de REMESSA TERCEIROS --- */

                WHEN 4 THEN DO:
                    /* --- Remessa Beneficiamento --- */

                    IF i-tp-oper-terc = 1 THEN ASSIGN tt-docum-est.conta-transit = STRING(estab-mat.conta-ent-benef).

                    /* --- Remessa Consignacao --- */

                    IF i-tp-oper-terc = 3 THEN ASSIGN tt-docum-est.conta-transit = STRING(estab-mat.conta-ent-consig).
                END.
            END CASE.
        END.

        /* --- Frete, Despesas e Desconto --- */

        IF de-vFrete <> 0 THEN
            ASSIGN tt-docum-est.valor-frete = de-vFrete.

        IF de-vSeg <> 0 THEN
            ASSIGN tt-docum-est.valor-seguro = de-vSeg.

        IF de-vOutro <> 0 THEN
            ASSIGN tt-docum-est.valor-outras = de-vOutro.

        IF de-vFrete <> 0 OR
           de-vSeg   <> 0 OR 
           de-vOutro <> 0 THEN
            ASSIGN tt-docum-est.despesa-nota = (de-vFrete + de-vSeg + de-vOutro).

        IF de-vDesc <> 0 THEN
            ASSIGN tt-docum-est.tot-desconto = de-vDesc.

        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "NotaAntesRecebFiscal"
               tt-epc.cod-parameter = "tt-docum-est(HANDLE)"
               tt-epc.val-parameter = STRING(TEMP-TABLE tt-docum-est:HANDLE).

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "NotaAntesRecebFiscal"
               tt-epc.cod-parameter = "bfu-nfe-nota-fiscal-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(bfu-nfe-nota-fiscal-rec)).

        {include\i-epc201.i "NotaAntesRecebFiscal"}
    END.

END PROCEDURE. /* pi_nota_receb_fiscal */

/**************************************************************************************************************************/

PROCEDURE pi_item_receb_fiscal:
    
    DEF INPUT        PARAM p_rowid_item            AS   ROWID                                      NO-UNDO.
    DEF INPUT        PARAM p_sequencia             AS   INTE                                       NO-UNDO.
    DEF INPUT        PARAM p_item_xPed             LIKE nfe-it-nota-fisc-rec.item-xPed             NO-UNDO.
    DEF INPUT        PARAM p_item_num_ordem        LIKE nfe-it-nota-fisc-rec.item-num-ordem        NO-UNDO.
    DEF INPUT        PARAM p_item_parcela_oc       LIKE nfe-it-nota-fisc-rec.item-parcela-oc       NO-UNDO.
    DEF INPUT        PARAM p_item_qtde             LIKE nfe-it-nota-fisc-rec.item-qtde             NO-UNDO.
    DEF INPUT        PARAM p_item_qCom             LIKE nfe-it-nota-fisc-rec.item-qCom             NO-UNDO.
    DEF INPUT        PARAM p_item_vProd            LIKE nfe-it-nota-fisc-rec.item-vProd            NO-UNDO.
    DEF INPUT        PARAM p_item_vUnCom           LIKE nfe-it-nota-fisc-rec.item-vUnCom           NO-UNDO.
    DEF INPUT        PARAM p_item_conta_contabil   LIKE nfe-it-nota-fisc-rec.item-conta-contabil   NO-UNDO.
    DEF INPUT        PARAM p_item_nr_ord_produ     LIKE nfe-it-nota-fisc-rec.item-nr-ord-produ     NO-UNDO.
    DEF INPUT        PARAM p_item_cod_depos        LIKE nfe-it-nota-fisc-rec.item-cod-depos        NO-UNDO.
    DEF INPUT        PARAM p_item_cod_localiz      LIKE nfe-it-nota-fisc-rec.item-cod-localiz      NO-UNDO.
    DEF INPUT        PARAM p_item_ser_lote         LIKE nfe-it-nota-fisc-rec.item-ser-lote         NO-UNDO.
    DEF INPUT        PARAM p_item_dt_vali_lote     LIKE nfe-it-nota-fisc-rec.item-dt-vali-lote     NO-UNDO.
    DEF INPUT        PARAM p_item_cod_refer        LIKE nfe-it-nota-fisc-rec.item-cod-refer        NO-UNDO.
    DEF INPUT        PARAM p_item_serie_comp       LIKE nfe-it-nota-fisc-rec.item-serie-comp       NO-UNDO.
    DEF INPUT        PARAM p_item_nro_comp         LIKE nfe-it-nota-fisc-rec.item-nro-comp         NO-UNDO.
    DEF INPUT        PARAM p_item_nat_comp         LIKE nfe-it-nota-fisc-rec.item-nat-comp         NO-UNDO.
    DEF INPUT        PARAM p_item_seq_comp         LIKE nfe-it-nota-fisc-rec.item-seq-comp         NO-UNDO.
    DEF INPUT        PARAM p_item_data_comp        LIKE nfe-it-nota-fisc-rec.item-data-comp        NO-UNDO.
    DEF INPUT        PARAM p_preco_bruto           AS LOG                                          NO-UNDO.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-item-doc-est.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-item-doc-est-aux.
    DEF OUTPUT       PARAM p_mensagem_erro         AS   CHAR FORMAT "x(200)" INITIAL ""            NO-UNDO.
    DEFINE VARIABLE h-acomp-5                      AS HANDLE                                       NO-UNDO.
    DEFINE VARIABLE h_cd4337                       AS HANDLE.
    DEFINE VARIABLE pi-trib-pis                    AS INTEGER.
    DEFINE VARIABLE pi-aliquota-pis                AS DECIMAL.
    DEFINE VARIABLE pi-valor-un-pis                AS DECIMAL.
    DEFINE VARIABLE pi-reduz-pis                   AS DECIMAL.
    DEFINE VARIABLE pi-trib-cofins                 AS INTEGER.
    DEFINE VARIABLE pi-aliquota-cofins             AS DECIMAL.
    DEFINE VARIABLE pi-valor-un-cofins             AS DECIMAL.
    DEFINE VARIABLE pi-reduz-cofins                AS DECIMAL.

    DEF VAR c-un LIKE nfe-it-nota-fisc-rec.item-uCom NO-UNDO.
    DEFINE VARIABLE l-preco AS LOGICAL     NO-UNDO.             
    DEFINE VARIABLE c-cod-unid-negoc-aux AS CHARACTER   NO-UNDO.
    DEFINE BUFFER bf-emitente FOR emitente .

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp-5.
    RUN pi-inicializar IN h-acomp-5 (INPUT "Carregando Item Recebimento Fiscal").

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-LOCK NO-ERROR.

    IF AVAIL bf-nfe-it-nota-fisc-rec THEN DO:
        FIND FIRST bfu-nfe-nota-fiscal-rec OF bf-nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.

        RUN cdp/cd4337.p PERSISTENT SET h_cd4337. 

        RUN pi-acompanhar IN h-acomp-5 (INPUT "ITEM: " + bf-nfe-it-nota-fisc-rec.it-codigo + " seq. " + string(bf-nfe-it-nota-fisc-rec.seq-item)).

        FIND emitente WHERE 
             emitente.nome-abrev = bfu-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

        FIND FIRST estab-mat WHERE 
                   estab-mat.cod-estabel = bfu-nfe-nota-fiscal-rec.cod-estabel NO-LOCK NO-ERROR.

        /*** MOD.01 - Baione - 07/06/2011 ***/   
        IF AVAIL emitente THEN DO:
            FIND FIRST item-fornec WHERE 
                       item-fornec.cod-emitente = emitente.cod-emitente             AND 
                       item-fornec.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.

            IF AVAIL item-fornec THEN DO:
                IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN DO:
                    CREATE nfe-item-fornec.

                    BUFFER-COPY item-fornec TO nfe-item-fornec. 
                END.

                FIND FIRST nfe-item-fornec OF item-fornec NO-LOCK NO-ERROR.

                ASSIGN c-un = item-fornec.unid-med-for.

                IF item-fornec.fator-conver <> nfe-item-fornec.fator-conver OR
                   item-fornec.num-casa-dec <> nfe-item-fornec.num-casa-dec OR
                   item-fornec.unid-med-for <> nfe-item-fornec.unid-med-for  THEN DO:
                    FIND FIRST ITEM WHERE 
                               ITEM.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.
                    
                    ASSIGN de-fat-conv   = item-fornec.fator-conver * EXP(10,(item-fornec.num-casa-dec * (-1)))
                           p_item_qCom   = p_item_qtde * de-fat-conv
                           p_item_vUnCom = p_item_vProd / p_item_qtde.
                END.
            END.
        END. /* IF AVAIL emitente.... */

        IF c-un = '' THEN 
            ASSIGN c-un  =  bf-nfe-it-nota-fisc-rec.item-uCom.
        
        FIND FIRST nfe-it-imposto-alt NO-LOCK
            WHERE nfe-it-imposto-alt.chave-acesso-nfe = bf-nfe-it-nota-fisc-rec.chave-acesso-nfe
            AND   nfe-it-imposto-alt.seq-item         = bf-nfe-it-nota-fisc-rec.seq-item        NO-ERROR.

        CREATE tt-item-doc-est.
        ASSIGN tt-item-doc-est.serie-docto         = STRING(bfu-nfe-nota-fiscal-rec.ide-Serie)
               tt-item-doc-est.nro-docto           = bfu-nfe-nota-fiscal-rec.ide-nNF
               tt-item-doc-est.cod-emitente        = IF AVAIL emitente THEN emitente.cod-emitente ELSE 0
               tt-item-doc-est.nat-operacao        = IF l-multi_natureza THEN nat-oper-cabec ELSE bf-nfe-it-nota-fisc-rec.item-nat-operacao
               tt-item-doc-est.nat-of              = bf-nfe-it-nota-fisc-rec.item-nat-operacao
               tt-item-doc-est.it-codigo           = bf-nfe-it-nota-fisc-rec.it-codigo
               tt-item-doc-est.narrativa           = IF bf-nfe-it-nota-fisc-rec.narrativa = "" THEN bf-nfe-it-nota-fisc-rec.item-xprod ELSE bf-nfe-it-nota-fisc-rec.narrativa
               tt-item-doc-est.sequencia           = p_sequencia
               tt-item-doc-est.qt-do-forn          = p_item_qCom
               tt-item-doc-est.quantidade          = p_item_qtde
               tt-item-doc-est.preco-total[1]      = p_item_vProd
               tt-item-doc-est.preco-unit[1]       = p_item_vUnCom
               tt-item-doc-est.cod-depos           = p_item_cod_depos
               tt-item-doc-est.cod-localiz         = p_item_cod_localiz
               tt-item-doc-est.class-fiscal        = bf-nfe-it-nota-fisc-rec.item-ncm
               tt-item-doc-est.cod-refer           = p_item_cod_refer
               tt-item-doc-est.num-pedido          = p_item_xPed
               tt-item-doc-est.numero-ordem        = p_item_num_ordem
               tt-item-doc-est.parcela             = p_item_parcela_oc
               tt-item-doc-est.nr-ord-produ        = p_item_nr_ord_produ
               tt-item-doc-est.baixa-ce            = YES /*natur-oper.baixa-estoq*/
               tt-item-doc-est.lote                = p_item_ser_lote
               tt-item-doc-est.dt-vali-lote        = p_item_dt_vali_lote
               tt-item-doc-est.un                  = c-un

               /* --- IPI --- */                   
               tt-item-doc-est.aliquota-ipi         = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-ipi-pIPI
               tt-item-doc-est.valor-ipi[1]         = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-valor ELSE bf-nfe-it-nota-fisc-rec.imp-ipi-vIPI

               /* --- ISS --- */                    
               tt-item-doc-est.aliquota-iss         = bf-nfe-it-nota-fisc-rec.imp-iss-vAliq
               tt-item-doc-est.base-iss[1]          = bf-nfe-it-nota-fisc-rec.imp-iss-vBC
               tt-item-doc-est.valor-iss[1]          = bf-nfe-it-nota-fisc-rec.imp-iss-vISSQN
               /*ICMS ST*/                         
               tt-item-doc-est.VL-SUBS[1]           = bf-nfe-it-nota-fisc-rec.imp-vICMSST
               tt-item-doc-est.BASE-SUBS[1]         = bf-nfe-it-nota-fisc-rec.imp-vBCST
               /* --- ICM --- */                   
               tt-item-doc-est.aliquota-icm         = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.icms-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-pICMS
               tt-item-doc-est.base-icm[1]          = bf-nfe-it-nota-fisc-rec.imp-vBC
               tt-item-doc-est.valor-icm[1]         = IF bfu-nfe-nota-fiscal-rec.emit-crt = 1 THEN bf-nfe-it-nota-fisc-rec.imp-vCredICMSSN ELSE bf-nfe-it-nota-fisc-rec.imp-vICMS   /*emit-crt = 1 simples nacional*/
               tt-item-doc-est.val-perc-red-icms    = bf-nfe-it-nota-fisc-rec.imp-pRedBC           /* perc icm*/
               SUBSTR(tt-item-doc-est.char-2,84,5)  = STRING(bf-nfe-it-nota-fisc-rec.imp-cofins-vAliqProd) /* Aliq Cofins*/
               SUBSTR(tt-item-doc-est.char-2,840,7) = IF l-CEST THEN STRING(bf-nfe-it-nota-fisc-rec.item-CEST) ELSE "" /* CEST*/
               tt-item-doc-est.serie-comp           = p_item_serie_comp
               tt-item-doc-est.nro-comp             = p_item_nro_comp
               tt-item-doc-est.nat-comp             = p_item_nat_comp
               tt-item-doc-est.seq-comp             = p_item_seq_comp
               tt-item-doc-est.data-comp            = p_item_data_comp
               tt-item-doc-est.cod-emit-terc        = bf-nfe-it-nota-fisc-rec.cod-emit-terc
               tt-item-doc-est.serie-terc           = bf-nfe-it-nota-fisc-rec.serie-terc
               tt-item-doc-est.nro-docto-terc       = bf-nfe-it-nota-fisc-rec.nro-docto-terc
               tt-item-doc-est.seq-terc             = bf-nfe-it-nota-fisc-rec.seq-terc       .                           


        /*CST ICMS*/
        IF l-cst-icms-xml THEN
            ASSIGN SUBSTR(tt-item-doc-est.char-2,502,3) = string(bf-nfe-it-nota-fisc-rec.imp-orig) + string(bf-nfe-it-nota-fisc-rec.imp-CST). /*CST ICMS*/ 
        
        FIND FIRST natur-oper NO-LOCK
            WHERE natur-oper.nat-operacao = IF l-multi_natureza THEN tt-item-doc-est.nat-of ELSE tt-item-doc-est.nat-operacao NO-ERROR.                           
        
        IF i-tipo-nfe = 2 THEN DO: /*devolucao*/
            /*Bauducco Imposto Conceitual Se os valores estiverem zerados no XML assume regras do ERP*/

            IF tt-item-doc-est.aliquota-icm = 0 AND
               tt-item-doc-est.base-icm[1]  = 0 AND
               tt-item-doc-est.valor-icm[1] = 0 THEN 
                ASSIGN l-ICMS-dev-xml = NO.
            
            IF tt-item-doc-est.aliquota-ipi = 0 AND
               tt-item-doc-est.valor-ipi[1] = 0 THEN
                ASSIGN l-IPI-dev-xml = NO.          

            /*PIS Devolucao*/
            IF l-PIS-dev-xml THEN
                ASSIGN tt-item-doc-est.val-aliq-pis     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-pis-pPIS.
            
            FIND FIRST ITEM NO-LOCK
                    WHERE item.it-codigo = tt-item-doc-est.it-codigo NO-ERROR.
            RUN pi-tributa-aliquota-pis IN h_cd4337 (BUFFER estabelec,
                                                     BUFFER item,       
                                                     BUFFER emitente,   
                                                     BUFFER natur-oper, 
                                                     INPUT TODAY,
                                                     OUTPUT pi-trib-pis,    
                                                     OUTPUT pi-aliquota-pis,
                                                     OUTPUT pi-valor-un-pis,
                                                     OUTPUT pi-reduz-pis).  

            ASSIGN tt-item-doc-est.idi-tributac-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-tribut else pi-trib-pis.
            IF l-PIS-dev-xml = NO THEN 
                ASSIGN tt-item-doc-est.val-aliq-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE pi-aliquota-pis
                       tt-item-doc-est.valor-pis    = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-val ELSE pi-valor-un-pis.

            /*cofins Devolucao*/
            IF l-COFINS-dev-xml THEN
                ASSIGN tt-item-doc-est.val-aliq-cofins     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-cofins-pCOFINS.
            
            RUN pi-tributa-aliquota-cofins IN h_cd4337 (BUFFER estabelec,
                                                        BUFFER item,       
                                                        BUFFER emitente,   
                                                        BUFFER natur-oper, 
                                                        INPUT TODAY,
                                                        OUTPUT pi-trib-cofins,    
                                                        OUTPUT pi-aliquota-cofins,
                                                        OUTPUT pi-valor-un-cofins,
                                                        OUTPUT pi-reduz-cofins).  
            ASSIGN tt-item-doc-est.idi-tributac-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-tribut else pi-trib-cofins.
            IF l-COFINS-dev-xml = NO THEN 
                ASSIGN tt-item-doc-est.val-aliq-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE pi-aliquota-cofins 
                       tt-item-doc-est.val-cofins      = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-valor ELSE pi-valor-un-cofins.

        END.
        ELSE DO: /*Outras Notas diferente de Devolucao*/
            
            /*Bauducco Imposto Conceitual Se os valores estiverem zerados no XML assume regras do ERP*/

            IF tt-item-doc-est.aliquota-icm = 0 AND
               tt-item-doc-est.base-icm[1]  = 0 AND
               tt-item-doc-est.valor-icm[1] = 0 THEN
                ASSIGN l-ICMS-xml = NO.
            
            IF tt-item-doc-est.aliquota-ipi = 0 AND
               tt-item-doc-est.valor-ipi[1] = 0 THEN 
                ASSIGN l-IPI-xml = NO.                

            /*PIS Compras*/
            IF l-PIS-xml THEN
                ASSIGN tt-item-doc-est.val-aliq-pis     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-pis-pPIS.
            
            RUN pi-tributa-aliquota-pis IN h_cd4337 (BUFFER estabelec,
                                                     BUFFER item,       
                                                     BUFFER emitente,   
                                                     BUFFER natur-oper, 
                                                     INPUT TODAY,
                                                     OUTPUT pi-trib-pis,    
                                                     OUTPUT pi-aliquota-pis,
                                                     OUTPUT pi-valor-un-pis,
                                                     OUTPUT pi-reduz-pis).  

            ASSIGN tt-item-doc-est.idi-tributac-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-tribut else pi-trib-pis.
            IF l-PIS-xml = NO THEN 
                ASSIGN tt-item-doc-est.val-aliq-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE pi-aliquota-pis
                       tt-item-doc-est.valor-pis    = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-val ELSE pi-valor-un-pis.

            /*cofins Compras*/
            IF l-COFINS-xml THEN
                ASSIGN tt-item-doc-est.val-aliq-cofins     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-cofins-pCOFINS.
            
            RUN pi-tributa-aliquota-cofins IN h_cd4337 (BUFFER estabelec,
                                                        BUFFER item,       
                                                        BUFFER emitente,   
                                                        BUFFER natur-oper, 
                                                        INPUT TODAY,
                                                        OUTPUT pi-trib-cofins,    
                                                        OUTPUT pi-aliquota-cofins,
                                                        OUTPUT pi-valor-un-cofins,
                                                        OUTPUT pi-reduz-cofins).  
            
            ASSIGN tt-item-doc-est.idi-tributac-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-tribut else pi-trib-cofins.
            IF l-COFINS-xml = NO THEN 
                ASSIGN tt-item-doc-est.val-aliq-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE pi-aliquota-cofins 
                       tt-item-doc-est.val-cofins      = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-valor ELSE pi-valor-un-cofins.



        END.
        /*Unificacao de conceitos Totvs11 Conta Contabil*/
        &IF '{&bf_dis_versao_ems}' <= '2.07' &THEN 
            FIND FIRST nfe-nota-fiscal-rec NO-LOCK
                WHERE nfe-nota-fiscal-rec.chave-acesso-nfe = bf-nfe-it-nota-fisc-rec.chave-acesso-nfe NO-ERROR.
            RUN pi_unidade_negocio (INPUT  bfu-nfe-nota-fiscal-rec.cod-estabel,
                            INPUT  tt-item-doc-est.it-codigo,
                            INPUT  tt-item-doc-est.numero-ordem,  
                            OUTPUT c-cod-unid-negoc-aux).
                
            ASSIGN tt-item-doc-est.cod-unid-negoc = c-cod-unid-negoc-aux.
        
            ASSIGN tt-item-doc-est.conta-contabil      = p_item_conta_contabil.

        &ELSE
        
            IF AVAIL nfe-relac-ordem-rec THEN
               ASSIGN tt-item-doc-est.ct-codigo      = nfe-relac-ordem-rec.ct-codigo
                      tt-item-doc-est.sc-codigo      = nfe-relac-ordem-rec.sc-codigo
                      tt-item-doc-est.cod-unid-negoc = nfe-relac-ordem-rec.cod-unid-negoc.

            ELSE
               ASSIGN tt-item-doc-est.ct-codigo      = bf-nfe-it-nota-fisc-rec.ct-codigo
                      tt-item-doc-est.sc-codigo      = bf-nfe-it-nota-fisc-rec.sc-codigo
                      tt-item-doc-est.cod-unid-negoc = bf-nfe-it-nota-fisc-rec.cod-unid-negoc.

        &ENDIF       
        
        IF p_preco_bruto THEN DO:

            FIND FIRST ordem-compra NO-LOCK
                WHERE ordem-compra.numero-ordem = p_item_num_ordem NO-ERROR.
            IF AVAIL ordem-compra THEN DO:
                
                ASSIGN tt-item-doc-est.preco-total[1] = tt-item-doc-est.preco-total[1] + (tt-item-doc-est.quantidade * ordem-compra.valor-descto)
                       tt-item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-unit[1]  + ordem-compra.valor-descto.
            END.
        END.
        
        /* --- Retorna o Tipo de Nota de acordo com a Natureza --- */

        RUN pi_valida_tipo_nota (INPUT  bf-nfe-it-nota-fisc-rec.item-nat-operacao,
                                 OUTPUT i-tipo-nfe,
                                 OUTPUT i-tp-oper-terc).
        
        IF i-tipo-nfe = 0 THEN DO:
            
            ASSIGN p_mensagem_erro = "1,"                                     +
                                     "Natureza de operacao nao encontrada !," +
                                     "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_ITEM_RECEB_FISCAL)".

            RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                  OUTPUT i-acao).
        END.

        /* --- FIFO e Ordens de Compra --- */
        IF bf-nfe-it-nota-fisc-rec.item-fifo-oc = YES THEN DO:
            &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN 
                ASSIGN tt-item-doc-est.log-fifo-oc = YES
                       tt-item-doc-est.log-1       = YES.
            &ELSE 
                ASSIGN tt-item-doc-est.log-1       = YES.
            &ENDIF                                       
        END.

        ASSIGN de-vFrete   = de-vFrete   + bf-nfe-it-nota-fisc-rec.item-vFrete 
               de-vSeg     = de-vSeg     + bf-nfe-it-nota-fisc-rec.item-vSeg
               de-vDesc    = de-vDesc    + bf-nfe-it-nota-fisc-rec.item-vDesc.
               de-vOutro   = de-vOutro   + bf-nfe-it-nota-fisc-rec.item-vOutro.
                                               
        /* --- Frete, Despesas e Desconto --- */
        IF bf-nfe-it-nota-fisc-rec.item-vDesc <> 0 THEN
            ASSIGN tt-item-doc-est.desconto[1] = bf-nfe-it-nota-fisc-rec.item-vDesc.

        IF bf-nfe-it-nota-fisc-rec.item-vSeg   <> 0 OR 
           bf-nfe-it-nota-fisc-rec.item-vFrete <> 0 OR
           bf-nfe-it-nota-fisc-rec.item-vOutro <> 0 THEN DO:

            ASSIGN tt-item-doc-est.despesas[1] = (bf-nfe-it-nota-fisc-rec.item-vSeg + bf-nfe-it-nota-fisc-rec.item-vFrete + bf-nfe-it-nota-fisc-rec.item-vOutro).

            ASSIGN i-rel-doc = 0
                   de-vOutro = 0.

            FOR EACH nfe-relac-doc-nota-rec 
                WHERE nfe-relac-doc-nota-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso-nfe
                  AND nfe-relac-doc-nota-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item NO-LOCK :

                ASSIGN i-rel-doc = i-rel-doc + 1.
                       
                 
                FIND FIRST tt2-nfe-relac-doc-nota-rec
                    WHERE tt2-nfe-relac-doc-nota-rec.chave-acesso = nfe-relac-doc-nota-rec.chave-acesso
                      AND tt2-nfe-relac-doc-nota-rec.seq-item     = nfe-relac-doc-nota-rec.seq-item  NO-LOCK NO-ERROR.
                IF NOT AVAIL tt2-nfe-relac-doc-nota-rec THEN DO:
                    CREATE tt2-nfe-relac-doc-nota-rec.         
                    ASSIGN tt2-nfe-relac-doc-nota-rec.chave-acesso = nfe-relac-doc-nota-rec.chave-acesso
                           tt2-nfe-relac-doc-nota-rec.seq-item     = nfe-relac-doc-nota-rec.seq-item  
                           tt2-nfe-relac-doc-nota-rec.i-rel-doc    = i-rel-doc.
                END.

                ASSIGN tt2-nfe-relac-doc-nota-rec.i-rel-doc    = i-rel-doc.
                       
                ASSIGN tt-item-doc-est.despesas[1] = (bf-nfe-it-nota-fisc-rec.item-vSeg + bf-nfe-it-nota-fisc-rec.item-vFrete + bf-nfe-it-nota-fisc-rec.item-vOutro) / tt2-nfe-relac-doc-nota-rec.i-rel-doc.

                ASSIGN de-vOutro = (de-vOutro   + bf-nfe-it-nota-fisc-rec.item-vOutro) / i-rel-doc.
            END.
        END.
        
        IF bf-nfe-it-nota-fisc-rec.item-vFrete <> 0 THEN
            ASSIGN tt-item-doc-est.pr-total-cmi = bf-nfe-it-nota-fisc-rec.item-vFrete.

        FIND FIRST natur-oper WHERE 
                   natur-oper.nat-operacao = bf-nfe-it-nota-fisc-rec.item-nat-operacao NO-LOCK NO-ERROR.

        IF AVAIL natur-oper THEN DO:
            /* --- ICM (ind-icm-ret) --- */
            IF natur-oper.subs-trib = YES THEN DO:
                &IF '{&bf_dis_versao_ems}' >= '2.06' &THEN 
                    ASSIGN tt-item-doc-est.log-icm-retido  = YES
                           tt-item-doc-est.log-2           = YES. /*NO EMS >= 2.06 ele utiliza os dois campos - V05 R00*/
                &ELSE 
                    ASSIGN tt-item-doc-est.log-2           = YES.
                &ENDIF                           
            END.
            ELSE DO:
                
                &IF '{&bf_dis_versao_ems}' >= '2.06' &THEN 
                    ASSIGN tt-item-doc-est.log-icm-retido  = NO
                           tt-item-doc-est.log-2           = NO. /*NO EMS >= 2.06 ele utiliza os dois campos - V05 R00*/
                &ELSE 
                    ASSIGN tt-item-doc-est.log-2           = NO.
                &ENDIF
            END.
                
            FIND FIRST ITEM WHERE 
                       ITEM.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.

            IF AVAIL ITEM THEN DO:
                IF AVAIL nfe-it-imposto-alt THEN
                    ASSIGN tt-item-doc-est.cd-trib-icm = nfe-it-imposto-alt.icms-tribut.
                ELSE DO:
                    
                    CASE natur-oper.cd-trib-icm :
                        /* --- Tributado --- */
    
                        WHEN 1 THEN DO:
                            
                            IF ITEM.cd-trib-icm = 1 OR                      /* --- 1 - Tributado    --- */
                               ITEM.cd-trib-icm = 4 THEN                    /* --- 4 - Reduzido     --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 1.     /* --- Tributado        --- */
    
                            IF ITEM.cd-trib-icm = 3 THEN                    /* --- 3 - Outros       --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 3.     /* --- Outros           --- */
    
                            IF ITEM.cd-trib-icm = 5 THEN                    /* --- 5 - Diferido     --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 5.     /* --- Diferido         --- */
    
                            IF ITEM.cd-trib-icm = 2 THEN                    /* --- 2 - Isento       --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 2.     /* --- Isento           --- */
                        END.
    
                        /* --- Isento --- */
    
                        WHEN 2 THEN DO:
                            IF ITEM.cd-trib-icm = 1 OR                      /* --- 1 - Tributado    --- */
                               ITEM.cd-trib-icm = 4 OR                      /* --- 4 - Reduzido     --- */
                               ITEM.cd-trib-icm = 3 OR                      /* --- 3 - Outros       --- */
                               ITEM.cd-trib-icm = 2 THEN                    /* --- 2 - Isento       --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 2.     /* --- Isento           --- */
                        END.
    
                        /* --- Outros --- */
    
                        WHEN 3 THEN 
                        DO:
                            IF ITEM.cd-trib-icm = 1 OR                      /* --- 1 - Tributado    --- */
                               ITEM.cd-trib-icm = 4 OR                      /* --- 4 - Reduzido     --- */
                               ITEM.cd-trib-icm = 3 OR                      /* --- 3 - Outros       --- */
                               ITEM.cd-trib-icm = 2 THEN                    /* --- 2 - Isento       --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 3.     /* --- Outros           --- */
                        END.
    
                        /* --- Reduzido --- */
    
                        WHEN 4 THEN 
                        DO:
                            IF ITEM.cd-trib-icm = 1 THEN                    /* --- 1 - Tributado    --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 1.     /* --- Tributado        --- */
    
                            IF ITEM.cd-trib-icm = 4 THEN                    /* --- 4 - Reduzido     --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 4.     /* --- Reduzido         --- */
    
                            IF ITEM.cd-trib-icm = 3 THEN                    /* --- 3 - Outros       --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 3.     /* --- Outros           --- */
    
                            IF ITEM.cd-trib-icm = 2 THEN                    /* --- 2 - Isento       --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 2.     /* --- Isento           --- */
                        END.
    
                        /* --- Diferido --- */
    
                        WHEN 5 THEN 
                        DO:
                            IF ITEM.cd-trib-icm = 1 OR                      /* --- 1 - Tributado    --- */
                               ITEM.cd-trib-icm = 4 OR                      /* --- 4 - Reduzido     --- */
                               ITEM.cd-trib-icm = 3 OR                      /* --- 3 - Outros       --- */
                               ITEM.cd-trib-icm = 2 THEN                    /* --- 2 - Isento       --- */
                                ASSIGN tt-item-doc-est.cd-trib-icm = 5.     /* --- Diferido         --- */
                        END.
    
                    END CASE.
                END.
                

                /* --- IPI --- */          
                CASE natur-oper.cd-trib-ipi :
                    /* --- Tributado --- */
                    WHEN 1 THEN DO:
                        IF ITEM.cd-trib-ipi = 1 OR                      /* --- 1 - Tributado    --- */
                           ITEM.cd-trib-ipi = 4 THEN                    /* --- 4 - Reduzido     --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 1.     /* --- Tributado        --- */

                        IF ITEM.cd-trib-ipi = 3 THEN                    /* --- 3 - Outros       --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 3.     /* --- Outros           --- */

                        IF ITEM.cd-trib-ipi = 5 THEN                    /* --- 5 - Diferido     --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 5.     /* --- Diferido         --- */

                        IF ITEM.cd-trib-ipi = 2 THEN                    /* --- 2 - Isento       --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 2.     /* --- Isento           --- */
                    END.

                    /* --- Isento --- */

                    WHEN 2 THEN 
                    DO:
                        IF ITEM.cd-trib-ipi = 1 OR                      /* --- 1 - Tributado    --- */
                           ITEM.cd-trib-ipi = 4 OR                      /* --- 4 - Reduzido     --- */
                           ITEM.cd-trib-ipi = 3 OR                      /* --- 3 - Outros       --- */
                           ITEM.cd-trib-ipi = 2 THEN                    /* --- 2 - Isento       --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 2.     /* --- Isento           --- */
                    END.

                    /* --- Outros --- */

                    WHEN 3 THEN 
                    DO:
                        IF ITEM.cd-trib-ipi = 1 OR                      /* --- 1 - Tributado    --- */
                           ITEM.cd-trib-ipi = 4 OR                      /* --- 4 - Reduzido     --- */
                           ITEM.cd-trib-ipi = 3 OR                      /* --- 3 - Outros       --- */
                           ITEM.cd-trib-ipi = 2 THEN                    /* --- 2 - Isento       --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 3.     /* --- Outros           --- */
                    END.

                    /* --- Reduzido --- */

                    WHEN 4 THEN 
                    DO:
                        IF ITEM.cd-trib-ipi = 1 THEN                    /* --- 1 - Tributado    --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 1.     /* --- Tributado        --- */

                        IF ITEM.cd-trib-ipi = 4 THEN                    /* --- 4 - Reduzido     --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 4.     /* --- Reduzido         --- */

                        IF ITEM.cd-trib-ipi = 3 THEN                    /* --- 3 - Outros       --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 3.     /* --- Outros           --- */

                        IF ITEM.cd-trib-ipi = 2 THEN                    /* --- 2 - Isento       --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 2.     /* --- Isento           --- */
                    END.

                    /* --- Diferido --- */

                    WHEN 5 THEN 
                    DO:
                        IF ITEM.cd-trib-ipi = 1 OR                      /* --- 1 - Tributado    --- */
                           ITEM.cd-trib-ipi = 4 OR                      /* --- 4 - Reduzido     --- */
                           ITEM.cd-trib-ipi = 3 OR                      /* --- 3 - Outros       --- */
                           ITEM.cd-trib-ipi = 2 THEN                    /* --- 2 - Isento       --- */
                            ASSIGN tt-item-doc-est.cd-trib-ipi = 5.     /* --- Diferido         --- */
                    END.
                END CASE.
            END. /* IF AVAIL ITEM THEN..... */

            /* --- Item de Servi?o --- */
            ASSIGN tt-item-doc-est.cd-trib-iss = natur-oper.cd-trib-iss.

            /* --- Busca Relacionamento Emitente x CFOP x Natureza e Verifica de onde Buscar a Aliquota de ICMS --- */
            FIND FIRST nfe-emit-cfop-nat WHERE 
                       nfe-emit-cfop-nat.cod-estabel  = bfu-nfe-nota-fiscal-rec.cod-estabel        AND 
                       nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                     AND 
                       nfe-emit-cfop-nat.cod-cfop     = STRING(bf-nfe-it-nota-fisc-rec.item-CFOP) AND 
                       nfe-emit-cfop-nat.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo         NO-LOCK NO-ERROR.

            IF NOT AVAIL nfe-emit-cfop-nat THEN DO:
                FIND FIRST nfe-emit-cfop-nat WHERE 
                           nfe-emit-cfop-nat.cod-estabel  = bfu-nfe-nota-fiscal-rec.cod-estabel        AND 
                           nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                     AND 
                           nfe-emit-cfop-nat.cod-cfop     = STRING(bf-nfe-it-nota-fisc-rec.item-CFOP) AND 
                           nfe-emit-cfop-nat.it-codigo    = "*"                                       NO-LOCK NO-ERROR.
            END.                                   

            IF AVAIL nfe-emit-cfop-nat THEN DO: /* --- Origem Natureza de Operacao --- */
                FIND FIRST estabelec NO-LOCK
                    WHERE estabelec.cod-estabel = bfu-nfe-nota-fiscal-rec.cod-estabel NO-ERROR.
                    
                FIND FIRST bf-emitente NO-LOCK
                    WHERE bf-emitente.cod-emitente = estabelec.cod-emitente NO-ERROR.

                IF i-tipo-nfe = 2 THEN DO: /*devolucao*/
                    /*ICMS Devolucao*/
                    IF l-ICMS-dev-xml = NO THEN DO:
                        RUN pi_aliquota_icm(INPUT bf-emitente.contrib-icms     ,     
                                            INPUT bf-emitente.natureza         ,     
                                            INPUT emitente.estado              ,
                                            INPUT emitente.pais                , 
                                            INPUT estabelec.estado             ,
                                            INPUT tt-item-doc-est.it-codigo    ,            
                                            INPUT IF l-multi_natureza THEN tt-item-doc-est.nat-of ELSE tt-item-doc-est.nat-operacao ,
                                            OUTPUT tt-item-doc-est.aliquota-icm).                                                    


                        IF bfu-nfe-nota-fiscal-rec.emit-crt = 1 THEN
                            ASSIGN tt-item-doc-est.aliquota-icm = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.icms-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-pCredSN.

                    END.
                 
                    /*IPI Devolucao*/
                    /*RA nÆo faz calculo de imposto*/
                    /*IF bf-nfe-it-nota-fisc-rec.imp-ipi-vBC = 0 THEN 
                        ASSIGN de-base-calculo = bf-nfe-it-nota-fisc-rec.item-vProd.
                    ELSE 
                        ASSIGN de-base-calculo = bf-nfe-it-nota-fisc-rec.imp-ipi-vBC.
            
                    IF tt-item-doc-est.cd-trib-ipi   = 1 AND
                       tt-item-doc-est.aliquota-ipi  = 0 THEN 
                        ASSIGN tt-item-doc-est.ipi-outras[1] = de-base-calculo.
            
                    IF tt-item-doc-est.cd-trib-ipi   = 4 AND
                       tt-item-doc-est.aliquota-ipi  = 0 THEN 
                        ASSIGN tt-item-doc-est.ipi-outras[1] = de-base-calculo.
            
                    IF tt-item-doc-est.cd-trib-ipi   = 2 THEN
                        ASSIGN tt-item-doc-est.ipi-ntrib[1]  = de-base-calculo.
                       
                    IF tt-item-doc-est.cd-trib-ipi   = 3 THEN 
                        ASSIGN tt-item-doc-est.ipi-outras[1] = de-base-calculo.

                    ASSIGN tt-item-doc-est.base-ipi[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE de-base-calculo.*/ 
                    IF l-IPI-dev-xml THEN 
                        ASSIGN tt-item-doc-est.aliquota-IPI = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-ipi-pIPI
                               tt-item-doc-est.valor-IPI[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-valor ELSE bf-nfe-it-nota-fisc-rec.imp-ipi-vIPI.
                                                                                                                                 
                    ELSE DO: /*Classificacao Fiscal ou item*/
                        FIND FIRST ITEM NO-LOCK
                             WHERE ITEM.it-codigo = tt-item-doc-est.it-codigo NO-ERROR.
                             
                        FIND FIRST classif-fisc NO-LOCK
                             WHERE classif-fisc.class-fiscal = bf-nfe-it-nota-fisc-rec.item-NCM NO-ERROR.
                             
                        FIND FIRST param-re NO-LOCK
                            WHERE param-re.usuario = c-seg-usuario NO-ERROR.

                        IF AVAIL param-re AND param-re.orig-aliq-ip  = 1 AND AVAIL classif-fisc THEN
                            assign tt-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE classif-fisc.aliquota-ipi.
                        else
                            assign tt-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE item.aliquota-ipi.
                    END.
                END.
                
                /*Outras notas diferentes de Devolucao*/
                ELSE DO:

                    /*ICMS Compras*/
                    IF l-ICMS-xml = NO THEN DO:
                        
                        RUN pi_aliquota_icm(INPUT bf-emitente.contrib-icms     ,     
                                            INPUT bf-emitente.natureza         ,     
                                            INPUT emitente.estado              ,
                                            INPUT emitente.pais                , 
                                            INPUT estabelec.estado             ,
                                            INPUT tt-item-doc-est.it-codigo    ,            
                                            INPUT IF l-multi_natureza THEN tt-item-doc-est.nat-of ELSE tt-item-doc-est.nat-operacao ,
                                            OUTPUT tt-item-doc-est.aliquota-icm).
                                                                                                       

                        IF bfu-nfe-nota-fiscal-rec.emit-crt = 1 THEN
                            ASSIGN tt-item-doc-est.aliquota-icm = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.icms-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-pCredSN.

                                                                                                                                     
                    END.

                    /*IPI Compras*/
                    /*RA nÆo faz calculo de impostos*/
                    /*IF bf-nfe-it-nota-fisc-rec.imp-ipi-vBC = 0 THEN DO:
                        ASSIGN de-base-calculo = bf-nfe-it-nota-fisc-rec.item-vProd.
                        
                    END.
                    ELSE DO: 
                        ASSIGN de-base-calculo = bf-nfe-it-nota-fisc-rec.imp-ipi-vBC.
                        
                    END.
            
                    
                    IF tt-item-doc-est.cd-trib-ipi   = 1 AND
                       tt-item-doc-est.aliquota-ipi  = 0 THEN 
                        ASSIGN tt-item-doc-est.ipi-outras[1] = de-base-calculo.
            
                    IF tt-item-doc-est.cd-trib-ipi   = 4 AND
                       tt-item-doc-est.aliquota-ipi  = 0 THEN 
                        ASSIGN tt-item-doc-est.ipi-outras[1] = de-base-calculo.
            
                    IF tt-item-doc-est.cd-trib-ipi   = 2 THEN
                        ASSIGN tt-item-doc-est.ipi-ntrib[1]  = de-base-calculo.
                       
                    IF tt-item-doc-est.cd-trib-ipi   = 3 THEN 
                        ASSIGN tt-item-doc-est.ipi-outras[1] = de-base-calculo.

                    ASSIGN tt-item-doc-est.base-ipi[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE de-base-calculo.*/

                    IF l-IPI-xml THEN 
                        ASSIGN tt-item-doc-est.aliquota-IPI = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE bf-nfe-it-nota-fisc-rec.imp-ipi-pIPI
                               tt-item-doc-est.valor-IPI[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-valor ELSE bf-nfe-it-nota-fisc-rec.imp-ipi-vIPI.
                                                                                                                                 
                    ELSE DO: /*Classificacao Fiscal ou item*/
                        FIND FIRST ITEM NO-LOCK
                             WHERE ITEM.it-codigo = tt-item-doc-est.it-codigo NO-ERROR.
                
                        FIND FIRST classif-fisc NO-LOCK
                            WHERE classif-fisc.class-fiscal = bf-nfe-it-nota-fisc-rec.item-NCM NO-ERROR.

                        FIND FIRST param-re NO-LOCK
                            WHERE param-re.usuario = c-seg-usuario NO-ERROR.    
                        
                        IF AVAIL param-re AND param-re.orig-aliq-ip  = 1
                            AND avail classif-fisc THEN /*Classificacao*/
                            assign tt-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE classif-fisc.aliquota-ipi.
                        else /*Item*/
                            assign tt-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE item.aliquota-ipi.
                    END.
                END.
            END.     
        END. /* IF AVAIL natur-oper..... */
        
        /* --- Cria tabela Auxiliar (Seq Original) --- */
        CREATE tt-item-doc-est-aux.

        ASSIGN tt-item-doc-est-aux.chave-acesso-nfe = bf-nfe-it-nota-fisc-rec.chave-acesso-nfe
               tt-item-doc-est-aux.seq-item         = 10 * bf-nfe-it-nota-fisc-rec.seq-item
               tt-item-doc-est-aux.sequencia        = tt-item-doc-est.sequencia
               tt-item-doc-est-aux.nat-operacao     = IF l-multi_natureza THEN tt-item-doc-est.nat-of ELSE tt-item-doc-est.nat-operacao.
    END. /* IF AVAIL bf-nfe-it-nota-fisc-rec..... */

    /* F?bio Guedes - 27/10/2011 - Versao: 1.04.023
       --- Recalcula Imposto da Nota no Recebimento Fiscal --- */
    RUN pi_recalcula_imposto (INPUT TABLE tt-item-doc-est, 
                                  OUTPUT p_mensagem_erro).   
RUN pi-finalizar IN h-acomp-5.   
END PROCEDURE. /* pi_item_receb_fiscal */

/**************************************************************************************************************************/

PROCEDURE pi_verifica_relac_item:
    
    DEF INPUT  PARAM p_rowid_item     AS ROWID       NO-UNDO.
    DEF OUTPUT PARAM p_l_ordem        AS LOG INIT NO NO-UNDO.
    DEF OUTPUT PARAM p_l_ord_ser_lote AS LOG INIT NO NO-UNDO.
    DEF OUTPUT PARAM p_l_ser_lote     AS LOG INIT NO NO-UNDO.
    DEF OUTPUT PARAM p_l_doc_nota     AS LOG INIT NO NO-UNDO.
    DEF OUTPUT PARAM p_l_doc_ser_lote AS LOG INIT NO NO-UNDO.

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-LOCK NO-ERROR.

    IF AVAIL bf-nfe-it-nota-fisc-rec THEN 
    DO:
        IF CAN-FIND(FIRST nfe-relac-ordem-rec WHERE 
                          nfe-relac-ordem-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                          nfe-relac-ordem-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK) THEN 
        DO:
            IF CAN-FIND(FIRST nfe-relac-ser-lote-rec WHERE 
                              nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                              nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK) THEN 
                ASSIGN p_l_ord_ser_lote = YES.
            ELSE 
                ASSIGN p_l_ordem        = YES.
        END.
        ELSE 
        DO:
            IF CAN-FIND(FIRST nfe-relac-ser-lote-rec WHERE 
                              nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                              nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK) THEN 
                ASSIGN p_l_ser_lote = YES.
        END.

        IF CAN-FIND(FIRST nfe-relac-doc-nota-rec WHERE 
                          nfe-relac-doc-nota-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                          nfe-relac-doc-nota-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK) THEN 
        DO:
            IF CAN-FIND(FIRST nfe-relac-ser-lote-rec WHERE 
                              nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                              nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK) THEN 
                ASSIGN p_l_doc_ser_lote = YES
                       p_l_ser_lote     = NO.
            ELSE 
                ASSIGN p_l_doc_nota     = YES
                       p_l_ser_lote     = NO.
        END.
    END.

END PROCEDURE. /* pi_verifica_relac_item */

/**************************************************************************************************************************/

PROCEDURE pi_nota_receb_fisico:

    DEF INPUT PARAM p_rowid_nota        AS ROWID                           NO-UNDO.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-doc-fisico.
    DEF OUTPUT PARAM p_mensagem_erro    AS CHAR FORMAT "x(200)" INITIAL "" NO-UNDO.

    FIND FIRST bfu-nfe-nota-fiscal-rec WHERE 
               ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_nota NO-LOCK NO-ERROR.

    
    

    EMPTY TEMP-TABLE tt-doc-fisico.

    IF AVAIL bfu-nfe-nota-fiscal-rec THEN 
    DO:
        FIND emitente WHERE 
             emitente.nome-abrev = tt-nfe-nota-fiscal-rec-fisico.nome-abrev NO-LOCK NO-ERROR.

        /* --- Documento --- */

        CREATE tt-doc-fisico.

        ASSIGN tt-doc-fisico.nro-docto    = tt-nfe-nota-fiscal-rec-fisico.ide-nNF
               tt-doc-fisico.serie-docto  = STRING(tt-nfe-nota-fiscal-rec-fisico.ide-Serie)
               tt-doc-fisico.cod-emitente = emitente.cod-emitente
               tt-doc-fisico.cod-estabel  = tt-nfe-nota-fiscal-rec-fisico.cod-estabel
               tt-doc-fisico.dt-emissao   = DATE(INTE(SUBSTR(tt-nfe-nota-fiscal-rec-fisico.ide-dEmi,5,2)),  
                                                 INTE(SUBSTR(tt-nfe-nota-fiscal-rec-fisico.ide-dEmi,7,2)),  
                                                 INTE(SUBSTR(tt-nfe-nota-fiscal-rec-fisico.ide-dEmi,1,4))) 
               tt-doc-fisico.dt-trans     = tt-nfe-nota-fiscal-rec-fisico.ide-dTrans
               tt-doc-fisico.dt-atualiza  = TODAY
               tt-doc-fisico.situacao     = 1 /* --- 1 - Nao Atualizado, 2 - Atualizado, 3 - Dig Receb, 4 - Atual Receb --- */
               tt-doc-fisico.usuario      = c-seg-usuario.

        FIND FIRST b-emitente 
            WHERE b-emitente.cod-emitente = tt-doc-fisico.cod-emitente NO-LOCK NO-ERROR.

        FIND FIRST b-estabelec
            WHERE b-estabelec.cgc = b-emitente.cgc NO-LOCK NO-ERROR.

        IF AVAIL b-estabelec THEN 
               ASSIGN tt-doc-fisico.estab-de-or      = b-estabelec.cod-estabel. 
        
        /* --- Verifica Se Importa Observacao da Nota --- */

        FIND FIRST nfe-param-rec WHERE 
                   nfe-param-rec.cod-parametro = "param_global" NO-LOCK NO-ERROR.

        IF AVAIL nfe-param-rec THEN 
        DO:
            FIND FIRST nfe-it-param-rec WHERE 
                       nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                       nfe-it-param-rec.cod-item-parametro = "importa_observacao"        NO-LOCK NO-ERROR.

            IF AVAIL nfe-it-param-rec                          AND
               nfe-it-param-rec.valor-1-item-parametro = "SIM" THEN 
                ASSIGN tt-doc-fisico.observacao = tt-nfe-nota-fiscal-rec-fisico.inf-infCpl.
        END.

        IF tt-nfe-nota-fiscal-rec-fisico.tipo-nota = 0 THEN
        DO:
            ASSIGN p_mensagem_erro = "1,"                                     +
                                     "Natureza de operacao nao encontrada !," +
                                     "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_NOTA_RECEB_FISICO)".

            RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                  OUTPUT i-acao).
        END.

        

        ASSIGN tt-doc-fisico.tipo-nota = tt-nfe-nota-fiscal-rec-fisico.tipo-nota.

        /* --- 1 - Compra, 2 - Devolucao, 3 - Transferencia --- */

        /* Retirado para dar lugar a rotina acima
        
        IF bf-nfe-it-nota-fisc-rec.item-esp-nat = "NFE" THEN ASSIGN tt-doc-fisico.tipo-nota = 1.

        IF bf-nfe-it-nota-fisc-rec.item-esp-nat = "NFD" THEN ASSIGN tt-doc-fisico.tipo-nota = 2.
        */

        /* F?bio Guedes - Fim - Versao : 1.04.022 */

        /* --- Total dos Produtos para Notas (Valor Duplicata) --- */

        DEFINE VARIABLE l-preco-bruto AS LOGICAL     NO-UNDO.
        DEFINE VARIABLE de-desc-ordem AS DECIMAL     NO-UNDO.
        DEFINE VARIABLE de-desc-ordem-total AS DECIMAL     NO-UNDO.

        ASSIGN l-preco-bruto = NO.

        FIND FIRST nfe-it-param-rec NO-LOCK    
            WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
            AND   nfe-it-param-rec.cod-item-parametro     = 'soma_desconto_preco' 
            AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
                
        IF AVAIL nfe-it-param-rec  THEN DO:

            ASSIGN l-preco-bruto = YES.

        END.

        ASSIGN tt-doc-fisico.valor-mercad = 0.

        
        FOR EACH tt-nfe-it-nota-fisc-rec-fisico
            WHERE tt-nfe-it-nota-fisc-rec-fisico.chave-acesso-nfe = tt-nfe-nota-fiscal-rec-fisico.chave-acesso-nfe
            AND   tt-nfe-it-nota-fisc-rec-fisico.tipo-nota        = tt-nfe-nota-fiscal-rec-fisico.tipo-nota:

            ASSIGN de-desc-ordem = 0.
            IF l-preco-bruto = YES THEN DO:

                FIND FIRST ordem-compra NO-LOCK
                    WHERE ordem-compra.numero-ordem = tt-nfe-it-nota-fisc-rec-fisico.item-num-ordem NO-ERROR.

                IF AVAIL ordem-compra THEN DO:

                    ASSIGN de-desc-ordem = ordem-compra.valor-descto.

                END.
                
                ASSIGN tt-doc-fisico.valor-mercad = tt-doc-fisico.valor-mercad + tt-nfe-it-nota-fisc-rec-fisico.item-vProd + (de-desc-ordem * tt-nfe-it-nota-fisc-rec-fisico.item-qtde)
                       tt-doc-fisico.tot-desconto = tt-doc-fisico.tot-desconto + (de-desc-ordem * tt-nfe-it-nota-fisc-rec-fisico.item-qtde).

            END.
            ELSE
                ASSIGN tt-doc-fisico.valor-mercad = tt-doc-fisico.valor-mercad + tt-nfe-it-nota-fisc-rec-fisico.item-vProd.

                
        END.      


        /* --- Frete, Despesas e Desconto --- */

        IF de-vFrete <> 0 THEN
            ASSIGN tt-doc-fisico.valor-frete = de-vFrete.

        IF de-vSeg <> 0 THEN
            ASSIGN tt-doc-fisico.valor-seguro = de-vSeg.

        IF de-vOutro <> 0 THEN
            ASSIGN tt-doc-fisico.valor-outras = de-vOutro.

        IF de-vFrete <> 0 OR
           de-vSeg   <> 0 OR 
           de-vOutro <> 0 THEN
            ASSIGN tt-doc-fisico.despesa-nota = (de-vFrete + de-vSeg + de-vOutro).

        IF de-vDesc <> 0 THEN
            ASSIGN tt-doc-fisico.tot-desconto = de-vDesc.
    END. /* IF AVAIL bfu-nfe-nota-fiscal-rec.... */

END PROCEDURE. /* pi_nota_receb_fisico */

/**************************************************************************************************************************/

PROCEDURE pi_item_receb_fisico:

    DEF INPUT  PARAM p_rowid_item            AS   ROWID                                      NO-UNDO.
    DEF INPUT  PARAM p_sequencia             AS   INTE                                       NO-UNDO.
    DEF INPUT  PARAM p_item_xPed             LIKE nfe-it-nota-fisc-rec.item-xPed             NO-UNDO.
    DEF INPUT  PARAM p_item_num_ordem        LIKE nfe-it-nota-fisc-rec.item-num-ordem        NO-UNDO.
    DEF INPUT  PARAM p_item_parcela_oc       LIKE nfe-it-nota-fisc-rec.item-parcela-oc       NO-UNDO.
    DEF INPUT  PARAM p_item_qtde             LIKE nfe-it-nota-fisc-rec.item-qtde             NO-UNDO.
    DEF INPUT  PARAM p_item_qCom             LIKE nfe-it-nota-fisc-rec.item-qCom             NO-UNDO.
    DEF INPUT  PARAM p_item_vProd            LIKE nfe-it-nota-fisc-rec.item-vProd            NO-UNDO.
    DEF INPUT  PARAM p_item_vUnCom           LIKE nfe-it-nota-fisc-rec.item-vUnCom           NO-UNDO.
    DEF INPUT  PARAM p_item_conta_contabil   LIKE nfe-it-nota-fisc-rec.item-conta-contabil   NO-UNDO.
    DEF INPUT  PARAM p_item_nr_ord_produ     LIKE nfe-it-nota-fisc-rec.item-nr-ord-produ     NO-UNDO.
    DEF INPUT  PARAM p_item_cod_depos        LIKE nfe-it-nota-fisc-rec.item-cod-depos        NO-UNDO.
    DEF INPUT  PARAM p_item_cod_localiz      LIKE nfe-it-nota-fisc-rec.item-cod-localiz      NO-UNDO.
    DEF INPUT  PARAM p_item_ser_lote         LIKE nfe-it-nota-fisc-rec.item-ser-lote         NO-UNDO.
    DEF INPUT  PARAM p_item_dt_vali_lote     LIKE nfe-it-nota-fisc-rec.item-dt-vali-lote     NO-UNDO.
    DEF INPUT  PARAM p_item_cod_refer        LIKE nfe-it-nota-fisc-rec.item-cod-refer        NO-UNDO.
    DEF INPUT  PARAM p_item_serie_comp       LIKE nfe-it-nota-fisc-rec.item-serie-comp       NO-UNDO.
    DEF INPUT  PARAM p_item_nro_comp         LIKE nfe-it-nota-fisc-rec.item-nro-comp         NO-UNDO.
    DEF INPUT  PARAM p_item_nat_comp         LIKE nfe-it-nota-fisc-rec.item-nat-comp         NO-UNDO.
    DEF INPUT  PARAM p_item_seq_comp         LIKE nfe-it-nota-fisc-rec.item-seq-comp         NO-UNDO.
    DEF INPUT  PARAM p_item_data_comp        LIKE nfe-it-nota-fisc-rec.item-data-comp        NO-UNDO.
    DEF INPUT  PARAM p_preco-bruto           AS LOG                                          NO-UNDO.
    DEF OUTPUT PARAM p_seq_it_docum_est      LIKE it-doc-fisico.sequencia                    NO-UNDO.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-it-doc-fisico.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-rat-lote.
    DEF OUTPUT PARAM p_mensagem_erro         AS   CHAR FORMAT "x(200)" INITIAL ""            NO-UNDO.

    DEF VAR h-it-doc-fisico AS   HANDLE                         NO-UNDO.
    DEF VAR c-un            LIKE nfe-it-nota-fisc-rec.item-uCom NO-UNDO.
    DEF VAR i-tipo-nfe      AS   INT                            NO-UNDO.

    
   DEFINE VARIABLE c-cod-unid-negoc-aux AS CHARACTER   NO-UNDO.

    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-LOCK NO-ERROR.

    IF AVAIL bf-nfe-it-nota-fisc-rec THEN 
    DO:
        FIND FIRST natur-oper NO-LOCK
             WHERE natur-oper.nat-operacao = bf-nfe-it-nota-fisc-rec.item-nat-operacao NO-ERROR.

        FIND FIRST bfu-nfe-nota-fiscal-rec OF bf-nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.

        FIND emitente WHERE 
             emitente.nome-abrev = bfu-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

        /*** MOD.01 - Baione - 07/06/2011 ***/

        IF AVAIL emitente THEN 
        DO:
            FIND FIRST item-fornec WHERE 
                       item-fornec.cod-emitente = emitente.cod-emitente             AND 
                       item-fornec.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.

            IF AVAIL item-fornec THEN 
            DO:
                IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN 
                DO:
                    CREATE nfe-item-fornec.

                    BUFFER-COPY item-fornec TO nfe-item-fornec. 
                END.

                FIND FIRST nfe-item-fornec OF item-fornec NO-LOCK NO-ERROR.

                ASSIGN c-un = item-fornec.unid-med-for.

                IF item-fornec.fator-conver <> nfe-item-fornec.fator-conver OR
                   item-fornec.num-casa-dec <> nfe-item-fornec.num-casa-dec OR
                   item-fornec.unid-med-for <> nfe-item-fornec.unid-med-for  THEN 
                DO:
                    
                    FIND FIRST ITEM WHERE 
                               ITEM.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.
                    
                    ASSIGN de-fat-conv   = item-fornec.fator-conver * EXP(10,(item-fornec.num-casa-dec * (-1)))
                           p_item_qCom   = p_item_qtde * de-fat-conv
                           p_item_vUnCom = p_item_vProd / p_item_qtde.
/*                            c-un          = IF AVAIL ITEM THEN ITEM.un ELSE bf-nfe-it-nota-fisc-rec.item-uCom. */
                                                                           
                END.
/*                 ELSE                                                 */
/*                     ASSIGN c-un = bf-nfe-it-nota-fisc-rec.item-uCom. */


                    
            END.
        END.

        IF c-un = '' THEN 
            ASSIGN c-un  =  bf-nfe-it-nota-fisc-rec.item-uCom.

        EMPTY TEMP-TABLE tt-it-doc-fisico.

        EMPTY TEMP-TABLE tt-mensagem-erro.
        
        CREATE tt-it-doc-fisico.

        ASSIGN tt-it-doc-fisico.nro-docto      = bfu-nfe-nota-fiscal-rec.ide-nNF
               tt-it-doc-fisico.serie-docto    = STRING(bfu-nfe-nota-fiscal-rec.ide-Serie)
               tt-it-doc-fisico.cod-emitente   = emitente.cod-emitente
               tt-it-doc-fisico.sequencia      = p_sequencia
               tt-it-doc-fisico.it-codigo      = bf-nfe-it-nota-fisc-rec.it-codigo
               tt-it-doc-fisico.narrativa      = IF bf-nfe-it-nota-fisc-rec.narrativa = "" THEN bf-nfe-it-nota-fisc-rec.item-xprod ELSE bf-nfe-it-nota-fisc-rec.narrativa
               tt-it-doc-fisico.quantidade     = p_item_qtde
               tt-it-doc-fisico.qt-do-forn     = p_item_qCom
               tt-it-doc-fisico.quant-conf     = 0
               tt-it-doc-fisico.preco-unit[1]  = p_item_vUnCom
               tt-it-doc-fisico.preco-total[1] = p_item_vProd
               tt-it-doc-fisico.quant-devol    = 0
               tt-it-doc-fisico.valor          = p_item_vProd 
               tt-it-doc-fisico.cod-depos      = p_item_cod_depos
               tt-it-doc-fisico.cod-localiz    = p_item_cod_localiz
               tt-it-doc-fisico.cod-refer      = p_item_cod_refer
               tt-it-doc-fisico.lote           = p_item_ser_lote
               tt-it-doc-fisico.un             = c-un
               tt-it-doc-fisico.dt-vali-lote   = p_item_dt_vali_lote
               tt-it-doc-fisico.dt-nota-dev    = ?
               /*tt-it-doc-fisico.conta-contabil = p_item_conta_contabil*/
               tt-it-doc-fisico.baixa-ce       = YES /*natur-oper.baixa-estoq */
               tt-it-doc-fisico.num-pedido     = p_item_xPed
               tt-it-doc-fisico.numero-ordem   = p_item_num_ordem
               tt-it-doc-fisico.parcela        = p_item_parcela_oc
               tt-it-doc-fisico.nr-ord-produ   = p_item_nr_ord_produ
               tt-it-doc-fisico.serie-comp     = p_item_serie_comp
               tt-it-doc-fisico.nro-comp       = p_item_nro_comp  
               tt-it-doc-fisico.nat-comp       = p_item_nat_comp  
               tt-it-doc-fisico.seq-comp       = p_item_seq_comp  
               tt-it-doc-fisico.data-comp      = p_item_data_comp.

        RUN pi_valida_tipo_nota_fisico (INPUT  nfe-it-nota-fisc-rec.item-nat-operacao,
                                        OUTPUT i-tipo-nfe).

        IF i-tipo-nfe = 3 THEN do: /*Transferencia*/
            
             FIND FIRST it-nota-fisc NO-LOCK
                 WHERE it-nota-fisc.cod-estabel              = tt-doc-fisico.estab-de-or
                   AND it-nota-fisc.serie                    = tt-doc-fisico.serie-docto
                   AND it-nota-fisc.nr-nota-fis              = tt-doc-fisico.nro-docto
                   AND it-nota-fisc.it-codigo                = tt-it-doc-fisico.it-codigo 
                   AND it-nota-fisc.nr-seq-fat  = p_sequencia NO-ERROR.
            IF AVAIL it-nota-fisc THEN DO:
                
                ASSIGN tt-it-doc-fisico.serie-comp     = it-nota-fisc.serie
                       tt-it-doc-fisico.nro-comp       = it-nota-fisc.nr-nota-fis  
                       tt-it-doc-fisico.nat-comp       = it-nota-fisc.nat-operacao 
                       tt-it-doc-fisico.seq-comp       = it-nota-fisc.nr-seq-fat.
            END.
        END.

        IF p_preco-bruto = YES THEN DO:

             FIND FIRST ordem-compra NO-LOCK
                WHERE ordem-compra.numero-ordem = p_item_num_ordem NO-ERROR.
            IF AVAIL ordem-compra THEN DO:

                ASSIGN tt-it-doc-fisico.preco-unit[1]  = tt-it-doc-fisico.preco-unit[1] + ordem-compra.valor-descto
                       tt-it-doc-fisico.preco-total[1] = tt-it-doc-fisico.preco-total[1] + (tt-it-doc-fisico.quantidade  * ordem-compra.valor-descto)
                       tt-it-doc-fisico.desconto       = tt-it-doc-fisico.quantidade * ordem-compra.valor-descto.
            END.
                
        END.
            
        FIND FIRST ITEM WHERE 
                   ITEM.it-codigo = tt-it-doc-fisico.it-codigo NO-LOCK NO-ERROR.

        IF AVAIL ITEM            AND
           ITEM.tipo-con-est = 3 THEN  /* --- Lote --- */
        DO:
            IF p_item_dt_vali_lote = ? THEN ASSIGN tt-it-doc-fisico.dt-vali-lote = 12.31.9999.
        END.

        
        /*Unidade de negocio*/
        
        /*Unificaîao de conceitos Totvs11 Conta Contabil*/
        &IF '{&bf_dis_versao_ems}' <= '2.07' &THEN 
           FIND FIRST nfe-nota-fiscal-rec NO-LOCK
               WHERE nfe-nota-fiscal-rec.chave-acesso-nfe = bf-nfe-it-nota-fisc-rec.chave-acesso-nfe NO-ERROR.

        
           RUN pi_unidade_negocio (INPUT  nfe-nota-fiscal-rec.cod-estabel,
                                   INPUT  tt-it-doc-fisico.it-codigo,
                                   INPUT  tt-it-doc-fisico.numero-ordem,  
                                   OUTPUT c-cod-unid-negoc-aux).



           ASSIGN tt-it-doc-fisico.cod-unid-negoc = c-cod-unid-negoc-aux.
           ASSIGN tt-it-doc-fisico.conta-contabil = p_item_conta_contabil.                  

        &ELSE
            IF AVAIL nfe-relac-ordem-rec THEN
               ASSIGN tt-it-doc-fisico.ct-codigo      = nfe-relac-ordem-rec.ct-codigo
                      tt-it-doc-fisico.sc-codigo      = nfe-relac-ordem-rec.sc-codigo
                      tt-it-doc-fisico.cod-unid-negoc = nfe-relac-ordem-rec.cod-unid-negoc.

            ELSE
               ASSIGN tt-it-doc-fisico.ct-codigo      = bf-nfe-it-nota-fisc-rec.ct-codigo
                      tt-it-doc-fisico.sc-codigo      = bf-nfe-it-nota-fisc-rec.sc-codigo
                      tt-it-doc-fisico.cod-unid-negoc = bf-nfe-it-nota-fisc-rec.cod-unid-negoc.
        &ENDIF



        /**/


        /* --- Retona o Tipo de Nota de acordo com a Natureza --- */

        RUN pi_valida_tipo_nota_fisico (INPUT  bf-nfe-it-nota-fisc-rec.item-nat-operacao,
                                        OUTPUT i-tipo-nfe).

        IF i-tipo-nfe = 0 THEN
        DO:
            ASSIGN p_mensagem_erro = "1,"                                     +
                                     "Natureza de operacao nao encontrada !," +
                                     "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_NOTA_RECEB_FISICO)".

            RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                  OUTPUT i-acao).
        END.

        ASSIGN tt-it-doc-fisico.tipo-nota = i-tipo-nfe.

        /* --- 1 - Compra, 2 - Devolucao, 3 - Transferencia --- */

        /* Versao: 1.04.022 - 25/10/2011 - F?bio Guedes
        
        IF bf-nfe-it-nota-fisc-rec.item-esp-nat = "NFE" THEN ASSIGN tt-it-doc-fisico.tipo-nota = 1.

        IF bf-nfe-it-nota-fisc-rec.item-esp-nat = "NFD" THEN ASSIGN tt-it-doc-fisico.tipo-nota = 2.
        */

        /* --- FIFO em Ordens de Compra --- */

        ASSIGN tt-it-doc-fisico.fifo-oc = bf-nfe-it-nota-fisc-rec.item-fifo-oc.

        /* --- Frete, Despesas e Desconto --- */

        IF bf-nfe-it-nota-fisc-rec.item-vDesc <> 0 THEN
            ASSIGN tt-it-doc-fisico.desconto[1] = bf-nfe-it-nota-fisc-rec.item-vDesc.

        IF bf-nfe-it-nota-fisc-rec.item-vSeg   <> 0 OR 
           bf-nfe-it-nota-fisc-rec.item-vFrete <> 0 OR
           bf-nfe-it-nota-fisc-rec.item-vOutro <> 0 THEN
            ASSIGN tt-it-doc-fisico.despesas = (bf-nfe-it-nota-fisc-rec.item-vSeg + bf-nfe-it-nota-fisc-rec.item-vFrete + bf-nfe-it-nota-fisc-rec.item-vOutro).

        IF bf-nfe-it-nota-fisc-rec.item-vFrete <> 0 THEN
            ASSIGN tt-it-doc-fisico.vl-frete = bf-nfe-it-nota-fisc-rec.item-vFrete.

        RUN pi_limpa_epc.

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "ItemAntesRecebFisico"
               tt-epc.cod-parameter = "tt-it-doc-fisico(HANDLE)"
               tt-epc.val-parameter = STRING(TEMP-TABLE tt-it-doc-fisico:HANDLE).

        CREATE tt-epc.

        ASSIGN tt-epc.cod-event     = "ItemAntesRecebFisico"
               tt-epc.cod-parameter = "bf-nfe-it-nota-fisc-rec(ROWID)"
               tt-epc.val-parameter = STRING(ROWID(bf-nfe-it-nota-fisc-rec)).

        {include\i-epc201.i "ItemAntesRecebFisico"}
            
        FIND FIRST bf-it-doc-fisico WHERE 
                   bf-it-doc-fisico.serie-docto     = tt-it-doc-fisico.serie-docto  AND 
                   bf-it-doc-fisico.nro-docto       = tt-it-doc-fisico.nro-docto    AND 
                   bf-it-doc-fisico.cod-emitente    = tt-it-doc-fisico.cod-emitente AND 
                   bf-it-doc-fisico.tipo-nota       = tt-it-doc-fisico.tipo-nota    AND 
                   bf-it-doc-fisico.sequencia       = tt-it-doc-fisico.sequencia    NO-LOCK NO-ERROR.

        IF NOT AVAIL bf-it-doc-fisico THEN 
        DO:
            CREATE bf-it-doc-fisico.

            BUFFER-COPY tt-it-doc-fisico TO bf-it-doc-fisico NO-ERROR.

            IF ERROR-STATUS:ERROR = YES THEN 
            DO:
                ASSIGN p_mensagem_erro = "1,"                                           +
                                         "Nao foi possÐvel criar a Sequencia : "        +
                                          STRING(tt-it-doc-fisico.sequencia) + ".,"     +
                                         "Verifique as informacoes do Item.".
            END.
            ELSE 
            DO:
                ASSIGN p_seq_it_docum_est = bf-it-doc-fisico.sequencia. 

                RELEASE bf-it-doc-fisico.
            END.
        END.
        ELSE 
        DO:
            ASSIGN p_mensagem_erro = "1,"                                           +
                                     "Nao foi possÐvel criar a Sequencia : "        +
                                      STRING(tt-it-doc-fisico.sequencia) + ".,"     +
                                     "J? existe ocorr?ncia para o Item.".
        END.

        IF TRIM(p_mensagem_erro) <> "" THEN LEAVE.
        ELSE 
        DO:
            /* --- Altera a Quantidade Alocada do Prazo de Compra --- */

            FIND FIRST prazo-compra WHERE 
                       prazo-compra.numero-ordem  = tt-it-doc-fisico.numero-ordem AND 
                       prazo-compra.parcela       = tt-it-doc-fisico.parcela      EXCLUSIVE-LOCK NO-ERROR.

            IF AVAIL prazo-compra THEN 
                ASSIGN prazo-compra.dec-1 = prazo-compra.dec-1 + tt-it-doc-fisico.quantidade.
            
            /* --- Gera Rat-Lote Fisico --- */

            RUN pi_limpa_epc.

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "ItemRatLote"
                   tt-epc.cod-parameter = "nfe-nota-fiscal-rec(ROWID)"
                   tt-epc.val-parameter = STRING(ROWID(bfu-nfe-nota-fiscal-rec)).

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "ItemRatLote"
                   tt-epc.cod-parameter = "tt-it-doc-fisico(HANDLE)"
                   tt-epc.val-parameter = STRING(TEMP-TABLE tt-it-doc-fisico:HANDLE).

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "ItemRatLote"
                   tt-epc.cod-parameter = "tt-rat-lote(HANDLE)"
                   tt-epc.val-parameter = STRING(TEMP-TABLE tt-rat-lote:HANDLE).

            {include\i-epc201.i "ItemRatLote"}

            /* --- Se nao Efetivar entrada via EPC retorna NOK para executar rotina Padrao --- */

            IF c-executa-bonfe001 = "PADRAO" THEN 
            DO:
                ASSIGN l-aux2 = NO.

                FOR EACH tt-rat-lote WHERE 
                         tt-rat-lote.serie-docto  = tt-it-doc-fisico.serie-docto              AND 
                         tt-rat-lote.nro-docto    = tt-it-doc-fisico.nro-docto                AND 
                         tt-rat-lote.cod-emitente = tt-it-doc-fisico.cod-emitente             AND 
                         tt-rat-lote.nat-operacao = bf-nfe-it-nota-fisc-rec.item-nat-operacao AND 
                          tt-rat-lote.sequencia    = tt-it-doc-fisico.sequencia               NO-LOCK:

                    ASSIGN l-aux2 = YES.

                    CREATE rat-lote.

                    ASSIGN rat-lote.cod-emitente = tt-rat-lote.cod-emitente
                           rat-lote.serie-docto  = tt-rat-lote.serie-docto
                           rat-lote.nro-docto    = tt-rat-lote.nro-docto
                           rat-lote.nat-operacao = ""
                           rat-lote.tipo-nota    = tt-it-doc-fisico.tipo-nota
                           rat-lote.sequencia    = tt-rat-lote.sequencia
                           rat-lote.int-2        = 1 /* Inclusao */
                           rat-lote.it-codigo    = tt-rat-lote.it-codigo
                           rat-lote.cod-depos    = tt-rat-lote.cod-depos
                           rat-lote.cod-localiz  = tt-rat-lote.cod-localiz
                           rat-lote.lote         = tt-rat-lote.lote
                           rat-lote.dt-vali-lote = tt-rat-lote.dt-vali-lote
                           rat-lote.quantidade   = tt-rat-lote.quantidade.
                    
                    /* --- Atualiza Nivel de Inspecao do Item Fornecedor --- */

                    RUN pi_atualiza_niv_inspec (INPUT tt-rat-lote.cod-emitente,
                                                INPUT tt-rat-lote.it-codigo,
                                                INPUT ROWID(rat-lote),
                                                INPUT 1, /* --- Soma --- */
                                                OUTPUT p_mensagem_erro).
                END.

                IF l-aux2 = NO THEN 
                DO:
                    FIND FIRST tt-it-doc-fisico NO-LOCK NO-ERROR.
                    
                    CREATE rat-lote.

                    ASSIGN rat-lote.cod-emitente = tt-it-doc-fisico.cod-emitente
                           rat-lote.serie-docto  = tt-it-doc-fisico.serie-docto
                           rat-lote.nro-docto    = tt-it-doc-fisico.nro-docto
                           rat-lote.nat-operacao = ""
                           rat-lote.tipo-nota    = tt-it-doc-fisico.tipo-nota
                           rat-lote.sequencia    = tt-it-doc-fisico.sequencia
                           rat-lote.int-2        = 1 /* Inclusao */
                           rat-lote.it-codigo    = tt-it-doc-fisico.it-codigo
                           rat-lote.cod-depos    = tt-it-doc-fisico.cod-depos
                           rat-lote.cod-localiz  = tt-it-doc-fisico.cod-localiz
                           rat-lote.lote         = tt-it-doc-fisico.lote
                           rat-lote.dt-vali-lote = tt-it-doc-fisico.dt-vali-lote
                           rat-lote.quantidade   = tt-it-doc-fisico.quantidade.
    
                    /* --- Atualiza Nivel de Inspecao do Item Fornecedor --- */

                    RUN pi_atualiza_niv_inspec (INPUT tt-it-doc-fisico.cod-emitente,
                                                INPUT tt-it-doc-fisico.it-codigo,
                                                INPUT ROWID(rat-lote),
                                                INPUT 1, /* --- Soma --- */
                                                OUTPUT p_mensagem_erro).
                END.
            END.

            /* --- Gera Fifo para Ordens de Compra Receb Fisico --- */

            FIND FIRST tt-it-doc-fisico NO-LOCK NO-ERROR.

            IF AVAIL tt-it-doc-fisico THEN 
            DO:
                IF tt-it-doc-fisico.fifo-oc = YES THEN 
                DO:
                    FIND FIRST bf-it-doc-fisico WHERE 
                               bf-it-doc-fisico.serie-docto  = tt-it-doc-fisico.serie-docto  AND 
                               bf-it-doc-fisico.nro-docto    = tt-it-doc-fisico.nro-docto    AND 
                               bf-it-doc-fisico.cod-emitente = tt-it-doc-fisico.cod-emitente AND 
                               bf-it-doc-fisico.tipo-nota    = tt-it-doc-fisico.tipo-nota    AND 
                               bf-it-doc-fisico.sequencia    = tt-it-doc-fisico.sequencia    NO-LOCK NO-ERROR.

                    IF AVAIL bf-it-doc-fisico THEN 
                        RUN rep/re2001p.p (INPUT ROWID(bf-it-doc-fisico)).


                END.
                
                /* --- Gera hist®rico de item gerados no Recebimento --- */

                FIND FIRST nfe-his-it-nota-fis-rec WHERE 
                           nfe-his-it-nota-fis-rec.chave-acesso-nfe = bf-nfe-it-nota-fisc-rec.chave-acesso      AND 
                           nfe-his-it-nota-fis-rec.seq-item         = bf-nfe-it-nota-fisc-rec.seq-item          AND 
                           nfe-his-it-nota-fis-rec.sequencia        = tt-it-doc-fisico.sequencia                AND 
                           nfe-his-it-nota-fis-rec.nat-operacao     = bf-nfe-it-nota-fisc-rec.item-nat-operacao NO-LOCK NO-ERROR.

                IF NOT AVAIL nfe-his-it-nota-fis-rec THEN 
                DO:
                    CREATE nfe-his-it-nota-fis-rec.

                    ASSIGN nfe-his-it-nota-fis-rec.chave-acesso-nfe = bf-nfe-it-nota-fisc-rec.chave-acesso     
                           nfe-his-it-nota-fis-rec.seq-item         = bf-nfe-it-nota-fisc-rec.seq-item         
                           nfe-his-it-nota-fis-rec.sequencia        = tt-it-doc-fisico.sequencia               
                           nfe-his-it-nota-fis-rec.nat-operacao     = bf-nfe-it-nota-fisc-rec.item-nat-operacao.
                END.
            END.

            RUN pi_limpa_epc.

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "ItemDepoisRecebFisico"
                   tt-epc.cod-parameter = "tt-it-doc-fisico(HANDLE)"
                   tt-epc.val-parameter = STRING(TEMP-TABLE tt-it-doc-fisico:HANDLE).

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "ItemDepoisRecebFisico"
                   tt-epc.cod-parameter = "bf-nfe-it-nota-fisc-rec(ROWID)"
                   tt-epc.val-parameter = STRING(ROWID(bf-nfe-it-nota-fisc-rec)).

            CREATE tt-epc.

            ASSIGN tt-epc.cod-event     = "ItemDepoisRecebFisico"
                   tt-epc.cod-parameter = "tt-rat-lote(HANDLE)"
                   tt-epc.val-parameter = STRING(TEMP-TABLE tt-rat-lote:HANDLE).

            {include\i-epc201.i "ItemDepoisRecebFisico"}
        END.
    END.

END PROCEDURE. /* pi_item_receb_fisico */

/**************************************************************************************************************************/

PROCEDURE pi_item_recebimento:

    DEF INPUT        PARAM p_rowid_item         AS ROWID                            NO-UNDO.
    DEF INPUT        PARAM p_origem             AS INTE                             NO-UNDO.
    DEF INPUT        PARAM p_preco_bruto        AS LOG                             NO-UNDO.
    DEF INPUT-OUTPUT PARAM p_sequencia          AS INTE                             NO-UNDO.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-item-doc-est.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-item-doc-est-aux.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-it-doc-fisico.
    DEF INPUT-OUTPUT PARAM TABLE FOR tt-rat-lote.
    DEF OUTPUT       PARAM p_mensagem_erro      AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    DEF VAR p_seq_it_docum_est      LIKE it-doc-fisico.sequencia                    NO-UNDO.
    DEFINE VARIABLE de-fator-aux AS DECIMAL     NO-UNDO.

    
    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE 
               ROWID(bf-nfe-it-nota-fisc-rec) = p_rowid_item NO-LOCK NO-ERROR.
    
    IF AVAIL bf-nfe-it-nota-fisc-rec THEN DO:
        FIND FIRST bfu-nfe-nota-fiscal-rec OF bf-nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.

        FIND FIRST bf-item WHERE 
                   bf-item.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK NO-ERROR.

        FIND emitente WHERE 
             emitente.nome-abrev = bfu-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

        /* --- Verifica quais Relacionamentos existem na Sequencia do Item (Ordem - Ser/lote - Docto/Nota) --- */

        RUN pi_verifica_relac_item (INPUT  ROWID(bf-nfe-it-nota-fisc-rec),
                                    OUTPUT l-ordem,
                                    OUTPUT l-ordem-ser-lote,
                                    OUTPUT l-ser-lote,
                                    OUTPUT l-docto-nota,
                                    OUTPUT l-docto-ser-lote).
        /* --- Entrada Recebimento com Relacionamento Ordens de Compra --- */
        IF l-ordem = YES THEN DO:
            FOR EACH nfe-relac-ordem-rec WHERE 
                     nfe-relac-ordem-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                     nfe-relac-ordem-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     EXCLUSIVE-LOCK:
                                    
                /* --- Controla Sequencia do Item no Recebimento --- */

                ASSIGN p_sequencia        = p_sequencia + 10
                       p_seq_it_docum_est = 0.

                IF p_origem = 1 THEN /* Recebimento FÐsico */ 
                DO:
                    /* --- Cria Tabela de Itens do Recebimento FÐsico --- */
        
                    FIND FIRST item-fornec NO-LOCK
                        WHERE item-fornec.cod-emitente = emitente.cod-emitente
                        AND   item-fornec.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo NO-ERROR.

                    IF AVAIL item-fornec THEN
                        ASSIGN de-fator-aux = item-fornec.fator-conver * EXP(10,(item-fornec.num-casa-dec * (-1))).
                    ELSE
                        ASSIGN de-fator-aux = 0.

                    
                    RUN pi_item_receb_fisico (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                              INPUT p_sequencia,
                                              INPUT nfe-relac-ordem-rec.num-pedido,
                                              INPUT nfe-relac-ordem-rec.numero-ordem,
                                              INPUT nfe-relac-ordem-rec.parcela-oc,
                                              INPUT nfe-relac-ordem-rec.quantidade , /*bf-nfe-it-nota-fisc-rec.item-qCom,*/ /*((nfe-relac-ordem-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),*/
                                              INPUT IF de-fator-aux = 0 THEN nfe-relac-ordem-rec.quantidade ELSE nfe-relac-ordem-rec.quantidade * de-fator-aux  /*bf-nfe-it-nota-fisc-rec.item-qtde*/, /*nfe-relac-ordem-rec.quantidade,  rontan */
                                              INPUT (nfe-relac-ordem-rec.quantidade * DECI(nfe-relac-ordem-rec.vl-unitario)),
                                              INPUT nfe-relac-ordem-rec.vl-unitario,
                                              INPUT nfe-relac-ordem-rec.conta-contabil,
                                              INPUT nfe-relac-ordem-rec.nr-ord-produ,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-cod-depos,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-cod-localiz,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-ser-lote,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-dt-vali-lote,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                              INPUT p_preco_bruto,
                                              OUTPUT p_seq_it_docum_est,
                                              INPUT-OUTPUT TABLE tt-it-doc-fisico,
                                              INPUT-OUTPUT TABLE tt-rat-lote,
                                              OUTPUT p_mensagem_erro).

                    IF p_seq_it_docum_est <> 0 THEN
                        ASSIGN nfe-relac-ordem-rec.seq-item-it-fisico = p_seq_it_docum_est.
                END.
                ELSE DO:
                    /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                    RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                              INPUT p_sequencia,
                                              INPUT nfe-relac-ordem-rec.num-pedido,
                                              INPUT nfe-relac-ordem-rec.numero-ordem,
                                              INPUT nfe-relac-ordem-rec.parcela-oc,
                                              INPUT nfe-relac-ordem-rec.quantidade,
                                              INPUT ((nfe-relac-ordem-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                              INPUT (nfe-relac-ordem-rec.quantidade * DECI(nfe-relac-ordem-rec.vl-unitario)),
                                              INPUT nfe-relac-ordem-rec.vl-unitario,
                                              INPUT nfe-relac-ordem-rec.conta-contabil,
                                              INPUT nfe-relac-ordem-rec.nr-ord-produ,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-cod-depos,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-cod-localiz,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-ser-lote,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-dt-vali-lote,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                              INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                              INPUT p_preco_bruto,
                                              INPUT-OUTPUT TABLE tt-item-doc-est,
                                              INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                              OUTPUT p_mensagem_erro).
                END.
            END. /* FOR EACH nfe-relac-ordem-rec WHERE....*/
        END. /* IF l-ordem = YES THEN.... */
        
        /* --- Entrada Recebimento com Relacionamento Ordens de Compra e Relacionamento de Serie/Lote --- */

        IF l-ordem-ser-lote = YES THEN 
        DO:
            FIND FIRST nfe-relac-ser-lote-rec WHERE 
                       nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                       nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK NO-ERROR.

            IF AVAIL nfe-relac-ser-lote-rec THEN 
                ASSIGN rw-aux               = ROWID(nfe-relac-ser-lote-rec)
                       de-saldo-ser-lote    = nfe-relac-ser-lote-rec.quantidade
                       l-aux                = NO.

            FOR EACH nfe-relac-ordem-rec WHERE 
                     nfe-relac-ordem-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                     nfe-relac-ordem-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     EXCLUSIVE-LOCK:

                ASSIGN de-saldo-ordem = nfe-relac-ordem-rec.quantidade.

                DO WHILE de-saldo-ordem <> 0 :
                    FIND FIRST nfe-relac-ser-lote-rec WHERE 
                               ROWID(nfe-relac-ser-lote-rec) = rw-aux NO-LOCK NO-ERROR.

                    /* --- Ordem Maior ou Igual do que Serie/Lote --- */

                    IF de-saldo-ordem >= de-saldo-ser-lote THEN
                        ASSIGN de-saldo-ordem    = de-saldo-ordem - de-saldo-ser-lote
                               de-quantidade     = de-saldo-ser-lote
                               de-saldo-ser-lote = 0
                               l-aux             = YES.

                    IF de-saldo-ordem < de-saldo-ser-lote THEN
                        ASSIGN de-saldo-ser-lote = de-saldo-ser-lote - de-saldo-ordem
                               de-quantidade     = de-saldo-ordem
                               de-saldo-ordem    = 0
                               l-aux             = NO.

                    /* --- Controla Sequencia do Item no Recebimento --- */

                    ASSIGN p_sequencia        = p_sequencia + 10
                           p_seq_it_docum_est = 0.

                    IF p_origem = 1 THEN /* Recebimento FÐsico */
                    DO:
                        /* --- Cria Tabela de Itens do Recebimento Fisico --- */

                        
                        RUN pi_item_receb_fisico (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                  INPUT p_sequencia,
                                                  INPUT nfe-relac-ordem-rec.num-pedido,
                                                  INPUT nfe-relac-ordem-rec.numero-ordem,
                                                  INPUT nfe-relac-ordem-rec.parcela-oc,
                                                  INPUT de-quantidade,
                                                  INPUT ((de-quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                  INPUT (de-quantidade * DECI(nfe-relac-ordem-rec.vl-unitario)),
                                                  INPUT nfe-relac-ordem-rec.vl-unitario,
                                                  INPUT nfe-relac-ordem-rec.conta-contabil,
                                                  INPUT nfe-relac-ordem-rec.nr-ord-produ,
                                                  INPUT nfe-relac-ser-lote-rec.cod-depos,
                                                  INPUT nfe-relac-ser-lote-rec.cod-localiz,
                                                  INPUT nfe-relac-ser-lote-rec.ser-lote,
                                                  INPUT nfe-relac-ser-lote-rec.dt-vali-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                                  INPUT p_preco_bruto,
                                                  OUTPUT p_seq_it_docum_est,
                                                  INPUT-OUTPUT TABLE tt-it-doc-fisico,
                                                  INPUT-OUTPUT TABLE tt-rat-lote,
                                                  OUTPUT p_mensagem_erro).
                        IF p_seq_it_docum_est <> 0 THEN
                            ASSIGN nfe-relac-ordem-rec.seq-item-it-fisico = p_seq_it_docum_est.
                    END.
                    ELSE 
                    DO:
                        /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                        RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                  INPUT p_sequencia,
                                                  INPUT nfe-relac-ordem-rec.num-pedido,
                                                  INPUT nfe-relac-ordem-rec.numero-ordem,
                                                  INPUT nfe-relac-ordem-rec.parcela-oc,
                                                  INPUT de-quantidade,
                                                  INPUT ((de-quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                  INPUT (de-quantidade * DECI(nfe-relac-ordem-rec.vl-unitario)),
                                                  INPUT nfe-relac-ordem-rec.vl-unitario,
                                                  INPUT nfe-relac-ordem-rec.conta-contabil,
                                                  INPUT nfe-relac-ordem-rec.nr-ord-produ,
                                                  INPUT nfe-relac-ser-lote-rec.cod-depos,
                                                  INPUT nfe-relac-ser-lote-rec.cod-localiz,
                                                  INPUT nfe-relac-ser-lote-rec.ser-lote,
                                                  INPUT nfe-relac-ser-lote-rec.dt-vali-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                                  INPUT p_preco_bruto,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                  OUTPUT p_mensagem_erro).
                    END.
                        
                    IF l-aux = YES THEN 
                    DO:
                        FIND NEXT nfe-relac-ser-lote-rec WHERE 
                                  nfe-relac-ser-lote-rec.chave-acesso = nfe-relac-ordem-rec.chave-acesso AND 
                                  nfe-relac-ser-lote-rec.seq-item     = nfe-relac-ordem-rec.seq-item     NO-LOCK NO-ERROR.

                        IF AVAIL nfe-relac-ser-lote-rec THEN
                            ASSIGN rw-aux            = ROWID(nfe-relac-ser-lote-rec)
                                   de-saldo-ser-lote = nfe-relac-ser-lote-rec.quantidade.
                    END.
                END. /* DO WHILE de-saldo-ordem <> 0.... */
            END. /* FOR EACH nfe-relac-ordem-rec WHERE..... */
        END. /* IF l-ordem-ser-lote = YES THEN..... */
        
        /* --- Entrada Recebimento com Relacionamento de Serie/Lote --- */

        IF l-ser-lote = YES THEN 
        DO:
            /* --- Por Serie --- */

            IF bf-item.tipo-con-est = 2 THEN 
            DO:
                FOR EACH nfe-relac-ser-lote-rec WHERE 
                         nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                         nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK:

                    /* --- Controla Sequencia do Item no Recebimento --- */

                    ASSIGN p_sequencia = p_sequencia + 10.
                        
                    IF p_origem = 1 THEN /* Recebimento FÐsico */
                    DO:
                        /* --- Cria Tabela de Itens do Recebimento Fisico --- */
                                                
                        RUN pi_item_receb_fisico (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                  INPUT p_sequencia,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                  INPUT nfe-relac-ser-lote-rec.quantidade,
                                                  INPUT ((nfe-relac-ser-lote-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                  INPUT (nfe-relac-ser-lote-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-vUnCom),
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nr-ord-produ,
                                                  INPUT nfe-relac-ser-lote-rec.cod-depos,
                                                  INPUT nfe-relac-ser-lote-rec.cod-localiz,
                                                  INPUT nfe-relac-ser-lote-rec.ser-lote,
                                                  INPUT nfe-relac-ser-lote-rec.dt-vali-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                                  INPUT p_preco_bruto,
                                                  OUTPUT p_seq_it_docum_est,
                                                  INPUT-OUTPUT TABLE tt-it-doc-fisico,
                                                  INPUT-OUTPUT TABLE tt-rat-lote,
                                                  OUTPUT p_mensagem_erro).
                    END.
                    ELSE 
                    DO:
                        /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                        RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                  INPUT p_sequencia,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                  INPUT nfe-relac-ser-lote-rec.quantidade,
                                                  INPUT ((nfe-relac-ser-lote-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                  INPUT (nfe-relac-ser-lote-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-vUnCom),
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nr-ord-produ,
                                                  INPUT nfe-relac-ser-lote-rec.cod-depos,
                                                  INPUT nfe-relac-ser-lote-rec.cod-localiz,
                                                  INPUT nfe-relac-ser-lote-rec.ser-lote,
                                                  INPUT nfe-relac-ser-lote-rec.dt-vali-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                                  INPUT p_preco_bruto,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                  OUTPUT p_mensagem_erro).
                    END.
                END. /* FOR EACH nfe-relac-ser-lote-rec WHERE.... */
            END. /* IF bf-item.tipo-con-est = 2 THEN.... */
            ELSE 
            DO:
                IF bf-item.tipo-con-est = 1 OR         /* --- Serial --- */
                   bf-item.tipo-con-est = 3 OR         /* --- Lote --- */
                   bf-item.tipo-con-est = 4 THEN       /* --- Referencia --- */
                DO:
                    /* --- Controla Sequencia do Item no Recebimento --- */

                    ASSIGN p_sequencia = p_sequencia + 10.

                    FOR EACH nfe-relac-ser-lote-rec WHERE 
                             nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                             nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK:

                        CREATE tt-rat-lote.

                        ASSIGN tt-rat-lote.cod-emitente = emitente.cod-emitente
                               tt-rat-lote.serie-docto  = bfu-nfe-nota-fiscal-rec.ide-Serie
                               tt-rat-lote.nro-docto    = bfu-nfe-nota-fiscal-rec.ide-nNF
                               tt-rat-lote.nat-operacao = bf-nfe-it-nota-fisc-rec.item-nat-operacao
                               tt-rat-lote.tipo-nota    = IF bf-nfe-it-nota-fisc-rec.item-esp-nat = "NFE" THEN 1 ELSE 2
                               tt-rat-lote.sequencia    = p_sequencia
                               tt-rat-lote.int-2        = 1 /* Inclusao */
                               tt-rat-lote.cod-depos    = nfe-relac-ser-lote-rec.cod-depos
                               tt-rat-lote.cod-localiz  = nfe-relac-ser-lote-rec.cod-localiz
                               tt-rat-lote.lote         = nfe-relac-ser-lote-rec.ser-lote
                               tt-rat-lote.dt-vali-lote = nfe-relac-ser-lote-rec.dt-vali-lote
                               tt-rat-lote.it-codigo    = bf-nfe-it-nota-fisc-rec.it-codigo
                               tt-rat-lote.quantidade   = nfe-relac-ser-lote-rec.quantidade.
                    END.

                    IF p_origem = 1 THEN /* Recebimento FÐsico */
                    DO:
                        /* --- Cria Tabela de Itens do Recebimento Fisico --- */
                                                
                        RUN pi_item_receb_fisico (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                  INPUT p_sequencia,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-qtde,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-qCom,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-vProd,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nr-ord-produ,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-depos,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-localiz,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-ser-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-dt-vali-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                                  INPUT p_preco_bruto,
                                                  OUTPUT p_seq_it_docum_est,
                                                  INPUT-OUTPUT TABLE tt-it-doc-fisico,
                                                  INPUT-OUTPUT TABLE tt-rat-lote,
                                                  OUTPUT p_mensagem_erro).
                    END.
                    ELSE
                    DO:
                        /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                        RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                  INPUT p_sequencia,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-qtde,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-qCom,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-vProd,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nr-ord-produ,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-depos,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-localiz,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-ser-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-dt-vali-lote,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                                  INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                                  INPUT p_preco_bruto,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est,
                                                  INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                  OUTPUT p_mensagem_erro).
                    END.
                END. /* IF bf-item.tipo-con-est = 1 OR         /* --- Serial --- */ */
            END. /* ELSE do IF bf-item.tipo-con-est = 2 THEN.... */
        END. /* IF l-ser-lote = YES THEN.... */

        /* --- Entrada Recebimento Fiscal com Relacionamento de Documento ou Nota Fiscal Referenciada e Deposito/Localiz/Serie --- */

        IF l-docto-ser-lote = YES THEN 
        DO:
            FIND FIRST nfe-relac-ser-lote-rec WHERE 
                       nfe-relac-ser-lote-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                       nfe-relac-ser-lote-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK NO-ERROR.

            IF AVAIL nfe-relac-ser-lote-rec THEN
                ASSIGN rw-aux               = ROWID(nfe-relac-ser-lote-rec)
                       de-saldo-ser-lote    = nfe-relac-ser-lote-rec.quantidade
                       l-aux                = NO.

            FOR EACH nfe-relac-doc-nota-rec WHERE 
                     nfe-relac-doc-nota-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                     nfe-relac-doc-nota-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK :

                ASSIGN de-saldo-doc-nota = nfe-relac-doc-nota-rec.quantidade.

                DO WHILE de-saldo-doc-nota <> 0 :
                    FIND FIRST nfe-relac-ser-lote-rec WHERE 
                               ROWID(nfe-relac-ser-lote-rec) = rw-aux NO-LOCK NO-ERROR.

                    /* --- Documento Maior ou Igual do que Serie/Lote --- */

                    IF de-saldo-doc-nota >= de-saldo-ser-lote THEN
                        ASSIGN de-saldo-doc-nota    = de-saldo-doc-nota - de-saldo-ser-lote
                               de-quantidade        = de-saldo-ser-lote
                               de-saldo-ser-lote    = 0
                               l-aux                = YES.

                    IF de-saldo-doc-nota < de-saldo-ser-lote THEN
                        ASSIGN de-saldo-ser-lote    = de-saldo-ser-lote - de-saldo-doc-nota
                               de-quantidade        = de-saldo-doc-nota
                               de-saldo-doc-nota    = 0
                               l-aux                = NO.
                        
                    /* --- Controla Sequencia do Item no Recebimento --- */

                    ASSIGN p_sequencia = p_sequencia + 10.
                    
                    /* --- Retona o Tipo de Nota de acordo com a Natureza --- */

                    RUN pi_valida_tipo_nota (INPUT  bf-nfe-it-nota-fisc-rec.item-nat-operacao,
                                             OUTPUT i-tipo-nfe,
                                             OUTPUT i-tp-oper-terc).

                    
                    
                    IF i-tipo-nfe = 0 THEN
                    DO:
                        ASSIGN p_mensagem_erro = "1,"                                     +
                                                 "Natureza de operacao nao encontrada !," +
                                                 "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_ITEM_RECEBIMENTO)".

                        RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                              OUTPUT i-acao).
                    END.

                    /* --- Devolucao de Clientes --- */

                    IF i-tipo-nfe = 2 THEN 
                    DO:
                        FIND FIRST it-nota-fisc WHERE 
                                   it-nota-fisc.cod-estabel  = nfe-relac-doc-nota-rec.cod-estabel   AND 
                                   it-nota-fisc.serie        = nfe-relac-doc-nota-rec.serie         AND 
                                   it-nota-fisc.nr-nota-fis  = nfe-relac-doc-nota-rec.nr-docto-nota AND 
                                   it-nota-fisc.nr-seq-fat   = nfe-relac-doc-nota-rec.sequencia     AND 
                                   it-nota-fisc.it-codigo    = nfe-relac-doc-nota-rec.it-codigo     NO-LOCK NO-ERROR.

                        IF AVAIL it-nota-fisc THEN 
                        DO:
                            IF p_origem = 2 THEN 
                            DO:
                                /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                                RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                          INPUT p_sequencia,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                          INPUT de-quantidade,
                                                          INPUT ((de-quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                          INPUT (de-quantidade * DECI(bf-nfe-it-nota-fisc-rec.item-vUnCom)),
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                          INPUT nfe-relac-doc-nota-rec.nr-ord-produ,
                                                          INPUT nfe-relac-ser-lote-rec.cod-depos,
                                                          INPUT nfe-relac-ser-lote-rec.cod-localiz,
                                                          INPUT nfe-relac-ser-lote-rec.ser-lote,
                                                          INPUT nfe-relac-ser-lote-rec.dt-vali-lote,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                          INPUT it-nota-fisc.serie,
                                                          INPUT it-nota-fisc.nr-nota-fis,
                                                          INPUT it-nota-fisc.nat-operacao,
                                                          INPUT it-nota-fisc.nr-seq-fat,
                                                          INPUT it-nota-fisc.dt-emis-nota,
                                                          INPUT p_preco_bruto,
                                                          INPUT-OUTPUT TABLE tt-item-doc-est,
                                                          INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                          OUTPUT p_mensagem_erro).
                            END.
                        END.
                    END.
                    
                    /* --- Retorno Terceiros --- */

                    IF i-tipo-nfe = 3 THEN 
                    DO:
                        FIND FIRST saldo-terc WHERE 
                                   saldo-terc.serie-docto  = nfe-relac-doc-nota-rec.serie         AND 
                                   saldo-terc.nro-docto    = nfe-relac-doc-nota-rec.nr-docto-nota AND 
                                   saldo-terc.cod-emitente = nfe-relac-doc-nota-rec.cod-emitente  AND 
                                   saldo-terc.nat-operacao = nfe-relac-doc-nota-rec.nat-operacao  AND 
                                   saldo-terc.it-codigo    = nfe-relac-doc-nota-rec.it-codigo     AND 
                                   saldo-terc.cod-refer    = nfe-relac-doc-nota-rec.cod-refer     AND 
                                   saldo-terc.sequencia    = nfe-relac-doc-nota-rec.sequencia     NO-LOCK NO-ERROR.

                        IF AVAIL saldo-terc THEN 
                        DO:
                            IF p_origem = 2 THEN 
                            DO:
                                /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                                RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                          INPUT p_sequencia,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                          INPUT de-quantidade,
                                                          INPUT ((de-quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                          INPUT (de-quantidade * DECI(bf-nfe-it-nota-fisc-rec.item-vUnCom)),
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                          INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                          INPUT nfe-relac-doc-nota-rec.nr-ord-produ,
                                                          INPUT nfe-relac-ser-lote-rec.cod-depos,
                                                          INPUT nfe-relac-ser-lote-rec.cod-localiz,
                                                          INPUT saldo-terc.lote,
                                                          INPUT nfe-relac-ser-lote-rec.dt-vali-lote,
                                                          INPUT saldo-terc.cod-refer,
                                                          INPUT saldo-terc.serie-docto,
                                                          INPUT saldo-terc.nro-docto,
                                                          INPUT saldo-terc.nat-operacao,
                                                          INPUT saldo-terc.sequencia,
                                                          INPUT saldo-terc.dt-retorno,
                                                          INPUT p_preco_bruto,
                                                          INPUT-OUTPUT TABLE tt-item-doc-est,
                                                          INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                          OUTPUT p_mensagem_erro).
                            END.
                        END.
                    END.
                        
                    IF l-aux = YES THEN 
                    DO:
                        FIND NEXT nfe-relac-ser-lote-rec WHERE 
                                  nfe-relac-ser-lote-rec.chave-acesso = nfe-relac-doc-nota-rec.chave-acesso AND 
                                  nfe-relac-ser-lote-rec.seq-item     = nfe-relac-doc-nota-rec.seq-item     NO-LOCK NO-ERROR.

                        IF AVAIL nfe-relac-ser-lote-rec THEN
                            ASSIGN rw-aux               = ROWID(nfe-relac-ser-lote-rec)
                                   de-saldo-ser-lote    = nfe-relac-ser-lote-rec.quantidade.
                    END.
                END. /* DO WHILE de-saldo-doc-nota <> 0... */
            END. /* FOR EACH nfe-relac-doc-nota-rec WHERE..... */
        END. /* IF l-docto-ser-lote = YES THEN.... */

        /* --- Entrada Recebimento Fiscal com Relacionamento de Documento ou Nota Fiscal Referenciada --- */

        IF l-docto-nota = YES THEN 
        DO:
            
            FOR EACH nfe-relac-doc-nota-rec WHERE 
                     nfe-relac-doc-nota-rec.chave-acesso = bf-nfe-it-nota-fisc-rec.chave-acesso AND 
                     nfe-relac-doc-nota-rec.seq-item     = bf-nfe-it-nota-fisc-rec.seq-item     NO-LOCK:

                /* --- Controla Sequencia do Item no Recebimento --- */

                ASSIGN p_sequencia = p_sequencia + 10.

                /* --- Retorna o Tipo de Nota de acordo com a Natureza --- */

                RUN pi_valida_tipo_nota (INPUT  bf-nfe-it-nota-fisc-rec.item-nat-operacao,
                                         OUTPUT i-tipo-nfe,
                                         OUTPUT i-tp-oper-terc).

                IF i-tipo-nfe = 0 THEN
                DO:
                    ASSIGN p_mensagem_erro = "1,"                                     +
                                             "Natureza de operacao nao encontrada !," +
                                             "Natureza de operacao nao encontrada na validacao do tipo da NF-e !. (PI_ITEM_RECEBIMENTO)".

                    RUN pi_mensagem_erro (INPUT  p_mensagem_erro,
                                          OUTPUT i-acao).
                END.

                /* --- Devolucao de Clientes --- */

                IF i-tipo-nfe = 2 THEN 
                DO:
                    FIND FIRST it-nota-fisc WHERE 
                               it-nota-fisc.cod-estabel  = nfe-relac-doc-nota-rec.cod-estabel   AND 
                               it-nota-fisc.serie        = nfe-relac-doc-nota-rec.serie         AND 
                               it-nota-fisc.nr-nota-fis  = nfe-relac-doc-nota-rec.nr-docto-nota AND 
                               it-nota-fisc.nr-seq-fat   = nfe-relac-doc-nota-rec.sequencia     AND 
                               it-nota-fisc.it-codigo    = nfe-relac-doc-nota-rec.it-codigo     NO-LOCK NO-ERROR.

                    IF AVAIL it-nota-fisc THEN 
                    DO:
                        IF p_origem = 2 THEN 
                        DO:
                            /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                            RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                      INPUT p_sequencia,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                      INPUT nfe-relac-doc-nota-rec.quantidade,
                                                      INPUT ((nfe-relac-doc-nota-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                      INPUT (nfe-relac-doc-nota-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-vUnCom),
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                      INPUT nfe-relac-doc-nota-rec.nr-ord-produ,
                                                      INPUT IF nfe-relac-doc-nota-rec.cod-depos <> "" THEN nfe-relac-doc-nota-rec.cod-depos ELSE bf-nfe-it-nota-fisc-rec.item-cod-depos,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-cod-localiz,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-ser-lote,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-dt-vali-lote,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                                      INPUT it-nota-fisc.serie,
                                                      INPUT it-nota-fisc.nr-nota-fis,
                                                      INPUT it-nota-fisc.nat-operacao,
                                                      INPUT it-nota-fisc.nr-seq-fat,
                                                      INPUT it-nota-fisc.dt-emis-nota,
                                                      INPUT p_preco_bruto,
                                                      INPUT-OUTPUT TABLE tt-item-doc-est,
                                                      INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                      OUTPUT p_mensagem_erro).
                        END.
                    END.
                END.

                /* --- Retorno Terceiros --- */

                IF i-tipo-nfe = 3 THEN 
                DO:
                    
                    FIND FIRST saldo-terc WHERE 
                               saldo-terc.serie-docto      = nfe-relac-doc-nota-rec.serie         AND 
                               saldo-terc.nro-docto        = nfe-relac-doc-nota-rec.nr-docto-nota AND 
                               saldo-terc.cod-emitente     = nfe-relac-doc-nota-rec.cod-emitente  AND 
                               saldo-terc.nat-operacao     = nfe-relac-doc-nota-rec.nat-operacao  AND 
                               saldo-terc.it-codigo        = nfe-relac-doc-nota-rec.it-codigo     AND 
                               saldo-terc.cod-refer        = nfe-relac-doc-nota-rec.cod-refer     AND 
                               saldo-terc.sequencia        = nfe-relac-doc-nota-rec.sequencia     NO-LOCK NO-ERROR.

                    IF AVAIL saldo-terc THEN 
                    DO:

                        
                        IF p_origem = 2 THEN 
                        DO:
                            FIND FIRST saldo-estoq WHERE 
                                       saldo-estoq.it-codigo  = saldo-terc.it-codigo AND 
                                       saldo-estoq.lote       = saldo-terc.lote      NO-LOCK NO-ERROR.

                            /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                            RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                                      INPUT p_sequencia,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                                      INPUT nfe-relac-doc-nota-rec.quantidade,
                                                      INPUT ((nfe-relac-doc-nota-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),
                                                      INPUT (nfe-relac-doc-nota-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-vUnCom),
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                                      INPUT nfe-relac-doc-nota-rec.nr-ord-produ,
                                                      INPUT IF nfe-relac-doc-nota-rec.cod-depos <> "" THEN nfe-relac-doc-nota-rec.cod-depos ELSE saldo-terc.cod-depos,
                                                      INPUT saldo-terc.cod-localiz,
                                                      INPUT saldo-terc.lote,
                                                      INPUT (IF AVAIL saldo-estoq THEN saldo-estoq.dt-vali-lote
                                                                                  ELSE bf-nfe-it-nota-fisc-rec.item-dt-vali-lote),
                                                      INPUT saldo-terc.cod-refer,
                                                      INPUT saldo-terc.serie-docto,
                                                      INPUT saldo-terc.nro-docto,
                                                      INPUT saldo-terc.nat-operacao,
                                                      INPUT saldo-terc.sequencia,
                                                      INPUT saldo-terc.dt-retorno,
                                                      INPUT p_preco_bruto,
                                                      INPUT-OUTPUT TABLE tt-item-doc-est,
                                                      INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                                      OUTPUT p_mensagem_erro).
                        END.
                        ELSE IF p_origem = 1 THEN DO:

                            

                            FIND FIRST saldo-estoq WHERE 
                                       saldo-estoq.it-codigo  = saldo-terc.it-codigo AND 
                                       saldo-estoq.lote       = saldo-terc.lote      NO-LOCK NO-ERROR.

                                                    
                            RUN pi_item_receb_fisico (INPUT ROWID(bf-nfe-it-nota-fisc-rec),                                                                                   
                                                      INPUT p_sequencia,                                                                                                      
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-xPed,                                                                                
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,                                                                           
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,                                                                          
                                                      INPUT nfe-relac-doc-nota-rec.quantidade,                                                                                
                                                      INPUT ((nfe-relac-doc-nota-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-qCom) / bf-nfe-it-nota-fisc-rec.item-qtde),    
                                                      INPUT (nfe-relac-doc-nota-rec.quantidade * bf-nfe-it-nota-fisc-rec.item-vUnCom),                                        
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,                                                                              
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,       
                                                      INPUT bf-nfe-it-nota-fisc-rec.item-nr-ord-produ,          
                                                      INPUT IF nfe-relac-doc-nota-rec.cod-depos <> "" THEN nfe-relac-doc-nota-rec.cod-depos ELSE saldo-terc.cod-depos,        
                                                      INPUT saldo-terc.cod-localiz,                                                                                           
                                                      INPUT saldo-terc.lote,                                                                                                  
                                                      INPUT (IF AVAIL saldo-estoq THEN saldo-estoq.dt-vali-lote                                                             
                                                                                  ELSE bf-nfe-it-nota-fisc-rec.item-dt-vali-lote),                                                   
                                                      INPUT saldo-terc.cod-refer,                              
                                                      INPUT saldo-terc.serie-docto,                            
                                                      INPUT saldo-terc.nro-docto,                                                                                          
                                                      INPUT saldo-terc.nat-operacao,                                                                                            
                                                      INPUT saldo-terc.sequencia,                                                                                           
                                                      INPUT saldo-terc.dt-retorno,                                                                       
                                                      INPUT p_preco_bruto,                                                                                                        
                                                      OUTPUT p_seq_it_docum_est,                                                                                   
                                                      INPUT-OUTPUT TABLE tt-it-doc-fisico,                                                                                                      
                                                      INPUT-OUTPUT TABLE tt-rat-lote,                                                                                                         
                                                      OUTPUT p_mensagem_erro).             

                            

                        END.


                    END.
                END.
            END. /* FOR EACH nfe-relac-doc-nota-rec WHERE.... */
        END. /* IF l-docto-nota = YES THEN.... */
            
        /* --- Entrada Recebimento Fiscal PADRAO --- */

        IF l-ordem          = NO AND
           l-ordem-ser-lote = NO AND
           l-ser-lote       = NO AND
           l-docto-nota     = NO AND
           l-docto-ser-lote = NO THEN 
        DO:
            /* --- Controla Sequencia do Item no Recebimento --- */

            ASSIGN p_sequencia = p_sequencia + 10.

            IF p_origem = 1 THEN /* Recebimento FÐsico */
            DO:
                /* --- Cria Tabela de Itens do Recebimento Fisico --- */
                                        

                RUN pi_item_receb_fisico (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                          INPUT p_sequencia,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-qtde,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-qCom,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-vProd,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-nr-ord-produ,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-cod-depos,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-cod-localiz,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-ser-lote,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-dt-vali-lote,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                          INPUT p_preco_bruto,
                                          OUTPUT p_seq_it_docum_est,
                                          INPUT-OUTPUT TABLE tt-it-doc-fisico,
                                          INPUT-OUTPUT TABLE tt-rat-lote,
                                          OUTPUT p_mensagem_erro).
            END.
            ELSE 
            DO:
                /* --- Cria Tabela de Itens do Recebimento Fiscal --- */

                RUN pi_item_receb_fiscal (INPUT ROWID(bf-nfe-it-nota-fisc-rec),
                                          INPUT p_sequencia,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-xPed,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-num-ordem,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-parcela-oc,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-qtde,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-qCom,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-vProd,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-vUnCom,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-conta-contabil,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-nr-ord-produ,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-cod-depos,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-cod-localiz,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-ser-lote,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-dt-vali-lote,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-cod-refer,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-serie-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-nro-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-nat-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-seq-comp,
                                          INPUT bf-nfe-it-nota-fisc-rec.item-data-comp,
                                          INPUT p_preco_bruto,
                                          INPUT-OUTPUT TABLE tt-item-doc-est,
                                          INPUT-OUTPUT TABLE tt-item-doc-est-aux,
                                          OUTPUT p_mensagem_erro).
            END.
        END.
    END. /* IF AVAIL bf-nfe-it-nota-fisc-rec THEN..... */

END PROCEDURE. /* pi_item_recebimento */

/**************************************************************************************************************************/

PROCEDURE pi_gera_rat_lote_fiscal :

    DEF INPUT  PARAM p_rowid_danfe      AS ROWID                            NO-UNDO.
    DEF INPUT  PARAM TABLE FOR tt-item-doc-est.
    DEF INPUT  PARAM TABLE FOR tt-rat-lote.
    DEF OUTPUT PARAM p_mensagem_erro    AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.
        
    RUN inbo/boin367.p PERSISTENT SET h_boin367.

    EMPTY TEMP-TABLE tt-item-doc-est-2.

    FOR EACH tt-item-doc-est:
        CREATE tt-item-doc-est-2.

        BUFFER-COPY tt-item-doc-est TO tt-item-doc-est-2.
    END.
        
    FOR EACH tt-item-doc-est-2 NO-LOCK
        BY tt-item-doc-est-2.sequencia :
        
        RUN openQueryStatic IN h_boin367 (INPUT "Main").
            
        FOR EACH rat-lote WHERE 
                 rat-lote.serie-docto    = tt-item-doc-est-2.serie-docto  AND 
                 rat-lote.nro-docto      = tt-item-doc-est-2.nro-docto    AND 
                 rat-lote.cod-emitente   = tt-item-doc-est-2.cod-emitente AND 
                 rat-lote.nat-operacao   = tt-item-doc-est-2.nat-operacao AND 
                 rat-lote.sequencia      = tt-item-doc-est-2.sequencia    NO-LOCK,
            FIRST tt-rat-lote WHERE 
                  tt-rat-lote.serie-docto  = rat-lote.serie-docto  AND 
                  tt-rat-lote.nro-docto    = rat-lote.nro-docto    AND 
                  tt-rat-lote.cod-emitente = rat-lote.cod-emitente AND 
                  tt-rat-lote.nat-operacao = rat-lote.nat-operacao AND 
                  tt-rat-lote.sequencia    = rat-lote.sequencia    NO-LOCK:
            
            RUN gotokey IN h_boin367 (INPUT rat-lote.serie-docto,
                                      INPUT rat-lote.nro-docto,
                                      INPUT rat-lote.cod-emitente,
                                      INPUT rat-lote.nat-operacao,
                                      INPUT rat-lote.sequencia).

            RUN deleteRecord IN h_boin367.
                
            IF RETURN-VALUE <> "NOK":U THEN 
            DO:
                /* --- Atualiza Nivel de Inspecao do Item Fornecedor --- */

                RUN pi_atualiza_niv_inspec (INPUT tt-item-doc-est-2.cod-emitente,
                                            INPUT tt-item-doc-est-2.it-codigo,
                                            INPUT ?,
                                            INPUT 2, /* --- Subtrai --- */
                                            OUTPUT p_mensagem_erro).

                DELETE tt-item-doc-est-2.
            END.
        END.
    END.
        
    FOR EACH tt-rat-lote NO-LOCK
        BY tt-rat-lote.lote 
        BY tt-rat-lote.cod-depos
        BY tt-rat-lote.cod-localiz :
        
        EMPTY TEMP-TABLE tt-rat-lote-aux.

        CREATE tt-rat-lote-aux.

        BUFFER-COPY tt-rat-lote TO tt-rat-lote-aux.
            
        RUN setRecord      IN h_boin367 (INPUT TABLE tt-rat-lote-aux).

        RUN emptyRowErrors IN h_boin367.

        RUN createRecord   IN h_boin367.
        
        IF RETURN-VALUE = "NOK":U THEN 
        DO:
            RUN getRowErrors IN h_boin367 (OUTPUT TABLE RowErrors).

            FIND FIRST RowErrors NO-LOCK NO-ERROR.

            IF AVAIL RowErrors THEN 
            DO:
                /* --- Verifica Parametros Globais --- */

                FIND FIRST nfe-param-rec WHERE 
                           nfe-param-rec.cod-parametro = "diretorios_recebimento_nfe" NO-LOCK NO-ERROR.

                IF AVAIL nfe-param-rec THEN 
                DO:
                    /* --- Verifica Permissao de Gera Item x Fornecedor --- */

                    FIND FIRST nfe-it-param-rec WHERE 
                               nfe-it-param-rec.cod-parametro      = nfe-param-rec.cod-parametro AND 
                               nfe-it-param-rec.cod-item-parametro = "dir_temporario"            NO-LOCK NO-ERROR.

                    IF AVAIL nfe-it-param-rec THEN 
                    DO:
                        FILE-INFO:FILE-NAME = nfe-it-param-rec.valor-1-item-parametro.

                        IF FILE-INFO:FULL-PATHNAME <> ?      AND
                           FILE-INFO:FILE-TYPE      = "DRW"  THEN 
                        DO:
                            OUTPUT STREAM s-log TO VALUE(nfe-it-param-rec.valor-1-item-parametro + "\log-rat-lote-fiscal_RA.txt") APPEND.

                            PUT STREAM s-log SKIP
                                "Serie: "   + STRING(tt-item-doc-est.serie-docto)   + " - " +
                                "Docto: "   + STRING(tt-item-doc-est.nro-docto)     + " - " +
                                "Fornec: "  + STRING(tt-item-doc-est.cod-emitente)  + " - " +
                                "Natur: "   + IF l-multi_natureza THEN string(tt-item-doc-est.nat-of) ELSE STRING(tt-item-doc-est.nat-operacao)   + " - " +
                                "Seq: "     + STRING(tt-item-doc-est.sequencia)     SKIP
                                "Erro: "    + STRING(Rowerrors.ErrorNumber)         + " - " +
                                "Desc: "    + STRING(Rowerrors.ErrorDescription)    FORMAT "X(300)".
                            OUTPUT STREAM s-log CLOSE.
                        END.
                    END.
                END.
            END.
        END.
        ELSE 
        DO:
            FIND FIRST rat-lote WHERE rat-lote.cod-emitente = tt-rat-lote.cod-emitente
                                  AND rat-lote.serie-docto  = tt-rat-lote.serie-docto
                                  AND rat-lote.nro-docto    = tt-rat-lote.nro-docto
                                  AND rat-lote.nat-operacao = tt-rat-lote.nat-operacao
                                  AND rat-lote.tipo-nota    = tt-rat-lote.tipo-nota
                                  AND rat-lote.sequencia    = tt-rat-lote.sequencia
                                  and rat-lote.lote         = tt-rat-lote.lote
                                  and rat-lote.cod-depos    = tt-rat-lote.cod-depos
                                  and rat-lote.cod-localiz  = tt-rat-lote.cod-localiz
                                NO-LOCK NO-ERROR.

            IF AVAIL rat-lote THEN 
            DO:
                /* --- Atualiza Nivel de Inspecao do Item Fornecedor --- */

                RUN pi_atualiza_niv_inspec (INPUT tt-rat-lote.cod-emitente,
                                            INPUT tt-rat-lote.it-codigo,
                                            INPUT ROWID(rat-lote),
                                            INPUT 1, /* --- Soma --- */
                                            OUTPUT p_mensagem_erro).
            END.
        END.
    END.

    DELETE PROCEDURE h_boin367.
        
    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "RatLotePadraoFiscal"
           tt-epc.cod-parameter = "tt-item-doc-est-2(HANDLE)"
           tt-epc.val-parameter = STRING(TEMP-TABLE tt-item-doc-est-2:HANDLE).

    CREATE tt-epc.

    ASSIGN tt-epc.cod-event     = "RatLotePadraoFiscal"
           tt-epc.cod-parameter = "p_rowid_danfe(ROWID)"
           tt-epc.val-parameter = STRING(p_rowid_danfe).

    {include\i-epc201.i "RatLotePadraoFiscal"}

    EMPTY TEMP-TABLE tt-item-doc-est-2.

END PROCEDURE. /* pi_gera_rat_lote_fiscal */

/**************************************************************************************************************************/

PROCEDURE pi_atualiza_niv_inspec :

    DEF INPUT  PARAM p_cod_emitente         AS INTE FORMAT ">>>>>>>>9"          NO-UNDO.    
    DEF INPUT  PARAM p_it_codigo            AS CHAR FORMAT "x(16)"              NO-UNDO.
    DEF INPUT  PARAM p_rowid_lote           AS ROWID                            NO-UNDO.
    DEF INPUT  PARAM p_acao                 AS INTE                             NO-UNDO.
    DEF OUTPUT PARAM p_mensagem_erro        AS CHAR FORMAT "x(200)" INITIAL ""  NO-UNDO.

    FIND FIRST item-fornec WHERE 
               item-fornec.cod-emitente = p_cod_emitente AND 
               item-fornec.it-codigo    = p_it_codigo    EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL item-fornec THEN 
    DO:
        /*** MOD.01 - Baione - 07/06/2011 ***/

        IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN 
        DO:
            CREATE nfe-item-fornec.

            BUFFER-COPY item-fornec TO nfe-item-fornec. 
        END.

        FIND FIRST nfe-item-fornec OF item-fornec EXCLUSIVE-LOCK NO-ERROR.

        FIND FIRST ITEM WHERE 
                   ITEM.it-codigo = item-fornec.it-codigo NO-LOCK NO-ERROR.

        IF AVAIL ITEM THEN 
        DO:
            IF item-fornec.tp-inspecao  = 4     AND      /* --- Preferencial Bom                 --- */
               ITEM.contr-qualid        = YES   THEN     /* --- Item com controle de qualidade   --- */
            DO:
                FIND FIRST param-cq NO-LOCK NO-ERROR.

                IF AVAIL param-cq THEN 
                DO:
                    /* --- Soma --- */

                    IF p_acao = 1 THEN 
                    DO:
                        IF item-fornec.niv-inspecao >= param-cq.ins-pref-x[1] THEN
                            ASSIGN item-fornec.niv-inspecao     = 0 /* --- NÐvel m?ximo atingido: zerar a contagem --- */
                                   nfe-item-fornec.niv-inspecao = item-fornec.niv-inspecao.
                        ELSE
                            ASSIGN item-fornec.niv-inspecao     = item-fornec.niv-inspecao + 1 /* --- Incrementar o nivel de inspe?ao --- */
                                   nfe-item-fornec.niv-inspecao = item-fornec.niv-inspecao.

                        RUN pi_limpa_epc.

                        CREATE tt-epc.

                        ASSIGN tt-epc.cod-event     = "SomaNivInspecao"
                               tt-epc.cod-parameter = "p_rowid_lote(ROWID)"
                               tt-epc.val-parameter = STRING(p_rowid_lote).

                        {include\i-epc201.i "SomaNivInspecao"}
                    END.
                    ELSE 
                    DO:
                        /* --- Subtrai --- */

                        IF item-fornec.niv-inspecao = 0 THEN
                            ASSIGN item-fornec.niv-inspecao     = param-cq.ins-pref-x[1] /* --- NÐvel m?ximo atingido --- */
                                   nfe-item-fornec.niv-inspecao = item-fornec.niv-inspecao.
                        ELSE
                            ASSIGN item-fornec.niv-inspecao     = item-fornec.niv-inspecao - 1 /* --- Diminuir o nivel de inspecao --- */
                                   nfe-item-fornec.niv-inspecao = item-fornec.niv-inspecao.
                   
                        RUN pi_limpa_epc.

                        CREATE tt-epc.

                        ASSIGN tt-epc.cod-event     = "SubtraiNivInspecao"
                               tt-epc.cod-parameter = "item-fornec(ROWID)"
                               tt-epc.val-parameter = STRING(ROWID(item-fornec)).

                        {include\i-epc201.i "SubtraiNivInspecao"}
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE. /* pi_atualiza_niv_inspec */

/**************************************************************************************************************************/

PROCEDURE pi_verifica_inconsist : /* @EMA! - 1.04.016 */
    
    DEF INPUT PARAM p_rowid_danfe AS ROWID NO-UNDO.
        
    DEF VAR i_tipo_inconsist AS INTE NO-UNDO.
    DEF VAR c_info_inconsist AS CHAR NO-UNDO.
    DEF VAR raw-param        AS RAW  NO-UNDO.
        
    FOR FIRST bfu-nfe-nota-fiscal-rec FIELDS(cgc emit-IE emit-UF chave-acesso-nfe) NO-LOCK WHERE 
              ROWID(bfu-nfe-nota-fiscal-rec) = p_rowid_danfe :
        /*
        FOR FIRST emitente FIELDS(ins-estadual estado char-1) NO-LOCK WHERE 
                  emitente.cgc = bfu-nfe-nota-fiscal-rec.cgc:*/

        FOR FIRST emitente FIELDS(ins-estadual estado char-1) NO-LOCK WHERE 
                  emitente.nome-abrev = bfu-nfe-nota-fiscal-rec.nome-abrev:


            RUN pi_grava_inconsist (INPUT bfu-nfe-nota-fiscal-rec.chave-acesso-nfe,
                                    INPUT '',
                                    INPUT 1,
                                    INPUT bfu-nfe-nota-fiscal-rec.emit-IE,
                                    INPUT REPLACE(REPLACE(REPLACE(emitente.ins-estadual, '.', ''), '-', ''), '/', '')).

            RUN pi_grava_inconsist (INPUT bfu-nfe-nota-fiscal-rec.chave-acesso-nfe,
                                    INPUT '',
                                    INPUT 2,
                                    INPUT bfu-nfe-nota-fiscal-rec.emit-UF,
                                    INPUT emitente.estado).
                
            RUN pi_grava_inconsist (INPUT bfu-nfe-nota-fiscal-rec.chave-acesso-nfe,
                                    INPUT '',
                                    INPUT 3,
                                    INPUT 'SIM',
                                    INPUT (IF SUBSTR(emitente.char-1,99,1) = '1' THEN 'SIM' ELSE 'NAO')).
        END.
            
        FOR EACH bf-nfe-it-nota-fisc-rec FIELDS(it-codigo item-NCM) NO-LOCK WHERE 
                 bf-nfe-it-nota-fisc-rec.chave-acesso-nfe = bfu-nfe-nota-fiscal-rec.chave-acesso-nfe,
            FIRST ITEM FIELDS(class-fiscal) WHERE 
                  ITEM.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK,
            EACH item-uni-estab FIELDS(char-1) WHERE 
                 item-uni-estab.it-codigo = bf-nfe-it-nota-fisc-rec.it-codigo NO-LOCK :
            
            /* Se o tipo de item para SPED for 7, 8, 9, 10 ou 99, nao ? necess?rio consistir a classificacao fiscal */

            IF LOOKUP(SUBSTR(item-uni-estab.char-1,133,1), '0,1,2,3,4,5,6') > 0 THEN 
            DO:
                RUN pi_grava_inconsist (INPUT bfu-nfe-nota-fiscal-rec.chave-acesso-nfe,
                                        INPUT bf-nfe-it-nota-fisc-rec.it-codigo,
                                        INPUT 4,
                                        INPUT bf-nfe-it-nota-fisc-rec.item-NCM,
                                        INPUT ITEM.class-fiscal).
            END.
        END.
        
        /* Se nao for o pr®prio relat®rio */

        IF NOT (PROGRAM-NAME(2) MATCHES '*esnfe800*') THEN 
        DO:
            FOR FIRST nfe-param-rec FIELDS(cod-parametro) WHERE 
                      nfe-param-rec.cod-parametro = "param_global" NO-LOCK:
                
                IF CAN-FIND(FIRST nfe-it-param-rec WHERE 
                                  nfe-it-param-rec.cod-parametro          = nfe-param-rec.cod-parametro AND 
                                  nfe-it-param-rec.cod-item-parametro     = "imprime_inconsistencias"   AND 
                                  nfe-it-param-rec.valor-1-item-parametro = "SIM"                       NO-LOCK) THEN 
                DO:
                    CREATE tt-param.

                    ASSIGN tt-param.destino          = 3
                           tt-param.arquivo          = SESSION:TEMP-DIR + 'ESNFE800.TMP'
                           tt-param.chave-acesso-nfe = bfu-nfe-nota-fiscal-rec.chave-acesso-nfe
                           tt-param.l-pendente       = YES
                           tt-param.l-corrigida      = NO.
                    
                    RAW-TRANSFER tt-param TO raw-param.

                    RUN dsc/ra/esp/esnfe800rp.p (raw-param).
                END.
            END.
        END.
    END. /* FOR FIRST bfu-nfe-nota-fiscal-rec..... */
        
END PROCEDURE. /* pi_verifica_inconsist */
    
/**************************************************************************************************************************/

PROCEDURE pi_grava_inconsist : /* @EMA! - 1.04.016 */
    
    DEF INPUT PARAM p_chave_acesso      AS CHAR     NO-UNDO.
    DEF INPUT PARAM p_it_codigo         AS CHAR     NO-UNDO.
    DEF INPUT PARAM p_cod_inconsist     AS INTE     NO-UNDO.
    DEF INPUT PARAM p_info_xml          AS CHAR     NO-UNDO.
    DEF INPUT PARAM p_info_cad          AS CHAR     NO-UNDO.
        
    FIND FIRST nfe-inconsist-rec WHERE 
               nfe-inconsist-rec.chave-acesso-nfe = p_chave_acesso  AND 
               nfe-inconsist-rec.it-codigo        = p_it_codigo     AND 
               nfe-inconsist-rec.cod-inconsist    = p_cod_inconsist EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL nfe-inconsist-rec  AND
       (p_info_xml <> p_info_cad)   THEN 
    DO:
        CREATE nfe-inconsist-rec.

        ASSIGN nfe-inconsist-rec.chave-acesso-nfe   = p_chave_acesso
               nfe-inconsist-rec.it-codigo          = p_it_codigo
               nfe-inconsist-rec.cod-inconsist      = p_cod_inconsist
               nfe-inconsist-rec.cod-situacao       = 0
               nfe-inconsist-rec.dt-situacao        = TODAY
               nfe-inconsist-rec.info-xml           = p_info_xml
               nfe-inconsist-rec.info-cad           = p_info_cad.
    END.
        
    IF AVAIL nfe-inconsist-rec      AND
       (p_info_xml = p_info_cad)    THEN 
    DO:
        ASSIGN nfe-inconsist-rec.cod-situacao  = 1
               nfe-inconsist-rec.dt-situacao   = TODAY
               nfe-inconsist-rec.info-xml      = p_info_xml
               nfe-inconsist-rec.info-cad      = p_info_cad.
    END.
        
END PROCEDURE. /* pi_grava_inconsist */
    
/**************************************************************************************************************************/

PROCEDURE pi_valida_upc:

    DEFINE INPUT PARAM p-ind-programa AS CHAR          NO-UNDO.
    DEFINE INPUT PARAM p-ind-event    AS CHAR          NO-UNDO.
    DEFINE INPUT PARAM p-ind-object   AS CHAR          NO-UNDO.
    DEFINE INPUT PARAM p-wgh-object   AS HANDLE        NO-UNDO.
    DEFINE INPUT PARAM p-wgh-frame    AS WIDGET-HANDLE NO-UNDO.
    DEFINE INPUT PARAM p-cod-table    AS CHAR          NO-UNDO.
    DEFINE INPUT PARAM p-row-table    AS ROWID         NO-UNDO.

    FIND FIRST prog_dtsul WHERE 
               prog_dtsul.cod_prog_dtsul = p-ind-programa NO-LOCK NO-ERROR.

    IF AVAIL prog_dtsul THEN 
    DO:
        IF SEARCH(prog_dtsul.nom_prog_upc) <> ? THEN
            RUN VALUE(prog_dtsul.nom_prog_upc) (INPUT p-ind-event,
                                                INPUT p-ind-object,
                                                INPUT p-wgh-object,
                                                INPUT p-wgh-frame,
                                                INPUT p-cod-table,
                                                INPUT p-row-table).
    END.

END PROCEDURE. /* pi_valida_upc */

/**************************************************************************************************************************/

PROCEDURE pi_limpa_epc :

    ASSIGN c-executa-bonfe001 = "PADRAO".

    EMPTY TEMP-TABLE tt-epc.

END PROCEDURE. /* pi_limpa_epc */

/**************************************************************************************************************************/

PROCEDURE pi_mensagem_erro :


    DEF INPUT  PARAM p_mensagem_erro AS CHAR FORMAT "x(200)" NO-UNDO.
    DEF OUTPUT PARAM p_acao          AS INTE                 NO-UNDO.

    ASSIGN p_acao             = INTE(ENTRY(1,CAPS(p_mensagem_erro),","))
           gsvc-tipo-mensagem = IF ENTRY(1,CAPS(p_mensagem_erro),",") = "1" THEN "ERRO" ELSE "ALERTA"
           gsvc-desc-mensagem = ENTRY(2,CAPS(p_mensagem_erro),",")
           gsvc-acao-mensagem = ENTRY(3,CAPS(p_mensagem_erro),",").



    RUN dsc\ra\esp\suemsg.w.


    ASSIGN gsvc-tipo-mensagem = ""
           gsvc-desc-mensagem = ""
           gsvc-acao-mensagem = "".

    /* F?bio Guedes - DSC - 04/10/2011 - Versao: 1.04.021
       Grava o erro na tabela para consulta futura no programa ESNFE200H.W
    
    CREATE nfe-log-erros.

    ASSIGN nfe-log-erros.data     = TODAY
           nfe-log-erros.tipo     = p_acao
           nfe-log-erros.motivo   = ENTRY(2,CAPS(p_mensagem_erro),",")
           nfe-log-erros.nome-arq = ?.
    */
END PROCEDURE. /* pi_mensagem_erro */


PROCEDURE pi_unidade_negocio:

    DEFINE INPUT  PARAMETER p-cod-estabel    AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER p-it-codigo      AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER p-ordem-compra   LIKE it-doc-fisico.numero-ordem NO-UNDO.
    DEFINE OUTPUT PARAMETER p-cod-unid-negoc AS CHAR NO-UNDO.

    DEFINE VARIABLE h-cdapi024           AS HANDLE      NO-UNDO.
    DEFINE VARIABLE c-cod-unid-negoc-aux AS CHARACTER   NO-UNDO.
        
    FIND FIRST ordem-compra NO-LOCK
        WHERE ordem-compra.numero-ordem  = p-ordem-compra NO-ERROR.

    IF AVAIL ordem-compra  AND  
       p-ordem-compra <> 0 THEN DO:

        ASSIGN p-cod-unid-negoc = ordem-compra.cod-unid-negoc.
        
    END.
    ELSE DO:
            
        RUN cdp/cdapi024.p PERSISTENT SET h-cdapi024.
            
        IF  VALID-HANDLE(h-cdapi024) THEN
            RUN RetornaUnidadeNegocioExternaliz IN h-cdapi024 (INPUT p-cod-estabel,
                                                               INPUT p-it-codigo,
                                                               INPUT "",
                                                               OUTPUT p-cod-unid-negoc).

        DELETE PROCEDURE h-cdapi024.

    END.
    
            


END PROCEDURE.

PROCEDURE PI_LIMPA_RETURN:

    RETURN "".

END.


PROCEDURE pi_grava_ean:

    DEFINE INPUT PARAMETER p-it-codigo AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER p-cod-ean   LIKE item-mat.cod-ean.


    FIND FIRST item-mat EXCLUSIVE-LOCK
        WHERE item-mat.it-codigo = p-it-codigo NO-ERROR.

    IF AVAIL item-mat THEN DO:

        IF p-cod-ean <> ""  AND
           p-cod-ean <> "0" THEN
            ASSIGN item-mat.cod-ean = p-cod-ean.

    END.


END PROCEDURE.

PROCEDURE pi_aliquota_icm:

    def input  param p-l-contrib-icms    like emitente.contrib-icms     no-undo.
    def input  param p-i-natureza-emit   like emitente.natureza         no-undo.
    def input  param p-c-estado-origem   like estabelec.estado          no-undo.
    def input  param p-c-pais-origem     like estabelec.pais            no-undo.
    def input  param p-c-estado-dest     like nota-fiscal.estado        no-undo.
    def input  param p-c-it-codigo       like item.it-codigo            no-undo.
    def input  param p-c-nat-operacao    like it-nota-fisc.nat-operacao no-undo.
    def output param p-de-aliquota       like natur-oper.aliquota-icm   no-undo.
        

    DEFINE VARIABLE h-bodi317im2bra AS HANDLE      NO-UNDO.
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.

    RUN dibo/bodi317im2bra.p PERSISTENT SET h-bodi317im2bra.

    RUN calculaAliquotaICMS in h-bodi317im2bra (INPUT  p-l-contrib-icms ,
                                                INPUT  p-i-natureza-emit,
                                                INPUT  p-c-estado-origem,
                                                INPUT  p-c-pais-origem  ,
                                                INPUT  p-c-estado-dest  ,
                                                INPUT  p-c-it-codigo    ,
                                                INPUT  p-c-nat-operacao ,
                                                OUTPUT p-de-aliquota    ,
                                                OUTPUT l-ok   ).

    

    DELETE PROCEDURE h-bodi317im2bra.

    /*Entrada: p-l-contrib-icms...: Indica se o emitente ì contribuinte de ICMS ou nao (emitente.contrib-icms).
                                 Faturamento: Passa o campo do cliente da nota fiscal.
                                 Recebimento: Passa o campo do emitente do estabelecimento ou YES quando o 
                                              estabelecimento nao est? relacionado a nenhum emitente.
            p-i-natureza-emit..: Indica a natureza do emitente (emitente.natureza)
                                 Faturamento: Passa o campo do cliente da nota fiscal.
                                 Recebimento: Passa o campo do emitente do estabelecimento ou 2 (jurÐdica) 
                                              quando o estabelecimento nao est? relacionado a nenhum emitente.
            p-c-estado-origem..: Refere-se ao estado onde a nota fiscal foi emitida.
                                 Faturamento: Passa o estado do estabelecimento.
                                 Recebimento: Passa o estado do emitente (fornecedor/cliente).
            p-c-pais-origem....: Refere-se ao paÐs onde a nota fiscal foi emitida.
                                 Faturamento: Passa o paÐs do estabelecimento.
                                 Recebimento: Passa o paÐs do emitente (fornecedor/cliente).
            p-c-estado-dest....: Refere-se ao estado para onde a nota fiscal foi emitida.
                                 Faturamento: Passa o estado do endereîo de entrega da nota (destinat?rio).
                                 Recebimento: Passa o estado do estabelecimento.
            p-c-it-codigo......: C®digo do item da nota.
            p-c-nat-operacao...: Natureza de operaîao do item da nota fiscal.
   SaÐda..: p-de-aliquota......: Retorna a alÐquota de ICMS para este item na nota;
            p-l-procedimento-ok: Retorna que o procedimento ocorreu por completo*/
END PROCEDURE.

PROCEDURE pi_saldo_ordem_nfe:

    DEFINE INPUT PARAMETER  p-numero-ordem LIKE ordem-compra.numero-ordem.
    DEFINE INPUT PARAMETER  p-parcela      LIKE prazo-compra.parcela.
    DEFINE INPUT PARAMETER  p-rw-item      AS ROWID.
    DEFINE OUTPUT PARAMETER p-quant-saldo  AS DEC.
        
    
    ASSIGN p-quant-saldo = 0.
        
    FIND FIRST nfe-it-nota-fisc-rec NO-LOCK
        WHERE  ROWID(nfe-it-nota-fisc-rec) = p-rw-item.
    
    IF AVAIL nfe-it-nota-fisc-rec  THEN DO:
        
        FOR EACH bf4-nfe-it-nota-fisc-rec NO-LOCK
            WHERE /*bf4-nfe-it-nota-fisc-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe
            AND  */ bf4-nfe-it-nota-fisc-rec.item-num-ordem   = p-numero-ordem
            AND   bf4-nfe-it-nota-fisc-rec.item-parcela     = p-parcela
            /*AND   bf4-nfe-it-nota-fisc-rec.seq-item        <> nfe-it-nota-fisc-rec.seq-item */:

            IF bf4-nfe-it-nota-fisc-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe AND 
               bf4-nfe-it-nota-fisc-rec.seq-item         = nfe-it-nota-fisc-rec.seq-item  THEN NEXT.

            FIND FIRST bf-nfe-nota-fiscal-rec OF bf4-nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.
            FIND FIRST nfe-dfe NO-LOCK
                WHERE nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso-nfe NO-ERROR.
                
            IF nfe-dfe.sit-erp >= 2 THEN NEXT.
            
            ASSIGN p-quant-saldo = p-quant-saldo + DEC(bf4-nfe-it-nota-fisc-rec.item-qtde).


            

        END.
            
        FOR EACH nfe-relac-ordem-rec NO-LOCK
            WHERE /*nfe-relac-ordem-rec.chave-acesso-nfe =  nfe-it-nota-fisc-rec.chave-acesso-nfe
            AND   nfe-relac-ordem-rec.seq-item         <> nfe-it-nota-fisc-rec.seq-item
            AND*/   nfe-relac-ordem-rec.numero-ordem     =  p-numero-ordem
            AND   nfe-relac-ordem-rec.parcela-oc       =  p-parcela:
            
            FIND FIRST bf-nfe-nota-fiscal-rec NO-LOCK
               WHERE bf-nfe-nota-fiscal-rec.chave-acesso-nfe = nfe-relac-ordem-rec.chave-acesso-nfe NO-ERROR.

            FIND FIRST nfe-dfe NO-LOCK
                WHERE nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso NO-ERROR.
                
            IF nfe-dfe.sit-erp >= 2 THEN NEXT.
            
            IF  nfe-relac-ordem-rec.chave-acesso-nfe =  nfe-it-nota-fisc-rec.chave-acesso-nfe AND   
                nfe-relac-ordem-rec.seq-item         = nfe-it-nota-fisc-rec.seq-item THEN NEXT.



            ASSIGN p-quant-saldo = p-quant-saldo + DEC(nfe-relac-ordem-rec.quantidade).

            


        END.

    END.


END PROCEDURE.

PROCEDURE pi_valida_fator_conversao:

    DEFINE INPUT  PARAMETER p-cod-emitente LIKE emitente.cod-emitente.
    DEFINE INPUT  PARAMETER p-item-do-forn LIKE item-fornec.item-do-forn.
    DEFINE INPUT  PARAMETER p-it-codigo    LIKE item-fornec.it-codigo.
    DEFINE OUTPUT PARAMETER p-fator        AS DEC NO-UNDO.
    DEFINE OUTPUT PARAMETER p-item-fornec  AS LOG NO-UNDO.

    FIND FIRST item-fornec no-lock
        WHERE item-fornec.cod-emitente = p-cod-emitente
        AND   item-fornec.item-do-forn = p-item-do-forn
        AND   item-fornec.it-codigo    = p-it-codigo  NO-ERROR.

    IF AVAIL item-fornec THEN DO:
    
        ASSIGN p-fator       = item-fornec.fator-conver * EXP(10,(item-fornec.num-casa-dec * (-1)))
               p-item-fornec = YES.
               

    END.
    ELSE
        ASSIGN p-item-fornec = NO.
                    
    
                    



END PROCEDURE.

PROCEDURE pi-valida-fifo:

    DEF INPUT  PARAM p_rowid_danfe   AS ROWID  NO-UNDO.
    DEF OUTPUT PARAM p_erro          AS LOG    NO-UNDO.
    DEFINE VARIABLE de-saldo-ordem AS DECIMAL  NO-UNDO.
        
    FIND FIRST param-re NO-LOCK
        WHERE param-re.usuario = c-seg-usuario NO-ERROR.    
    
    FIND FIRST nfe-nota-fiscal-rec no-lock
        WHERE ROWID(nfe-nota-fiscal-rec) = p_rowid_danfe  NO-ERROR.

    FOR EACH nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK
        WHERE nfe-it-nota-fisc-rec.item-fifo-oc = YES:

        ASSIGN de-saldo-ordem = 0.

        FIND FIRST tt-ordens-fifo 
            WHERE tt-ordens-fifo.it-codigo = nfe-it-nota-fisc-rec.it-codigo NO-ERROR.

        IF NOT AVAIL tt-ordens-fifo THEN DO:
        
            CREATE tt-ordens-fifo.
            ASSIGN tt-ordens-fifo.rw-item = ROWID(nfe-it-nota-fisc-rec).

        END.


        ASSIGN tt-ordens-fifo.it-codigo     = nfe-it-nota-fisc-rec.it-codigo 
               tt-ordens-fifo.de-quantidade = tt-ordens-fifo.de-quantidade + nfe-it-nota-fisc-rec.item-qtde
               tt-ordens-fifo.sequencia     = tt-ordens-fifo.sequencia + "/" + string(nfe-it-nota-fisc-rec.seq-item).
            
        

    END.



    FOR EACH tt-ordens-fifo:
        
        ASSIGN de-saldo-ordem = 0.
            
        FOR EACH ordem-compra no-lock
            WHERE ordem-compra.it-codigo = tt-ordens-fifo.it-codigo 
            AND   ordem-compra.situacao  = 2 /* --- 2 - Confirmada --- */:

            FOR EACH prazo-compra NO-LOCK
                 WHERE prazo-compra.numero-ordem                       = ordem-compra.numero-ordem  
                 AND   prazo-compra.situacao                           = 2 /* --- Confirmada --- */ 
                 AND   (prazo-compra.quant-saldo - prazo-compra.dec-1) > 0     
                BREAK BY prazo-compra.parcela:

                IF prazo-compra.data-entrega > TODAY + param-re.variacao THEN NEXT.

            
                ASSIGN de-saldo-ordem = de-saldo-ordem + (prazo-compra.quant-saldo - prazo-compra.dec-1 - fc-saldo-ordem(ordem-compra.numero-ordem , prazo-compra.parcela , tt-ordens-fifo.rw-item)) .


            END.
        END.
            
        IF de-saldo-ordem < tt-ordens-fifo.de-quantidade THEN DO:
            
            ASSIGN p_erro = YES.

            RUN pi_mensagem_erro (INPUT "1," + "ITEM Nao Possiu Saldo em Ordens Para Aplicar o FIFO, Sequencia " + string(tt-ordens-fifo.sequencia) + " Nao Possui Saldo de ordem de compra para aplicar a funcao FIFO." ,
                                   OUTPUT i-acao) /* --- 1 - Erro / 2 - Alerta --- */.
                
          


            RETURN NO-APPLY.
        END.

    END.


END PROCEDURE.

PROCEDURE PidisplayErrors:

    FOR EACH RowErrors NO-LOCK:

        MESSAGE "ErrorSequence    : " RowErrors.ErrorSequence    SKIP
                "ErrorNumber      : " RowErrors.ErrorNumber      SKIP
                "ErrorDescription : " RowErrors.ErrorDescription SKIP
                "ErrorParameters  : " RowErrors.ErrorParameters  SKIP
                "ErrorType        : " RowErrors.ErrorType        SKIP
                "ErrorHelp        : " RowErrors.ErrorHelp        SKIP
                "ErrorSubType     : " RowErrors.ErrorSubType    
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.


END PROCEDURE.












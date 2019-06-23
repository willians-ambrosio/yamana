/******************************************************************************************************************************************
** Programa: esp/esft4003rp.p
** Data    : 28-11-2015
** Autor   : Mauricio Cerqueira Miranda
** Objetivo: Gerar Automaticamente FT4003
********************************************************************************************************************************************/
ASSIGN CURRENT-LANGUAGE = CURRENT-LANGUAGE.

DEFINE BUFFER prog_dtsul         FOR emsfnd.prog_dtsul.
DEFINE BUFFER usuar_mestre       FOR emsfnd.usuar_mestre.
DEFINE BUFFER layout_impres_padr FOR emsfnd.layout_impres_padr.
DEFINE BUFFER layout_impres      FOR emsfnd.layout_impres.
DEFINE BUFFER imprsor_usuar      FOR emsfnd.imprsor_usuar.
/* include de controle de vers�o */
{include/i-prgvrs.i esft4003 "DEPS"}


define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    FIELD arquivo          AS CHAR
    field hora-exec        as integer
    FIELD cod-estab-de     AS CHAR
    FIELD cod-serie        AS CHAR
    FIELD cod-emitente     AS INT
    FIELD nat-operacao     AS CHAR
    FIELD qtd-itens-nf     AS INT.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de par�metros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*Carregando a tt-digita*/
create tt-param.
raw-transfer raw-param to tt-param.


/* carregando tt-digita */
For Each tt-raw-digita:
    Create tt-digita.
    Raw-transfer tt-raw-digita.raw-digita To tt-digita.
End.     

/*---------------------VARIAVEIS LOCAIS------------------------- */

DEFINE NEW GLOBAL SHARED VARIABLE v_cod_usuar_corren         AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.


/* ---------------------CONFIGURA EXCEL-------------------------- */
DEFINE VARIABLE chexcelApplication        AS COM-HANDLE.
DEFINE VARIABLE chWorkbook                AS COM-HANDLE.
DEFINE VARIABLE chWorksheet               AS COM-HANDLE.
DEFINE VARIABLE chChart                   AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange          AS COM-HANDLE.
DEFINE VARIABLE chWorkSheetRageSetup      AS COM-HANDLE.
DEFINE VARIABLE iCount                    AS INTEGER.
DEFINE VARIABLE cRange                    AS CHARACTER.
DEFINE VARIABLE i-linha                   AS INTEGER INITIAL 4.
/* -------------------------------------------------------------- */

def var h-bodi317                     as handle no-undo.
def var h-bodi317pr                   as handle no-undo.
def var h-bodi317sd                   as handle no-undo.
def var h-bodi317im1bra               as handle no-undo.
def var h-bodi317va                   as handle no-undo.
def var h-bodi317in                   as handle no-undo.
def var h-bodi317ef                   as handle no-undo.
def var l-proc-ok-aux                 as log    no-undo.
def var c-ultimo-metodo-exec          as char   no-undo.
def var c-cod-estabel                 as char   no-undo.
def var c-serie                       as char   no-undo.
def var c-it-codigo                   as char   no-undo.
def var c-seg-usuario                 as char   no-undo.
def var c-nome-abrev                  as char   no-undo.   
def var c-nr-pedcli                   as char   no-undo.
def var c-nat-operacao                as char   no-undo.
def var c-cod-canal-venda             as char   no-undo.
def var c-cod-refer                   as char   no-undo.
def var da-dt-emis-nota               as date   no-undo.
def var da-dt-base-dup                as date   no-undo.
def var da-dt-prvenc                  as date   no-undo.
def var i-seq-wt-docto                as int    no-undo.
def var i-nr-seq-nota                 as int    no-undo.
def var i-seq-wt-it-docto             as int    no-undo.
def var i-cont-itens                  as int    no-undo.
def var de-quantidade                 as dec    no-undo.
def var de-vl-preori-ped              as dec    no-undo.
def var de-val-pct-desconto-tab-preco as dec    no-undo.
def var de-per-des-item               as dec    no-undo.

/* Def temp-table de erros. Ela tb‚m est  definida na include dbotterr.i */
DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD errorsequence    AS INT
    FIELD errornumber      AS INT
    FIELD errordescription AS CHAR
    FIELD errorparameters  AS CHAR
    FIELD errortype        AS CHAR
    FIELD errorhelp        AS CHAR
    FIELD errorsubtype     AS CHAR.

/* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */
DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

DEF TEMP-TABLE tt-pedido NO-UNDO
    FIELD nr-pedcli LIKE ped-venda.nr-pedcli
    FIELD nr-pedido LIKE ped-venda.nr-pedido.

DEF TEMP-TABLE tt-import
    FIELD it-codigo   AS CHAR
	FIELD estab       AS CHAR
	FIELD cod-depos   AS CHAR 
    FIELD cod-localiz AS CHAR 
    FIELD vlr-item    AS DEC FORMAT ">>>>,>>>,>>9.9999"
    FIELD qtd-item    AS DEC FORMAT ">>>>,>>>,>>9.9999"
    FIELD l-passou    AS LOG.

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD cod-erro  AS CHAR
    FIELD tipo-erro AS CHAR
    FIELD desc-erro AS CHAR.

DEF TEMP-TABLE tt-saida NO-UNDO
    FIELD nr-pedido    LIKE ped-venda.nr-pedido
    FIELD nome-abrev   LIKE ped-venda.nome-abrev        
    FIELD dt-atual-cr  LIKE nota-fiscal.dt-atual-cr  
    FIELD dt-emis-nota LIKE nota-fiscal.dt-emis-nota 
    FIELD cod-estabel  LIKE nota-fiscal.cod-estabel  
    FIELD serie        LIKE nota-fiscal.serie        
    FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis  
    FIELD dt-entrega   LIKE ped-venda.dt-entrega     
    FIELD situacao     AS CHAR
    FIELD cod-sit-ped  AS CHAR
    FIELD ERRO         AS CHAR.
                                
DEFINE VARIABLE wc-arq          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cItem           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE Codigo          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE Descricao       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-linha         AS CHARACTER   NO-UNDO. 
DEFINE VARIABLE CFOP            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-lote          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-carrega       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-aux           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-continua      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-terminou      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-qtd           AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-cont    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-nula    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtd-item       AS INTEGER     NO-UNDO.
DEFINE VARIABLE dVl-preco       AS DEC     NO-UNDO.
DEFINE VARIABLE iSequencia      AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-erros         AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-acompanha-msg AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtd             AS DEC FORMAT ">>>>,>>>,>>9.9999"     NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.

DEFINE BUFFER bwt-it-docto FOR wt-it-docto.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Gerando Notas FT4003").
ASSIGN i-acompanha-msg = 0.

run pi-acompanhar in h-acomp(input "Importando Dados, aguarde").
RUN pi-import-excel.

ASSIGN i-acompanha-msg = 0.

OUTPUT TO "D:\temp\import.csv".
    FOR EACH tt-import:
        EXPORT DELIMITER ';' tt-import.

    END.
    OUTPUT CLOSE.

Criar:
DO TRANSACTION ON ERROR UNDO Criar, RETURN "NOK":
    DO WHILE l-terminou = FALSE:
    
    

        ASSIGN i-acompanha-msg = 0.
        RUN pi-acompanha-msgm( INPUT "Criando Cabe�alho",
                               INPUT i-acompanha-msg).
        
        RUN pi-cria-cabecalho.

        /*IF CAN-FIND(FIRST tt-erro WHERE tt-erro.tipo-erro = "Tipo: ERROR" ) THEN DO: 
            LEAVE.

        END.*/


        ASSIGN i-acompanha-msg = 0.
        RUN pi-acompanha-msgm( INPUT "Criando Itens NF",
                               INPUT i-acompanha-msg).
        RUN pi-cria-itens.
    
        IF CAN-FIND (FIRST tt-import WHERE tt-import.l-passou = FALSE) THEN
            ASSIGN l-terminou = FALSE.
        ELSE
            ASSIGN l-terminou = TRUE.
    
    END.
END.

ASSIGN i-acompanha-msg = 0.
OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erros_esft4003.csv").

    FOR EACH tt-erro:


        IF i-acompanha-msg = 3 THEN
            ASSIGN i-acompanha-msg = 0.
        ELSE
            ASSIGN i-acompanha-msg = i-acompanha-msg + 1.


        RUN pi-acompanha-msgm( INPUT "Exportando Erros",
                               INPUT i-acompanha-msg).

        EXPORT DELIMITER ';'
            tt-erro.
    END.

    MESSAGE "Foi gerado um arquivo de erros em: " + SESSION:TEMP-DIRECTORY + "erros_esft4003.csv"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

OUTPUT CLOSE.

RUN pi-finalizar IN h-acomp.



PROCEDURE pi-import-excel:

    INPUT FROM VALUE(tt-param.arq-entrada).

    REPEAT:

        IF i-acompanha-msg = 3 THEN
            ASSIGN i-acompanha-msg = 0.
        ELSE
            ASSIGN i-acompanha-msg = i-acompanha-msg + 1.


        RUN pi-acompanha-msgm( INPUT "Importando dados",
                               INPUT i-acompanha-msg).

        CREATE tt-import.

        IMPORT DELIMITER ';'
            tt-import.it-codigo
            tt-import.estab 
            tt-import.cod-depos
            tt-import.cod-localiz
            tt-import.vlr-item
            tt-import.qtd-item
            .

    END.

    INPUT CLOSE.


    FOR EACH tt-import
        WHERE tt-import.qtd-item <= 0:

        DELETE tt-import.
    END.

END PROCEDURE.


PROCEDURE pi-cria-cabecalho:



/* Inicializa‡Æo das BOS para C lculo */
        run dibo/bodi317in.p persistent set h-bodi317in.
        run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                         output h-bodi317sd,     
                                         output h-bodi317im1bra,
                                         output h-bodi317va).

        FIND FIRST emitente NO-LOCK
             WHERE emitente.cod-emitente = tt-param.cod-emitente NO-ERROR.

        /* Informa?es do embarque para clculo */
        assign c-seg-usuario     = v_cod_usuar_corren                                                        /* Usurio                    */
               c-cod-estabel     = tt-param.cod-estab-de   /* Estabelecimento do pedido  */
               c-serie           = tt-param.cod-serie                                                                    /* Sòie das notas            */
               c-nome-abrev      = emitente.nome-abrev       /* Nome abreviado do cliente  */
               c-nr-pedcli       = ""                                                                     /* Nr pedido do cliente       */
               da-dt-emis-nota   = today                                                                  /* Data de emiss? da nota    */
               c-nat-operacao    = tt-param.nat-operacao         /* Quando  ? busca do pedido */
               c-cod-canal-venda = '0'.                                                                   /* Quando  ? busca do pedido */
 
        run emptyRowErrors in h-bodi317in.
        run criaWtDocto in h-bodi317sd
                (input  c-seg-usuario, /* Usu rio do sistema */            
                 input  c-cod-estabel,      /* C¢digo do estabelecimento da nota */ 
                 input  c-serie,            /* S‚rie da nota fiscal */ 
                 input  "1",                /* Nr nf, para notas manuais */ 
                 input  c-nome-abrev,       /* C¢digo, nome abreviado ou CGC do cliente  */ 
                 input  ?,                  /* Nr do pedido de venda, quando existir */ 
                 input  4,                  /* Tp da nf 01:Sistema(com pedido ou com embarque) 02:Nota Manual  03:Diferen‡a de pre‡o  04:Complementar de mercadoria  50:Complementar de imposto  */ 
                 input  4003,               /* C¢digo do programa que gerou a nota Ex: Programa FT4724, passar o valor 4724 */ 
                 input  da-dt-emis-nota,    /* Data de emissÆo da nota fiscal     */ 
                 input  0,                  /* N£mero do embarque, quando existir */ 
                 input  c-nat-operacao,     /* C¢digo da natureza de opera‡Æo     */ 
                 input  c-cod-canal-venda,  /* C¢digo do canal de venda do cliente, quando existir  */ 
                 output i-seq-wt-docto,     /* Seqˆncia do documento (chave £nica)  */ 
                 output l-proc-ok-aux).     /* Execu‡Æo do m‚todo com sucesso ou nÆo */ 

        /* Busca poss¡veis erros que ocorreram nas valida‡äes */
        run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                 output table RowErrors).
        /* Pesquisa algum erro ou advertˆncia que tenha ocorrido */
        find first RowErrors no-lock no-error.

        /* Caso tenha achado algum erro ou advertˆncia, mostra em tela */
        if  avail RowErrors then DO:
            for each RowErrors:
        
                CREATE tt-erro.
                ASSIGN tt-erro.cod-erro  = "Erro: " + string(ErrorNumber)
                       tt-erro.tipo-erro = "Tipo: "      + string(ErrorSubtype)
                       tt-erro.desc-erro = "Descri��o: " + string(ErrorDescription)  + " - " + string(ErrorParameters) + "Ajuda: " + string(ErrorType).

            end. /*for each RowErrors*/
        END.
        /* Caso prob nas valid, n cont o proc*/
        if  not l-proc-ok-aux THEN NEXT. /*DO:
            MESSAGE 'erro'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            undo, leave.
        END.                         */
            

        run LocalizaWtDocto in h-bodi317sd(input  i-seq-wt-docto,
                                           output l-proc-ok-aux).

END PROCEDURE.
              

PROCEDURE pi-cria-itens:
    
    
    FOR EACH tt-import
       WHERE tt-import.qtd-item > 0
         AND tt-import.l-passou = FALSE:

        assign cItem     = tt-import.it-codigo
               iQtd-item = tt-import.qtd-item 
               dVl-preco = tt-import.vlr-item.

        ASSIGN i-cont-itens =  i-cont-itens + 1.

        IF i-cont-itens > tt-param.qtd-itens-nf THEN DO:

            ASSIGN i-cont-itens = 0.
            LEAVE.

        END.
        ELSE                                 
            ASSIGN tt-import.l-passou = TRUE.



        IF i-acompanha-msg = 3 THEN
            ASSIGN i-acompanha-msg = 0.
        ELSE
            ASSIGN i-acompanha-msg = i-acompanha-msg + 1.


        RUN pi-acompanha-msgm( INPUT "Criando Itens NF",
                               INPUT i-acompanha-msg).

        

        assign c-it-codigo                   = cItem     /* C¢digo do item     */
               c-cod-refer                   = ""        /* Referˆncia do item */
               de-quantidade                 = iQtd-item /* Quantidade         */
               de-vl-preori-ped              = dVl-preco /* Pre‡o unit rio     */
               de-val-pct-desconto-tab-preco = 0         /* Desconto de tabela */
               de-per-des-item               = 0.        /* Desconto do item   */

               iSequencia = i-cont-itens * 10.

        /* Limpar a tabela de erros em todas as BOS */
        run emptyRowErrors        in h-bodi317in.

        /* Disponibilizar o registro WT-DOCTO na bodi317sd */
        run localizaWtDocto in h-bodi317sd(input  i-seq-wt-docto,
                                           output l-proc-ok-aux).

        /* Cria um item para nota fiscal. */
        run criaWtItDocto in h-bodi317sd  (input  ?,                 /* Rowid da tabela ped-item ou ped-ent quando existir                                                                                                                                  */ 
                                           input  "",                /* Tabela do rowid passado no parƒmetro anterior - "ped-item": quando passado rowid do ped-item - "ped-ent": quando passado rowid do ped-ent - " ": quando nÆo existir pedido de venda */ 
                                           input  iSequencia,        /* Seqˆncia do item da nota. Passar quando nÆo existir pedido de venda                                                                                                                */ 
                                           input  c-it-codigo,       /* C¢digo do item da a nota. Passar quando nÆo existir pedido de venda                                                                                                                 */ 
                                           input  c-cod-refer,       /* C¢digo da referˆncia do item da a nota. Passar quando nÆo existir pedido de venda                                                                                                   */ 
                                           input  c-nat-operacao,    /* Natureza de opera‡Æo.                                                                                                                                                               */ 
                                           output i-seq-wt-it-docto, /* Seqˆncia do item do documento                                                                                                                                                      */ 
                                           output l-proc-ok-aux).    /* Execu‡Æo do m‚todo com sucesso ou nÆo                                                                                                                                               */

        /* Busca poss¡veis erros que ocorreram nas valida‡äes */
        run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                 output table RowErrors).

        /* Pesquisa algum erro ou advertˆncia que tenha ocorrido */
        find first RowErrors no-lock no-error.

        FIND FIRST bwt-it-docto
             WHERE bwt-it-docto.seq-wt-docto    = i-seq-wt-docto    
               AND bwt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto NO-ERROR.
        IF AVAIL(bwt-it-docto) THEN
            ASSIGN bwt-it-docto.quantidade = de-quantidade.

        FOR EACH wt-fat-ser-lote
           WHERE wt-fat-ser-lote.seq-wt-docto    = i-seq-wt-docto
             AND wt-fat-ser-lote.seq-wt-it-docto = i-seq-wt-it-docto:

            IF wt-fat-ser-lote.it-codigo = bwt-it-docto.it-codigo  THEN DO:
                IF wt-fat-ser-lote.cod-depos <> tt-import.cod-depos THEN
                    ASSIGN wt-fat-ser-lote.cod-depos = tt-import.cod-depos.

                IF wt-fat-ser-lote.cod-localiz <> tt-import.cod-localiz THEN
                    ASSIGN wt-fat-ser-lote.cod-localiz = tt-import.cod-localiz.

            END.

        END.
        /* Caso tenha achado algum erro ou advertˆncia, mostra em tela */
        if  avail RowErrors then
            for each RowErrors:


            CREATE tt-erro.
            ASSIGN tt-erro.cod-erro  = "Erro: " + string(ErrorNumber)
                   tt-erro.tipo-erro = "Tipo: "      + string(ErrorSubtype)
                   tt-erro.desc-erro = "Descri��o: " + string(ErrorDescription)  + " - " + string(ErrorParameters) + "Ajuda: " + string(ErrorType).

            end. /*for each RowErrors*/ 

        /* Caso ocorreu problema nas valida‡äes, nÆo continua o processo*/ 
        if  not l-proc-ok-aux then NEXT.
            /*undo, leave.*/

        /* Grava informa‡äes gerais para o item da nota */
        run gravaInfGeraisWtItDocto in h-bodi317sd 
               (input i-seq-wt-docto,                /* Seqˆncia do documento                     */ 
                input i-seq-wt-it-docto,             /* Seqˆncia do item do documento             */ 
                input de-quantidade,                 /* Quantidade do item da nota                 */ 
                input de-vl-preori-ped,              /* Valor do pre‡o unit rio informado          */ 
                input de-val-pct-desconto-tab-preco, /* Percentual de desconto da tabela de pre‡o  */ 
                input de-per-des-item).              /* Percentual de desconto do item da nota     */ 

        /* Limpar a tabela de erros em todas as BOS */
        run emptyRowErrors        in h-bodi317in.

        /* Disp. registro WT-DOCTO, WT-IT-DOCTO e WT-IT-IMPOSTO na bodi317pr */
        run localizaWtDocto       in h-bodi317pr(input  i-seq-wt-docto,
                                                 output l-proc-ok-aux).
        run localizaWtItImposto   in h-bodi317pr(input  i-seq-wt-docto,
                                                 input  i-seq-wt-it-docto,
                                                 output l-proc-ok-aux).
        run LocalizaWtNotaTrans in h-bodi317pr(input  i-seq-wt-docto, 
                                               input  i-nr-seq-nota,
                                               output l-proc-ok-aux).

        /* Atualiza dados c lculados do item */
        run atualizaDadosItemNota in h-bodi317pr(output l-proc-ok-aux).

        /* Busca poss¡veis erros que ocorreram nas valida‡äes */
        run devolveErrosbodi317pr in h-bodi317pr(output c-ultimo-metodo-exec,
                                                 output table RowErrors).

        /* Pesquisa algum erro ou advertˆncia que tenha ocorrido */
        find first RowErrors no-lock no-error.

        /* Caso tenha achado algum erro ou advertˆncia, mostra em tela */
        if  avail RowErrors then
            for each RowErrors:
                /* <Mostra os Erros/Advertˆncias encontradas */

            CREATE tt-erro.
            ASSIGN tt-erro.cod-erro  = "Erro: " + string(ErrorNumber)
                   tt-erro.tipo-erro = "Tipo: "      + string(ErrorSubtype)
                   tt-erro.desc-erro = "Descri��o: " + string(ErrorDescription)  + " - " + string(ErrorParameters) + "Ajuda: " + string(ErrorType).

            end. /*for each RowErrors*/

        /* Caso ocorreu problema nas valida‡äes, nÆo continua o processo */

        if  not l-proc-ok-aux THEN NEXT.
            /*undo, leave.*/

        /* Limpar a tabela de erros em todas as BOS */
        run emptyRowErrors        in h-bodi317in.

        
        

        /* Valida informa‡äes do item */
        run validaItemDaNota      in h-bodi317va(input  i-seq-wt-docto,
                                                 input  i-seq-wt-it-docto,
                                                 output l-proc-ok-aux).

        /* Busca poss¡veis erros que ocorreram nas valida‡äes */
        run devolveErrosbodi317va in h-bodi317va(output c-ultimo-metodo-exec,
                                                 output table RowErrors).

        /* Pesquisa algum erro ou advertˆncia que tenha ocorrido */
        find first RowErrors no-lock no-error.

        /* Caso tenha achado algum erro ou advertˆncia, mostra em tela */
        if  avail RowErrors THEN DO:
                    
            for each RowErrors:

            CREATE tt-erro.
            ASSIGN tt-erro.cod-erro  = "Erro: " + string(ErrorNumber)
                   tt-erro.tipo-erro = "Tipo: "      + string(ErrorSubtype)
                   tt-erro.desc-erro = "Descri��o: " + string(ErrorDescription)  + " - " + string(ErrorParameters) + "Ajuda: " + string(ErrorType).
            end. /*for each RowErrors*/
        END.
        /*ELSE DO:
            FOR EACH wt-fat-ser-lote
               WHERE wt-fat-ser-lote.seq-wt-docto    = i-seq-wt-docto
                 AND wt-fat-ser-lote.seq-wt-it-docto = i-seq-wt-it-docto:

                IF wt-fat-ser-lote.it-codigo = tt-import.it-codigo  THEN DO:
                    IF wt-fat-ser-lote.cod-depos <> tt-import.cod-depos THEN
                        ASSIGN wt-fat-ser-lote.cod-depos = tt-import.cod-depos.

                    IF wt-fat-ser-lote.cod-localiz <> tt-import.cod-localiz THEN
                        ASSIGN wt-fat-ser-lote.cod-localiz = tt-import.cod-localiz.

                END.

            END.
        END.*/
        /* Caso ocorreu problema nas valida‡äes, nÆo continua o processo */
        if  not l-proc-ok-aux THEN NEXT.
            /*undo, leave.  */   
    

    END.
        /* Finaliza‡Æo das BOS utilizada no c lculo */
    
        run finalizaBOS in h-bodi317in.

END PROCEDURE.

PROCEDURE pi-acompanha-msgm:
DEFINE INPUT PARAM c-mensagem       AS CHAR NO-UNDO.
DEFINE INPUT PARAM i-acompanhamento AS INT NO-UNDO.

CASE i-acompanha-msg:
    WHEN 0 THEN
        RUN pi-acompanhar in h-acomp(input c-mensagem + " , aguarde").
    WHEN 1 THEN
        RUN pi-acompanhar in h-acomp(input c-mensagem + " , aguarde.").
    WHEN 2 THEN
        RUN pi-acompanhar in h-acomp(input c-mensagem + " , aguarde..").
    WHEN 3 THEN
        RUN pi-acompanhar in h-acomp(input c-mensagem + " , aguarde...").
END CASE.

END PROCEDURE.

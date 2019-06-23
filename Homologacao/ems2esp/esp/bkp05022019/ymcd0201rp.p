&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMCD0201RP 11.6.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMCD0201RP 11.6.00.000}
&ENDIF

/* ***************************  Definitions  ************************** */
&global-define programa YMCD0201RP

/* Vari†veis Globais */    
{utp/ut-glob.i}
/*         v_cdn_empresa_evento */
/*         i-ep-codigo-usuario  */
/*         v_cdn_empres_usuar   */
{cdp/cdcfgman.i}

def var c-liter-par                  as character FORMAT "x(13)":U.
def var c-liter-sel                  as character FORMAT "x(10)":U.
def var c-liter-imp                  as character FORMAT "x(12)":U.    
def var c-destino                    as character FORMAT "x(15)":U.

DEFINE VARIABLE i-cont AS INTEGER NO-UNDO.

DEFINE STREAM st-log.

DEFINE TEMP-TABLE tt-emp NO-UNDO
    FIELD ep-codigo   AS CHARACTER.

Define Temp-table ttErrosConexao No-undo
    Field i-sequen As Integer
    Field cd-erro As Integer
    Field mensagem As Character Format "x(255)".

define temp-table tt-param NO-UNDO
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD arq-dest-dif     AS CHAR
    FIELD tempo-exec       AS INTEGER
    FIELD excel            AS LOGICAL.

/* Transfer Definitions */
def temp-table tt-raw-digita NO-UNDO
   field raw-digita      as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp         as handle no-undo.   


form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
    tt-param.arq-destino no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.

/* for each tt-raw-digita:                                 */
/*     create tt-digita.                                   */
/*     raw-transfer tt-raw-digita.raw-digita to tt-digita. */
/* end.                                                    */

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PAR∂METROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÄ«O * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESS«O * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usu†rio * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}
/*cleilton ems2cadme.*/
find ems2cadme.empresa
    where empresa.ep-codigo = v_cdn_empres_usuar
    no-lock no-error.
find first param-global no-lock no-error.

{utp/ut-liter.i Cadastros * }
assign c-sistema = return-value.
{utp/ut-liter.i CARGA_DE_ITENS_HARMONIZADOS * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "11.5.11":U
       c-revisao     = "001"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.


DEFINE BUFFER bitem-orig    FOR ITEM.
DEFINE BUFFER bitem-para    FOR ITEM.
DEFINE VARIABLE ia-ini-exec AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE tt-item-eliminado LIKE ITEM.

/* Definicao Temp-Tables Ordem de Producao ******************************/
/*********  *********/
{cpp/cpapi301.i}

/* Definicao Temp-Tables Reporte da Ordem *******************************/
{cpp/cpapi001.i}
{cpp/cpapi001.i1}

/* Definicao Temp-Table de Erros ****************************************/
{cdp/cd0666.i}

/* Temp-table */
DEFINE TEMP-TABLE tt-carga NO-UNDO
    FIELD ep-codigo         AS CHARACTER FORMAT "x(3)"    COLUMN-LABEL "Emp"
    FIELD id-klassmatt      AS CHARACTER FORMAT "x(9)"    COLUMN-LABEL "ID !KLASSMATT"
    FIELD it-codigo-orig    AS CHARACTER FORMAT "x(16)"   COLUMN-LABEL "Item atual"
    FIELD it-codigo-temp    AS CHARACTER FORMAT "x(16)"   COLUMN-LABEL "Item (TEMP)"
    FIELD descricao         AS CHARACTER FORMAT "x(40)"   COLUMN-LABEL "Descriá∆o_Resumida"
    FIELD narrativa         AS CHARACTER FORMAT "x(400)"  COLUMN-LABEL "Descricao_Completa"
    FIELD fm-codigo         AS CHARACTER FORMAT "x(12)"   COLUMN-LABEL "SubGrupo"
    FIELD cod-refer         AS CHARACTER FORMAT "x(8)"    COLUMN-LABEL "Codigo Complem"
    FIELD class-fiscal      AS CHARACTER FORMAT "x(10)"   COLUMN-LABEL "NCM"
    FIELD ex-ncm            AS CHARACTER FORMAT "x(10)"   COLUMN-LABEL "Exceá∆o_NCM"
    FIELD fm-cod-com        AS CHARACTER FORMAT "x(12)"   COLUMN-LABEL "Familia"

    FIELD fm-codigo-orig    AS CHARACTER FORMAT "x(12)"   COLUMN-LABEL "SubGrupo_1"
    FIELD cod-localiz-orig  LIKE ITEM.cod-localiz
    FIELD log-carga         AS CHARACTER FORMAT "x(4000)" COLUMN-LABEL "Erro/Advertància"
    FIELD it-codigo-para    AS CHARACTER FORMAT "x(16)"   COLUMN-LABEL "Novo c¢digo"
    FIELD it-sit-para       AS INTEGER /* 1 - N∆o existe, 2 - Obsoleto, 3 - Ativo */
    FIELD old-ge-codigo     AS INTEGER
    FIELD finalizado        AS LOGICAL INITIAL NO
    FIELD executado         AS LOGICAL INITIAL NO
    FIELD desc-item-orig    AS CHARACTER FORMAT "x(60)"
    FIELD fator-conv        AS DECIMAL FORMAT ">,>>9.99999" DECIMALS 5  COLUMN-LABEL "Fator Convers∆o" INITIAL 1
    FIELD un-de             AS CHARACTER FORMAT "x(4)"    COLUMN-LABEL "UN!(DE)"
    FIELD un-para           AS CHARACTER FORMAT "x(4)"    COLUMN-LABEL "UN!(PARA)"    
    FIELD ge-codigo         AS INTEGER   FORMAT "99"      COLUMN-LABEL "Grp!Est"
    FIELD cod-orig          AS INTEGER   FORMAT ">9"      COLUMN-LABEL "Origem"
    FIELD controla_estoque  AS INTEGER   FORMAT ">9"    COLUMN-LABEL "Controla!Qtde Estoq?"
    FIELD quant-segur       AS DECIMAL FORMAT ">>>>,>>9.9999" DECIMALS 4  COLUMN-LABEL "Qtde !Min Est?"
    FIELD cod-servico       AS INTEGER FORMAT ">>>>9"   COLUMN-LABEL "C¢d Serv"
    FIELD tipo-item         AS CHARACTER FORMAT "x(2)"    COLUMN-LABEL "Tipo"
    FIELD cod-depto         AS INTEGER FORMAT ">>>>>>>9"    COLUMN-LABEL "Departamento"
    FIELD char-2            AS CHARACTER
    FIELD cod-localiz       LIKE ITEM.cod-localiz
    .

DEFINE TEMP-TABLE tt-item-estab
       FIELDS it-codigo-de    LIKE ITEM.it-codigo
       FIELDS it-codigo-para  LIKE ITEM.it-codigo
       FIELDS cod-estabel     LIKE item-uni-estab.cod-estabel
       FIELDS cod-obsoleto    LIKE item-uni-estab.cod-obsoleto.


def temp-table tt-versao-integr no-undo
       field cod-versao-integracao as integer FORMAT "999"
       field ind-origem-msg        as integer FORMAT "99".
 
def temp-table tt-erros-geral no-undo
       field identif-msg           as char    FORMAT "x(60)"
       field num-sequencia-erro    as integer FORMAT "999"
       field cod-erro              as integer FORMAT "99999"   
       field des-erro              as char    FORMAT "x(60)"
       field cod-maq-origem        as integer FORMAT "999"
       field num-processo          as integer FORMAT "999999999".
 

/* {cdp/cdapi244.i} */
def temp-table tt-item no-undo like item 
    field cod-maq-origem   as   integer FORMAT "9999"
    field num-processo     as   integer FORMAT ">>>>>>>>9" initial 0
    field num-sequencia    as   integer FORMAT ">>>>>9"    initial 0
    field ind-tipo-movto   as   integer FORMAT "99"        initial 1
    field cod-erro         as   integer FORMAT "99999" 
    field des-erro         as   char    FORMAT "x(60)"
    INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                num-processo
                                num-sequencia.

/*CD0205RP*/
define temp-table tt-paramCD0205 no-undo
    field destino          as integer
    field arquivo          as char
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field l-todos          as logical.

define temp-table tt-altera no-undo
    field it-codigo     like item.it-codigo
    field descricao     as char FORMAT "x(60)"
    field un            like item.un
    field new-it-codigo like item.it-codigo
    field new-un        like item.un
    field fator-conv    as decimal FORMAT ">,>>9.99999" 
    field ge-codigo     like item.ge-codigo
    field old-ge-codigo like item.ge-codigo.

define temp-table tt-elimina no-undo
    field it-codigo     like item.it-codigo
    field un            like item.un
    field descricao     as char FORMAT "x(60)".

define temp-table tt-digitaCD0205 no-undo
    field it-codigo     like item.it-codigo
    field descricao     as char FORMAT "x(60)"
    field un            like item.un
    field new-it-codigo like item.it-codigo
    field new-un        like item.un
    field fator-conv    as decimal FORMAT ">,>>9.99999"
    field ge-codigo     like item.ge-codigo
    field old-ge-codigo like item.ge-codigo
    field tipo-operacao as char FORMAT "x".

def temp-table tt-raw-digitaCD0205
   field raw-digita      as raw.

def temp-table tt-raw-eliminaCD0205
   field raw-elimina      as raw.

DEFINE TEMP-TABLE tt-item-uni-estab LIKE item-uni-estab
       FIELDS it-codigo-para LIKE ITEM.it-codigo.

DEF TEMP-TABLE tt-uni-estab-orig LIKE item-uni-estab.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 12.96
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
OUTPUT STREAM st-log TO VALUE("D:\temp\rpw\troca_item_novo.txt") NO-CONVERT NO-MAP APPEND.
    
do on stop undo, leave:
    {include/i-rpout.i &tofile=tt-param.arq-destino}

    IF NOT tt-param.excel THEN DO:
        view frame f-cabec.
        view frame f-rodape.    
    END.
    run utp/ut-acomp.p persistent set h-acomp.  
    
/*     {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c} */
    ASSIGN ia-ini-exec = TIME.
    
    run pi-inicializar in h-acomp (input "Carga de item...":U). 
    
    /*:T --- Colocar aqui o c¢digo de impress∆o --- */
    run pi-acompanhar in h-acomp (input "Carregando Planilha...":U).

    EMPTY TEMP-TABLE tt-item-uni-estab.

    RUN pi-carregar.
    FIND FIRST tt-carga NO-ERROR.
    IF NOT AVAIL tt-carga THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Planilha de Carga sem dados~~" + 
                                 "O arquivo " + QUOTER(tt-param.arq-entrada) + 
                                 " n∆o tinha nenhuma informaá∆o, verifique e tente novamente..").
        run pi-finalizar in h-acomp.
        {include/i-rpclo.i}
        RETURN "NOK".
    END.
    
    run pi-acompanhar in h-acomp (input "Validaá‰es B†sicas":U).
    RUN pi-validar.

    
/* /* /*     run pi-acompanhar in h-acomp (input "Eliminando Obsoletos":U). */  */  */
/* /* /*     RUN pi-obsoleto.                                               */  */  */

    run pi-acompanhar in h-acomp (input "Mudando c¢digo":U).
    RUN pi-mudarcodigo.
    
    run pi-acompanhar in h-acomp (input "Verificando itens ativos":U).
    RUN pi-itemativo.
    
    run pi-acompanhar in h-acomp (input "Gravando de-para":U).
    RUN pi-gravardepara.
    
    run pi-acompanhar in h-acomp (input "Identificando empresas para replicaá∆o":U).
    RUN pi-empresas.
    
    run pi-acompanhar in h-acomp (input "Replicando demais empresas":U).
    RUN pi-replicar.
    
    run pi-acompanhar in h-acomp (input "Retornando WS":U).
    RUN pi-retornows.
    
    run pi-acompanhar in h-acomp (input "Exportando itens n∆o executados":U).
    RUN pi-diferenca.


    FOR EACH tt-item-uni-estab
             NO-LOCK:
       run pi-acompanhar in h-acomp (input "Corrigindo item-uni-estab: ":U + tt-item-uni-estab.it-codigo-para).

       FIND FIRST item-uni-estab
            WHERE item-uni-estab.cod-estabel = tt-item-uni-estab.cod-estabel AND
                  item-uni-estab.it-codigo   = tt-item-uni-estab.it-codigo-para
            EXCLUSIVE-LOCK NO-ERROR.
       IF AVAILABLE(item-uni-estab) THEN DO:
          BUFFER-COPY tt-item-uni-estab EXCEPT cod-estabel it-codigo cod-obsoleto TO item-uni-estab.
       END.
    END.
    
    run pi-acompanhar in h-acomp (input "Gerando resultado":U).
    IF tt-param.excel THEN
        RUN pi-excel.
    ELSE
        RUN pi-resultado.
    
    run pi-finalizar in h-acomp.

    PUT UNFORMATTED SKIP(1) 
        "Tempo de Execuá∆o em minutos: " STRING(( TIME - ia-ini-exec) / 60,"9999") SKIP.

    OUTPUT STREAM st-log CLOSE.

    {include/i-rpclo.i}
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-carregar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carregar Procedure 
PROCEDURE pi-carregar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER b-item-localiz FOR ITEM.

EMPTY TEMP-TABLE tt-carga.
INPUT FROM VALUE(tt-param.arq-entrada) NO-CONVERT.
REPEAT:
    CREATE tt-carga.
    IMPORT DELIMITER ";"
           tt-carga.ep-codigo      
           tt-carga.id-klassmatt   
           tt-carga.it-codigo-orig 
           tt-carga.it-codigo-temp 
           tt-carga.descricao      
           tt-carga.narrativa      
           tt-carga.fm-codigo      
           tt-carga.cod-refer      
           tt-carga.class-fiscal   
           tt-carga.ex-ncm         
           tt-carga.fm-cod-com.     
    run pi-acompanhar in h-acomp (input "Carregando item:":U + tt-carga.it-codigo-orig).

END.
INPUT CLOSE.
FOR EACH tt-carga:

    ASSIGN tt-carga.class-fiscal = REPLACE(tt-carga.class-fiscal,".","").
    IF tt-carga.ep-codigo BEGINS "Emp" OR 
       TRIM(tt-carga.ep-codigo) = ""   THEN
        DELETE tt-carga.


    FIND FIRST b-item-localiz NO-LOCK
         WHERE b-item-localiz.it-codigo = tt-carga.it-codigo-orig NO-ERROR.
    IF AVAIL(b-item-localiz) THEN DO:
        ASSIGN tt-carga.cod-localiz = b-item-localiz.cod-localiz.
    END.

    /* ELG - 01-02-16 */
    FIND ITEM WHERE
         ITEM.it-codigo =   tt-carga.it-codigo-orig NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN DO:
        ASSIGN tt-carga.fm-codigo-orig      =   ITEM.fm-codigo
               tt-carga.cod-localiz-orig    =   ITEM.cod-localiz.
    END.

    /* ELG - 01-02-16*/
    FOR EACH item-uni-estab WHERE
             item-uni-estab.it-codigo   =   tt-carga.it-codigo-orig NO-LOCK.

        FIND FIRST tt-uni-estab-orig WHERE
                   tt-uni-estab-orig.cod-estabel  =   item-uni-estab.cod-estabel AND
                   tt-uni-estab-orig.it-codigo    =   item-uni-estab.it-codigo NO-ERROR.
        IF NOT AVAIL tt-uni-estab-orig THEN DO:
            CREATE tt-uni-estab-orig.
            BUFFER-COPY item-uni-estab TO tt-uni-estab-orig.
        END.

    END.
    
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cd0205rp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cd0205rp Procedure 
PROCEDURE pi-cd0205rp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE raw-paramCD0205 AS RAW      NO-UNDO.

    EMPTY TEMP-TABLE tt-paramCD0205.
    EMPTY TEMP-TABLE tt-digitaCD0205.
                                    

    create tt-paramCD0205.
    assign tt-paramCD0205.usuario    = c-seg-usuario
           tt-paramCD0205.destino    = 2 /* Arquivo*/
           tt-paramCD0205.data-exec  = TODAY 
           tt-paramCD0205.hora-exec  = TIME 
           tt-paramCD0205.l-todos    = NO.
    
    ASSIGN tt-paramCD0205.arquivo  = IF tt-paramCD0205.arquivo <> "" THEN tt-paramCD0205.arquivo ELSE SESSION:TEMP-DIRECTORY + ("CD0205" + STRING(TIME) + ".carga").

    IF i-num-ped-exec-rpw <> 0 THEN
       ASSIGN tt-paramCD0205.arquivo  = replace(STRING(TODAY,'99/99/9999'),'/','') +  STRING(TIME) + ".carga".

    /*IF tt-paramCD0205.arquivo <> "" THEN
        ASSIGN tt-paramCD0205.arquivo  = tt-paramCD0205.arquivo + "/CD0205" + STRING(TIME) + ".carga".
    ELSE
        ASSIGN tt-paramCD0205.arquivo  = SESSION:TEMP-DIRECTORY + "CD0205" + STRING(TIME) + ".carga" .

        ASSIGN FILE-INFO:FILE-NAME = SESSION:TEMP-DIRECTORY.
        IF FILE-INFO:FULL-PATHNAME = "?" THEN DO:

            FIND FIRST usuar_mestre NO-LOCK
                WHERE usuar_mestre.cod_usuar = v_cod_usuar_corren NO-ERROR.

            ASSIGN  tt-paramCD0205.arquivo = usuar_mestre.nom_dir_spool + "/CD0205"  + STRING(TIME) + ".carga" .
        END.
            */

    for each tt-altera:
        create tt-digitaCD0205.
        assign tt-digitaCD0205.it-codigo     = tt-altera.it-codigo
               tt-digitaCD0205.descricao     = tt-altera.descricao
               tt-digitaCD0205.un            = tt-altera.un
               tt-digitaCD0205.new-it-codigo = tt-altera.new-it-codigo 
               tt-digitaCD0205.new-un        = tt-altera.new-un
               tt-digitaCD0205.fator-conv    = tt-altera.fator-conv
               tt-digitaCD0205.ge-codigo     = tt-altera.ge-codigo     
               tt-digitaCD0205.old-ge-codigo = tt-altera.old-ge-codigo
               tt-digitaCD0205.tipo-operacao = "A".
    end.

    for each tt-elimina:
        create tt-digitaCD0205.
        assign tt-digitaCD0205.it-codigo     = tt-elimina.it-codigo   
               tt-digitaCD0205.un            = tt-elimina.un          
               tt-digitaCD0205.descricao     = tt-elimina.descricao
               tt-digitaCD0205.tipo-operacao = "E".
    end.

    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

    raw-transfer tt-paramCD0205    to raw-paramCD0205.

    for each tt-raw-digitaCD0205:
        delete tt-raw-digitaCD0205.
    end.
    for each tt-digitaCD0205:
        create tt-raw-digitaCD0205.
        raw-transfer tt-digitaCD0205 to tt-raw-digitaCD0205.raw-digita.
    end.  

    PUT STREAM st-log STRING(TIME,'hh:mm:ss') "- inicio cdp/cd0205rp" SKIP.

    run cdp/cd0205rp.p (input raw-paramCD0205,
                        input table tt-raw-digitaCD0205).

    PUT STREAM st-log STRING(TIME,'hh:mm:ss') "- fim cdp/cd0205rp" SKIP.


    FOR EACH tt-altera:
        FIND FIRST bitem-para WHERE 
                   bitem-para.it-codigo = tt-altera.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL bitem-para THEN
        DO:       
            /* Begins Jan/2018 - Willians Ambrosio DKP */
            FIND FIRST es-klassmatt-integr WHERE
                       es-klassmatt-integr.idklassmatt = 999999999                                   AND
                       es-klassmatt-integr.dt-trans    = TODAY                                       AND
                       es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","")     EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL es-klassmatt-integr THEN
            DO:
               CREATE es-klassmatt-integr.
               BUFFER-COPY bitem-para           TO es-klassmatt-integr
                                            ASSIGN es-klassmatt-integr.idklassmatt = 999999999                               
                                                   es-klassmatt-integr.dt-trans    = TODAY                                   
                                                   es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","") 
                                                   es-klassmatt-integr.log-retorno   = "YMCD0201RP.P - TIPO DE CONTROLE ATUALIZADO Tipo-Contr = "  + STRING(bitem-para.tipo-contr)      
                                                   es-klassmatt-integr.statusRetorno = "N".                                              
            END.
            /* End Jan/2018 - Willians Ambrosio DKP */
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-diferenca) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-diferenca Procedure 
PROCEDURE pi-diferenca :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT TO VALUE(tt-param.arq-dest-dif) NO-CONVERT.
FOR EACH tt-carga
    WHERE tt-carga.executado = NO:
    EXPORT DELIMITER ";"
           tt-carga.ep-codigo        
           tt-carga.id-klassmatt     
           tt-carga.it-codigo-orig   
           IF tt-carga.it-codigo-temp = "" THEN tt-carga.it-codigo-para ELSE tt-carga.it-codigo-temp
/*            tt-carga.ge-codigo  */
/*            tt-carga.un-de      */
/*            tt-carga.un-para    */
/*            tt-carga.fator-conv */
           tt-carga.descricao        
           tt-carga.narrativa        
           tt-carga.fm-codigo        
           tt-carga.cod-refer        
/*            tt-carga.cod-orig */
           tt-carga.class-fiscal     
           tt-carga.ex-ncm           
/*            tt-carga.controla_estoque */
/*            tt-carga.quant-segur      */
/*            tt-carga.cod-servico      */
           tt-carga.fm-cod-com       
/*            tt-carga.tipo-item */
/*            tt-carga.cod-depto */
           .    
END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-empresas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-empresas Procedure 
PROCEDURE pi-empresas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-emp.
FOR EACH ems2cadme.empresa FIELDS(ep-codigo data-inicio data-fim) NO-LOCK
   /*WHERE empresa.data-inicio <= TODAY
     AND empresa.data-fim    >= TODAY*/,
    EACH bco_empres
    WHERE bco_empres.cod_bco_logic = "ems2cademp"
      AND bco_empres.cod_empresa   = empresa.ep-codigo NO-LOCK:
    FIND FIRST es-emp-desativada 
         WHERE es-emp-desativada.ep-codigo = bco_empres.cod_empresa NO-ERROR.
   IF AVAIL(es-emp-desativada) THEN NEXT.


    FIND FIRST trad_org_ext WHERE trad_org_ext.cod_tip_unid_organ      = "998"                  
                            AND   trad_org_ext.cod_matriz_trad_org_ext = "ems2"            
                            AND   trad_org_ext.cod_unid_organ_ext          = bco_empres.cod_empresa NO-LOCK NO-ERROR. 
    IF AVAIL(trad_org_ext) THEN DO:
        FIND FIRST ems5.unid_organ NO-LOCK
             WHERE ems5.unid_organ.cod_tip_unid_organ      = "998"
               AND ems5.unid_organ.cod_unid_organ = trad_org_ext.cod_unid_organ NO-ERROR.
        IF AVAIL(ems5.unid_organ) THEN DO:
    
            IF ems5.unid_organ.dat_fim_valid < TODAY THEN NEXT.
    
        END.
    END.

    IF v_cdn_empres_usuar = bco_empres.cod_empresa THEN
        NEXT.
    /*     DISP bco_empres.cod_empresa. */
    CREATE tt-emp.
    ASSIGN tt-emp.ep-codigo = bco_empres.cod_empresa.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel Procedure 
PROCEDURE pi-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EXPORT DELIMITER ";"
       "Empresa"
       "Id Klassmatt"
       "Item Origem"  
       "Item Tempor†rio"  
       "Item Padronizado"  
       "Grupo de Estoque"       
       "Unidade (DE)"           
       "Unidade (PARA)"         
       "Fator de Convers∆o"      
       "Descriá∆o Item"       
       "Mensagens" .
                              
FOR EACH tt-carga
    WHERE tt-carga.executado = TRUE:

    run pi-acompanhar in h-acomp (input "Imprimindo:":U + tt-carga.it-codigo-para).

    IF tt-param.todos = 2 AND tt-carga.log-carga = "" THEN
        NEXT.

    ASSIGN tt-carga.log-carga = TRIM(tt-carga.log-carga,"| ").
    EXPORT DELIMITER ";"
           tt-carga.ep-codigo       
           tt-carga.id-klassmatt    
           tt-carga.it-codigo-orig  
           tt-carga.it-codigo-temp  
           tt-carga.it-codigo-para  
           tt-carga.ge-codigo       
           tt-carga.un-de           
           tt-carga.un-para         
           tt-carga.fator-conv      
           tt-carga.descricao       
           tt-carga.log-carga.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-gravardepara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gravardepara Procedure 
PROCEDURE pi-gravardepara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-carga
    WHERE tt-carga.finalizado = TRUE:

    CREATE es-de-para-it-padr.
    ASSIGN es-de-para-it-padr.ep-codigo      = tt-carga.ep-codigo
           es-de-para-it-padr.id-klassmatt   = INTEGER(tt-carga.id-klassmatt)
           es-de-para-it-padr.it-codigo-orig = tt-carga.it-codigo-orig
           es-de-para-it-padr.it-codigo-padr = tt-carga.it-codigo-para
           es-de-para-it-padr.it-codigo-temp = tt-carga.it-codigo-temp
           es-de-para-it-padr.desc-item-orig = tt-carga.desc-item-orig
           .           
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-itemativo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-itemativo Procedure 
PROCEDURE pi-itemativo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE l-temordem  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-saldoterc AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-saldo     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-periodo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-periodo  AS DATE        NO-UNDO.

DEFINE BUFFER bext-item-cfa  FOR ext-item-cfa.
DEFINE BUFFER bconsumo-estab FOR consumo-estab.

ASSIGN dt-periodo = ADD-INTERVAL(NOW,-12,"months")
       c-periodo  = STRING(YEAR(dt-periodo),"9999") + STRING(MONTH(dt-periodo),"99").

CREATE tt-versao-integr. 
ASSIGN tt-versao-integr.cod-versao-integracao = 001     
       tt-versao-integr.ind-origem-msg        = 01.  

carga:
FOR EACH tt-carga
    WHERE tt-carga.log-carga   = ""
      AND tt-carga.it-sit-para = 3 /* ATIVO */:

    run pi-acompanhar in h-acomp (input "Item Ativo:":U + tt-carga.it-codigo-orig).

    FIND FIRST bitem-orig
        WHERE bitem-orig.it-codigo = tt-carga.it-codigo-orig NO-LOCK NO-ERROR.

    FOR FIRST bitem-para FIELDS(cod-estabel it-codigo)
        WHERE bitem-para.it-codigo = tt-carga.it-codigo-para NO-LOCK: END.
    IF NOT AVAIL bitem-para THEN DO:
        ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                    "Item Para " + QUOTER(tt-carga.it-codigo-orig) + " n∆o encontrado devido a erro prÇvio na mudanáa de c¢digo."
               tt-carga.executado = FALSE.
        NEXT carga.

    END.
    /* Verificar se s∆o CFAs diferentes */
    FIND FIRST ext-item-cfa
        WHERE ext-item-cfa.ep-codigo   = tt-carga.ep-codigo
          AND ext-item-cfa.it-codigo   = tt-carga.it-codigo-orig NO-LOCK NO-ERROR.
    IF NOT AVAIL ext-item-cfa THEN DO:
        ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                    "Item Orig " + QUOTER(tt-carga.it-codigo-orig) + " n∆o tem CFA.".
    END.

    FOR FIRST bitem-para FIELDS(it-codigo cod-obsoleto cod-estabel)
        WHERE bitem-para.it-codigo = tt-carga.it-codigo-para NO-LOCK: END.
    IF NOT AVAIL bitem-para THEN DO:
        FIND FIRST bext-item-cfa
            WHERE bext-item-cfa.ep-codigo   = tt-carga.ep-codigo
              AND bext-item-cfa.it-codigo   = tt-carga.it-codigo-para NO-LOCK NO-ERROR.
        IF NOT AVAIL bext-item-cfa THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Item Padronizado " + QUOTER(tt-carga.it-codigo-para) + " n∆o tem CFA.".
        END.
    END.


    IF AVAIL ext-item-cfa  AND 
       AVAIL bext-item-cfa AND
       ext-item-cfa.classe <> bext-item-cfa.classe THEN DO:
        ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                    "CFA Origem " + QUOTER(ext-item-cfa.classe) + " diferente de CFA Padronizado " +
                                    QUOTER(bext-item-cfa.classe) + ".".
    END.

    ASSIGN l-temordem = NO.
    ordem-compra:
    FOR EACH ordem-compra FIELDS(numero-ordem situacao)
        WHERE ordem-compra.it-codigo = tt-carga.it-codigo-orig NO-LOCK,
        EACH prazo-compra FIELDS(quant-saldo)
        OF ordem-compra NO-LOCK:
        IF ordem-compra.situacao <> 2 /* Confirmada */ THEN
            NEXT ordem-compra.

        IF prazo-compra.quant-saldo <= 0 THEN
            NEXT ordem-compra.

        ASSIGN l-temordem = TRUE.
        LEAVE ordem-compra.

    END. /* ordem-compra */

    /* Verificar Saldo de terceiros */
    ASSIGN l-saldoterc = NO.
    saldo-terc:
    FOR EACH saldo-terc FIELDS (it-codigo quantidade)
        WHERE saldo-terc.it-codigo = tt-carga.it-codigo-orig NO-LOCK:

        IF saldo-terc.quantidade > 0 THEN DO:
            ASSIGN l-saldoterc = YES.
            LEAVE saldo-terc.
        END.
    END. /* EACH saldo-terc */    
    /* Obsoletar item para novas ordens */
    IF l-temordem OR l-saldoterc THEN DO:

        EMPTY TEMP-TABLE tt-item.
        EMPTY TEMP-TABLE tt-erros-geral.    
    
        CREATE tt-item.            
        BUFFER-COPY bitem-orig TO tt-item
            ASSIGN tt-item.ind-tipo-movto = 2 /*1 inclusao / 2 alteracao*/ 
                   tt-item.cod-obsoleto   = 3. /* Obsoleto Todas as Ordens */
   
        RUN cdp/cdapi344.p ( INPUT        TABLE tt-versao-integr,          
                             OUTPUT       TABLE tt-erros-geral,           
                             INPUT-OUTPUT TABLE tt-item).

        FIND FIRST  tt-erros-geral NO-LOCK  NO-ERROR.
        IF AVAIL tt-erros-geral THEN DO:
            FOR EACH tt-erros-geral:
                ASSIGN tt-carga.log-carga = tt-carga.log-carga  + "| " + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro
                       tt-carga.executado = TRUE.
            END.
        END.
        ELSE DO:

            IF l-temordem THEN
                ASSIGN tt-carga.log-carga  = tt-carga.log-carga  + "| " + "Item possui Ordens e/ou Pedidos de compras em aberto. Alterada situaá∆o para 'Obsoleto para Ordens'."
                       tt-carga.finalizado = TRUE
                       tt-carga.executado  = TRUE.
            IF l-saldoterc THEN
                ASSIGN tt-carga.log-carga  = tt-carga.log-carga  + "| " + "Item possui Saldo de Terceiros em aberto. Alterada situaá∆o para 'Obsoleto para Ordens'."
                       tt-carga.finalizado = TRUE
                       tt-carga.executado  = TRUE.
        END.

    END. /* l-temordem */
    ELSE DO:
        /* Verificar se tem saldo */

        saldo-estoq:
        FOR EACH saldo-estoq FIELDS(qtidade-atu)
            WHERE saldo-estoq.it-codigo = tt-carga.it-codigo-orig NO-LOCK:
            ASSIGN l-saldo = saldo-estoq.qtidade-atu > 0.
            IF l-saldo THEN
                LEAVE saldo-estoq.
        END.
        IF l-saldo THEN DO:
            RUN pi-ordprod (BUFFER tt-carga) NO-ERROR.
            IF LOOKUP(RETURN-VALUE,",OK") = 0 THEN DO:
                NEXT carga.
            END.
                
        END. /* de-saldo > 0 */

        /* Atualizar consumo */
        FOR EACH consumo-estab
            WHERE consumo-estab.it-codigo  = tt-carga.it-codigo-orig
              AND consumo-estab.periodo   >= c-periodo NO-LOCK:

            FIND FIRST bconsumo-estab
                WHERE bconsumo-estab.it-codigo        = tt-carga.it-codigo-para 
                  AND bconsumo-estab.periodo          = consumo-estab.periodo
                  AND bconsumo-estab.cod-estabel-prin = consumo-estab.cod-estabel-prin
                  AND bconsumo-estab.ct-codigo        = consumo-estab.ct-codigo
                  AND bconsumo-estab.sc-codigo        = consumo-estab.sc-codigo
                NO-ERROR.
            IF NOT AVAIL bconsumo-estab THEN DO:
                CREATE bconsumo-estab.
                BUFFER-COPY consumo-estab EXCEPT it-codigo TO bconsumo-estab
                    ASSIGN bconsumo-estab.it-codigo = tt-carga.it-codigo-para.
            END.
            ELSE DO:
                ASSIGN bconsumo-estab.qt-entrada = bconsumo-estab.qt-entrada + (consumo-estab.qt-entrada * tt-carga.fator-conv)
                       bconsumo-estab.qt-saida   = bconsumo-estab.qt-saida   + (consumo-estab.qt-saida   * tt-carga.fator-conv).
            END.
        END.

        /* Obsoletar item */
        EMPTY TEMP-TABLE tt-item.
        EMPTY TEMP-TABLE tt-erros-geral.    
    
        CREATE tt-item.            
        BUFFER-COPY bitem-orig TO tt-item
            ASSIGN tt-item.ind-tipo-movto = 2 /*1 inclusao / 2 alteracao*/ 
                   tt-item.cod-obsoleto   = 4. /* TOTAL OBSOLETO */
   
        RUN cdp/cdapi344.p ( INPUT        TABLE tt-versao-integr,          
                             OUTPUT       TABLE tt-erros-geral,           
                             INPUT-OUTPUT TABLE tt-item).

        FIND FIRST  tt-erros-geral NO-LOCK  NO-ERROR.
        IF AVAIL tt-erros-geral THEN DO:
            FOR EACH tt-erros-geral:
                ASSIGN tt-carga.log-carga = tt-carga.log-carga  + "| " + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro
                       tt-carga.executado = TRUE.
            END.
        END.
        ELSE
            ASSIGN tt-carga.finalizado = TRUE
                   tt-carga.executado  = TRUE.

    END. /* NOT l-temordem AND NOT l-saldoterc */

END. /* carga */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-mudarcodigo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mudarcodigo Procedure 
PROCEDURE pi-mudarcodigo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DISABLE TRIGGERS FOR LOAD OF ITEM.
DISABLE TRIGGERS FOR LOAD OF item-uni-estab.
DISABLE TRIGGERS FOR LOAD OF nfe-emit-cfop-nat.
DISABLE TRIGGERS FOR LOAD OF mab-evento-mat.
DISABLE TRIGGERS FOR LOAD OF ext-item-servico.
DISABLE TRIGGERS FOR LOAD OF es-it-depto.
DISABLE TRIGGERS FOR LOAD OF ext-item-arq.
DISABLE TRIGGERS FOR LOAD OF ext-audit-item-fam.
DISABLE TRIGGERS FOR LOAD OF ext-item-estab.
DISABLE TRIGGERS FOR LOAD OF item-fornec-ext.
DISABLE TRIGGERS FOR LOAD OF item-fornec.
DISABLE TRIGGERS FOR LOAD OF ext-item-uni-estab01.
DISABLE TRIGGERS FOR LOAD OF ext-item-uni-estab.
DISABLE TRIGGERS FOR LOAD OF esp-necessidade-oc.
DISABLE TRIGGERS FOR LOAD OF log-ext-item-uni-estab01.
DISABLE TRIGGERS FOR LOAD OF ext-item-cfa.
DISABLE TRIGGERS FOR LOAD OF ext-item-contrat.
DISABLE TRIGGERS FOR LOAD OF ext-ordem-compra.
DISABLE TRIGGERS FOR LOAD OF es-it-uf-estorno-cred.
DISABLE TRIGGERS FOR LOAD OF es-movto-ext-item-cfa.
DISABLE TRIGGERS FOR LOAD OF es-item-doc-est-natoper.
DISABLE TRIGGERS FOR LOAD OF nfe-item-fornec.
DISABLE TRIGGERS FOR LOAD OF ITEM.

DISABLE TRIGGERS FOR DUMP OF ITEM.
DISABLE TRIGGERS FOR DUMP OF item-uni-estab.
DISABLE TRIGGERS FOR DUMP OF item-fornec.
DISABLE TRIGGERS FOR DUMP OF nfe-emit-cfop-nat.
DISABLE TRIGGERS FOR DUMP OF mab-evento-mat.
DISABLE TRIGGERS FOR DUMP OF ext-item-servico.
DISABLE TRIGGERS FOR DUMP OF es-it-depto.
DISABLE TRIGGERS FOR DUMP OF ext-item-arq.
DISABLE TRIGGERS FOR DUMP OF ext-audit-item-fam.
DISABLE TRIGGERS FOR DUMP OF ext-item-estab.
DISABLE TRIGGERS FOR DUMP OF item-fornec-ext.
DISABLE TRIGGERS FOR DUMP OF ext-item-uni-estab01.
DISABLE TRIGGERS FOR DUMP OF ext-item-uni-estab.
DISABLE TRIGGERS FOR DUMP OF esp-necessidade-oc.
DISABLE TRIGGERS FOR DUMP OF log-ext-item-uni-estab01.
DISABLE TRIGGERS FOR DUMP OF ext-item-cfa.
DISABLE TRIGGERS FOR DUMP OF ext-item-contrat.
DISABLE TRIGGERS FOR DUMP OF ext-ordem-compra.
DISABLE TRIGGERS FOR DUMP OF es-it-uf-estorno-cred.
DISABLE TRIGGERS FOR DUMP OF es-movto-ext-item-cfa.
DISABLE TRIGGERS FOR DUMP OF es-item-doc-est-natoper.
DISABLE TRIGGERS FOR DUMP OF nfe-item-fornec.
DISABLE TRIGGERS FOR DUMP OF ITEM.


DEFINE BUFFER b-item-ativo             FOR ITEM.
DEFINE BUFFER b-item-uni-estab         FOR item-uni-estab.
DEFINE BUFFER b-ext-item-cfa           FOR ext-item-cfa.
DEFINE BUFFER b-item-localiz           FOR ITEM.
DEFINE BUFFER b-localiz-item-uni-estab FOR item-uni-estab.

DEFINE VARIABLE h-deleta-item AS HANDLE      NO-UNDO.

CREATE tt-versao-integr. 
ASSIGN tt-versao-integr.cod-versao-integracao = 001     
       tt-versao-integr.ind-origem-msg        = 01.  

MudarCodigo:
FOR EACH tt-carga
    WHERE tt-carga.log-carga    = ""
      AND tt-carga.it-sit-para <= 2 /* N∆o existe */ :

    run pi-acompanhar in h-acomp (input "DE-PARA:":U + tt-carga.it-codigo-orig + "-" + tt-carga.it-codigo-para).

    IF tt-carga.it-sit-para = 2 /* TOTAL OBSOLETO */ THEN DO:
    
        EMPTY TEMP-TABLE tt-altera.
        EMPTY TEMP-TABLE tt-elimina.

        CREATE tt-elimina.
        ASSIGN tt-elimina.it-codigo  = tt-carga.it-codigo-para
               tt-elimina.un         = tt-carga.un-para
               tt-elimina.descricao  = tt-carga.descricao.

        RUN pi-cd0205rp.
      
        /* Verificar se item para existe e esta ativo no ERP */
        FOR FIRST bitem-para FIELDS(it-codigo cod-obsoleto cod-estabel)
            WHERE bitem-para.it-codigo = tt-carga.it-codigo-para NO-LOCK: END.
        IF AVAIL bitem-para THEN DO:

            RUN esp/deletaitemrp-b.w PERSISTENT SET h-deleta-item(INPUT bitem-para.it-codigo).

            /*FIND FIRST b-ext-item-cfa
                 WHERE b-ext-item-cfa.ep-codigo   = tt-carga.ep-codigo
                   AND b-ext-item-cfa.it-codigo   = tt-carga.it-codigo-orig NO-LOCK NO-ERROR.


            FOR EACH b-item-ativo 
               WHERE b-item-ativo.it-codigo = tt-carga.it-codigo-para:
                ASSIGN b-item-ativo.cod-obsoleto = 1
                       b-item-ativo.cod-localiz  = IF tt-carga.cod-localiz <> "" THEN tt-carga.cod-localiz ELSE b-item-ativo.cod-localiz.
            END.

          
            FOR EACH estabelec NO-LOCK
               WHERE estabelec.ep-codigo = tt-carga.ep-codigo:

                FOR EACH b-item-uni-estab
                   WHERE b-item-uni-estab.it-codigo = tt-carga.it-codigo-para
                     AND b-item-uni-estab.cod-estabel = estabelec.cod-estab:
                    ASSIGN b-item-uni-estab.cod-obsoleto = 1
                           b-item-uni-estab.cod-localiz = IF tt-carga.cod-localiz <> "" THEN tt-carga.cod-localiz ELSE b-item-uni-estab.cod-localiz.
                END.
            END.
                
            FIND FIRST ext-item-cfa 
                 WHERE ext-item-cfa.it-codigo = tt-carga.it-codigo-para
                   AND ext-item-cfa.ep-codigo = tt-carga.ep-codigo NO-ERROR.
            IF NOT AVAIL(ext-item-cfa) THEN DO:

                CREATE ext-item-cfa.
                ASSIGN ext-item-cfa.it-codigo   = tt-carga.it-codigo-para
                       ext-item-cfa.ep-codigo   = tt-carga.ep-codigo
                       ext-item-cfa.classe      = b-ext-item-cfa.classe
                       ext-item-cfa.idKlassmatt = int(tt-carga.id-klassmatt)
                       ext-item-cfa.chancela    = FALSE .
            END.

            ASSIGN tt-carga.it-sit-para = 3. /* Ativo */ */
           /* NEXT MudarCodigo.*/
        END.

        /* Eliminar, porque vai ser recriado no parÉmetro */
        FOR FIRST es-it-depto
            WHERE es-it-depto.it-codigo = tt-carga.it-codigo-para:
            DELETE es-it-depto.
        END.
    END.

    EMPTY TEMP-TABLE tt-altera.
    EMPTY TEMP-TABLE tt-elimina.

    CREATE tt-altera.
    ASSIGN tt-altera.it-codigo     = tt-carga.it-codigo-orig
           tt-altera.descricao     = tt-carga.descricao
           tt-altera.un            = tt-carga.un-de
           tt-altera.new-it-codigo = tt-carga.it-codigo-para
           tt-altera.new-un        = tt-carga.un-para
           tt-altera.fator-conv    = tt-carga.fator-conv
           tt-altera.ge-codigo     = tt-carga.ge-codigo
           tt-altera.old-ge-codigo = tt-carga.old-ge-codigo.

    RUN pi-cd0205rp.

    PUT STREAM st-log "i-num-ped-exec-rpw-> " i-num-ped-exec-rpw SKIP
                      "SESSION:TEMP-DIRECTORY" SESSION:TEMP-DIRECTORY FORMAT "x(200)" SKIP
        "tt-paramCD0205.arquivo" tt-paramCD0205.arquivo FORMAT "x(200)" SKIP(1).
    
    IF i-num-ped-exec-rpw <> 0 THEN DO:
       
       DO WHILE NOT CAN-FIND(FIRST bitem-para
                       WHERE bitem-para.it-codigo = tt-carga.it-codigo-para 
                       NO-LOCK):
           PUT STREAM st-log STRING(TIME,'hh:mm:ss') " - de: "  tt-carga.it-codigo-orig " para: " tt-carga.it-codigo-para SKIP.
       END.
       

    END.
    ELSE DO:
       blwait:
       DO i-cont = 1 TO (10000):
           IF CAN-FIND(FIRST bitem-para
                       WHERE bitem-para.it-codigo = tt-carga.it-codigo-para 
                       NO-LOCK) THEN 
               LEAVE blwait.
       
           PAUSE 1 BEFORE-HIDE NO-MESSAGE.
       END.
    END.


    /* Verificar se item para existe e esta ativo no ERP */
    FOR FIRST bitem-para
        WHERE bitem-para.it-codigo = tt-carga.it-codigo-para NO-LOCK: END.
    IF NOT AVAIL bitem-para THEN DO:
        ASSIGN tt-carga.log-carga = "N∆o foi poss°vel mudar o c¢digo (Erro Interno)."
               tt-carga.executado = TRUE.
    END.
    ELSE DO:
        /* Atualizar dados enviados */
        EMPTY TEMP-TABLE tt-item.
        EMPTY TEMP-TABLE tt-erros-geral.    
    
        CREATE tt-item.            
        BUFFER-COPY bitem-para TO tt-item.

        ASSIGN tt-item.ind-tipo-movto = 2. /*1 inclusao / 2 alteracao*/ 
        ASSIGN tt-item.desc-item    = tt-carga.descricao
               tt-item.narrativa    = replace(tt-carga.narrativa,"CHR(10)",CHR(10))
               tt-item.fm-codigo    = tt-carga.fm-codigo   
               tt-item.fm-cod-com   = tt-carga.fm-cod-com
               tt-item.codigo-refer = tt-carga.cod-refer
               tt-item.class-fiscal = tt-carga.class-fiscal
               tt-item.dec-1        = DEC(tt-carga.ex-ncm).


        /* Comentado em 02-01-16 - ELG */

/*         RUN cdp/cdapi344.p ( INPUT        TABLE tt-versao-integr, */
/*                              OUTPUT       TABLE tt-erros-geral,   */
/*                              INPUT-OUTPUT TABLE tt-item).         */
    
        
	    /** 11/01/2016 **/
    	FIND FIRST bitem-para WHERE bitem-para.it-codigo = tt-carga.it-codigo-para EXCLUSIVE-LOCK NO-ERROR.
    
    	IF AVAIL  bitem-para AND bitem-para.fm-cod-com NE tt-carga.fm-cod-com THEN
               ASSIGN bitem-para.fm-cod-com = tt-carga.fm-cod-com.
    
        /* 22/01/2016 */
        IF AVAIL bitem-para THEN 
            ASSIGN bitem-para.fm-codigo  = tt-carga.fm-codigo.  /* C¢d. Fam°lia do item PARA */
    
            
    	/** 11/01/2016 **/
    
            FIND FIRST  tt-erros-geral NO-LOCK  NO-ERROR.
    
            IF AVAIL tt-erros-geral THEN DO:
                FOR EACH tt-erros-geral:
                    ASSIGN tt-carga.log-carga = tt-carga.log-carga  + "| " + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro.
                END.
            END.

        run pi-acompanhar in h-acomp (input "ESP DE-PARA:":U + tt-carga.it-codigo-orig + "-" + tt-carga.it-codigo-para).

        /* Tratar tabelas espec°ficas */
        
        FOR EACH mab-evento-mat FIELDS( it-codigo)
            WHERE mab-evento-mat.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN mab-evento-mat.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH ext-item-servico FIELDS( it-codigo)
            WHERE ext-item-servico.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-item-servico.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR FIRST es-it-depto FIELDS( it-codigo)
            WHERE es-it-depto.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN es-it-depto.it-codigo = tt-carga.it-codigo-para
                   es-it-depto.cod-dept  = INT(tt-carga.cod-depto).
        END.
        
        FOR EACH ext-item-arq FIELDS( it-codigo)
            WHERE ext-item-arq.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-item-arq.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH ext-audit-item-fam FIELDS( it-codigo)
            WHERE ext-audit-item-fam.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-audit-item-fam.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH ext-item-estab FIELDS( it-codigo)
            WHERE ext-item-estab.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-item-estab.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH item-fornec-ext FIELDS( it-codigo)
            WHERE item-fornec-ext.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN item-fornec-ext.it-codigo = tt-carga.it-codigo-para.
        END.

        FOR EACH item-uni-estab FIELDS( it-codigo)
            WHERE item-uni-estab.it-codigo = tt-carga.it-codigo-orig:
            DELETE item-uni-estab.
        END.
        
        FOR EACH ext-item-uni-estab01 FIELDS( it-codigo)
            WHERE ext-item-uni-estab01.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-item-uni-estab01.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH ext-item-uni-estab FIELDS( it-codigo)
            WHERE ext-item-uni-estab.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-item-uni-estab.it-codigo = tt-carga.it-codigo-para.
        END.

        FOR EACH esp-necessidade-oc FIELDS( it-codigo)
            WHERE esp-necessidade-oc.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN esp-necessidade-oc.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH log-ext-item-uni-estab01 FIELDS( it-codigo)
            WHERE log-ext-item-uni-estab01.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN log-ext-item-uni-estab01.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH ext-item-cfa  FIELDS(ep-codigo it-codigo)
            WHERE ext-item-cfa.ep-codigo = tt-carga.ep-codigo
              AND ext-item-cfa.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-item-cfa.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH ext-item-contrat FIELDS( it-codigo)
            WHERE ext-item-contrat.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-item-contrat.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH ext-ordem-compra  FIELDS( it-codigo)
            WHERE ext-ordem-compra.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN ext-ordem-compra.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH es-it-uf-estorno-cred  FIELDS( it-codigo)
            WHERE es-it-uf-estorno-cred.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN es-it-uf-estorno-cred.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH es-movto-ext-item-cfa  FIELDS(ep-codigo it-codigo)
            WHERE es-movto-ext-item-cfa.ep-codigo = tt-carga.ep-codigo
              AND es-movto-ext-item-cfa.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN es-movto-ext-item-cfa.it-codigo = tt-carga.it-codigo-para.
        END.
        
        FOR EACH es-item-doc-est-natoper FIELDS(ep-codigo it-codigo)
            WHERE es-item-doc-est-natoper.ep-codigo = tt-carga.ep-codigo
              AND es-item-doc-est-natoper.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN es-item-doc-est-natoper.it-codigo = tt-carga.it-codigo-para.
        END.

        FOR EACH estabelec NO-LOCK
           WHERE estabelec.ep-codigo = tt-carga.ep-codigo:

            FOR EACH nfe-emit-cfop-nat FIELDS(cod-estabel it-codigo)
               WHERE nfe-emit-cfop-nat.cod-estabel = estabelec.cod-estabel
                 AND nfe-emit-cfop-nat.it-codigo   = tt-carga.it-codigo-orig:
                ASSIGN nfe-emit-cfop-nat.it-codigo  = tt-carga.it-codigo-para.
            END.

        END.
        
        FOR EACH nfe-item-fornec FIELDS(it-codigo)
            WHERE nfe-item-fornec.it-codigo = tt-carga.it-codigo-orig:
            ASSIGN nfe-item-fornec.it-codigo = tt-carga.it-codigo-para.
        END.


        /* Comentado em 22/01/2016 */
        
        /* Alterar localizaá∆o data 17/12/2015*/  /*** 11/01/2015 */
        /* ELG - 01-02-16 */
        FIND FIRST b-item-localiz                                                              /* FIELDS(cod-localiz) */
             WHERE b-item-localiz.it-codigo = tt-carga.it-codigo-para EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL b-item-localiz THEN
            ASSIGN b-item-localiz.cod-localiz       = tt-carga.cod-localiz-orig /*IF tt-carga.cod-localiz <> "" THEN tt-carga.cod-localiz ELSE b-item-localiz.cod-localiz.*/
                   b-item-localiz.cod-refer         = tt-carga.cod-refer
                   b-item-localiz.desc-item         = tt-item.desc-item   
                   b-item-localiz.narrativa         = tt-item.narrativa   
                   b-item-localiz.fm-codigo         = tt-item.fm-codigo   
                   b-item-localiz.fm-cod-com        = tt-item.fm-cod-com  
                   b-item-localiz.codigo-refer      = tt-item.codigo-refer
                   b-item-localiz.class-fiscal      = tt-item.class-fiscal
                   b-item-localiz.dec-1             = tt-item.dec-1. 

     /* END. */

        /** 11/01/2015 */

/*         FOR EACH estabelec NO-LOCK                                                                                                                               */
/*            WHERE estabelec.ep-codigo = tt-carga.ep-codigo:                                                                                                       */
/*                                                                                                                                                                  */
/*             FOR EACH b-localiz-item-uni-estab FIELDS(cod-localiz)                                                                                                */
/*                WHERE b-localiz-item-uni-estab.it-codigo = tt-carga.it-codigo-para                                                                                */
/*                  AND b-localiz-item-uni-estab.cod-estabel = estabelec.cod-estab:                                                                                 */
/*                 ASSIGN b-localiz-item-uni-estab.cod-localiz = IF tt-carga.cod-localiz <> "" THEN tt-carga.cod-localiz ELSE b-localiz-item-uni-estab.cod-localiz. */
/*             END.                                                                                                                                                 */
/*         END.           
                                                                                                                                          */


      

        /* ELG - 01-02-16 */
        FOR EACH estabelec NO-LOCK WHERE
                 estabelec.ep-codigo = tt-carga.ep-codigo:
            FOR EACH b-localiz-item-uni-estab EXCLUSIVE-LOCK
               WHERE b-localiz-item-uni-estab.it-codigo   = tt-carga.it-codigo-para
                 AND b-localiz-item-uni-estab.cod-estabel = estabelec.cod-estab:

                   /* 22/01/2016 */
                   IF AVAIL bitem-para THEN 
                      ASSIGN b-localiz-item-uni-estab.cod-obsoleto = bitem-para.cod-obsoleto.

                FIND FIRST tt-uni-estab-orig NO-LOCK WHERE
                           tt-uni-estab-orig.cod-estabel    =  estabelec.cod-estabel AND
                           tt-uni-estab-orig.it-codigo      =  tt-carga.it-codigo-orig   NO-ERROR.
                IF AVAIL tt-uni-estab-orig THEN DO:
                    ASSIGN b-localiz-item-uni-estab.cod-localiz =   tt-uni-estab-orig.cod-localiz.

                      ASSIGN b-localiz-item-uni-estab.cod-obsoleto = tt-uni-estab-orig.cod-obsoleto.


                END.
            END.
        END.


        /* Alterar localizaá∆o data 17/12/2015*/

        ASSIGN tt-carga.finalizado = TRUE
               tt-carga.executado  = TRUE.

        /* Verificar se existe fila de replicaá∆o pendente e eliminar */
        FIND FIRST es-fila-rep-item
             WHERE es-fila-rep-item.ep-codigo = tt-carga.ep-codigo       
               AND es-fila-rep-item.it-codigo = tt-carga.it-codigo-para NO-ERROR.
        IF AVAIL es-fila-rep-item THEN
            DELETE es-fila-rep-item.

    END. 


    IF VALID-HANDLE(h-deleta-item) THEN
         DELETE OBJECT h-deleta-item NO-ERROR.

    IF ((TIME - ia-ini-exec) / 60) >= tt-param.tempo-exec THEN
        LEAVE MudarCodigo.
END. /* MudarCodigo */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-obsoleto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-obsoleto Procedure 
PROCEDURE pi-obsoleto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-carga
    WHERE tt-carga.log-carga   = ""
      AND tt-carga.it-sit-para = 2 /* TOTAL OBSOLETO */:

    EMPTY TEMP-TABLE tt-altera.
    EMPTY TEMP-TABLE tt-elimina.

    CREATE tt-elimina.
    ASSIGN tt-elimina.it-codigo  = tt-carga.it-codigo-para
           tt-elimina.un         = tt-carga.un-para
           tt-elimina.descricao  = tt-carga.descricao.

    RUN pi-cd0205rp.

    /* Verificar se item para existe e esta ativo no ERP */
    FOR FIRST bitem-para FIELDS(it-codigo cod-obsoleto cod-estabel)
        WHERE bitem-para.it-codigo = tt-carga.it-codigo-para NO-LOCK: END.
    IF NOT AVAIL bitem-para THEN
        ASSIGN tt-carga.it-sit-para = 1. /* N∆o existe */
    ELSE DO:
        ASSIGN tt-carga.it-sit-para = 3. /* Ativo */
    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-ordprod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ordprod Procedure 
PROCEDURE pi-ordprod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER btt-carga FOR TEMP-TABLE tt-carga. /* PRESELECT.*/


DEFINE VARIABLE ia-nr-linha AS INTEGER     NO-UNDO.
DEFINE VARIABLE ia-seq      AS INTEGER     NO-UNDO.
DEFINE VARIABLE h-cpapi001  AS HANDLE      NO-UNDO.
DEFINE VARIABLE da-ini      AS DATE        NO-UNDO.
DEFINE VARIABLE da-fim      AS DATE        NO-UNDO.
DEFINE BUFFER bord-prod  FOR ord-prod .
     
FOR FIRST bitem-para FIELDS(it-codigo cod-obsoleto cod-estabel un)
    WHERE bitem-para.it-codigo = btt-carga.it-codigo-para NO-LOCK: END.

FOR FIRST bitem-orig
    WHERE bitem-orig.it-codigo = btt-carga.it-codigo-orig NO-LOCK: END.

ASSIGN da-ini = TODAY
       da-fim = TODAY.
find first param-cp     no-lock no-error.
IF param-cp.cd-tipo = 3 /* Mensal */ THEN DO:
    for first periodo fields (cd-tipo   dt-inicio   dt-termino) use-index ch-termino
        where periodo.cd-tipo     = param-cp.cd-tipo
          and periodo.dt-termino >= TODAY no-lock: 
        ASSIGN da-ini = periodo.dt-inicio
               da-fim = periodo.dt-termino.
    end.
END.

Gravar: DO TRANSACTION ON ERROR UNDO Gravar, RETURN "NOK":
    saldo-estoq:
    FOR EACH saldo-estoq
        WHERE saldo-estoq.it-codigo = btt-carga.it-codigo-orig NO-LOCK:
        IF saldo-estoq.qtidade-atu = 0 THEN
            NEXT saldo-estoq.

        FIND FIRST es-estab-carga
            WHERE es-estab-carga.ep-codigo   = i-ep-codigo-usuario
              AND es-estab-carga.cod-estabel = saldo-estoq.cod-estabel NO-LOCK NO-ERROR.
        IF AVAIL es-estab-carga THEN
            ASSIGN ia-nr-linha = es-estab-carga.nr-linha.
        ELSE DO:
            FIND FIRST lin-prod
                WHERE lin-prod.cod-estabel = saldo-estoq.cod-estabel
                  AND lin-prod.sum-requis  = 2 /* Ordem Serviáo */ NO-LOCK NO-ERROR.
            IF AVAIL lin-prod THEN
                ASSIGN ia-nr-linha = lin-prod.nr-linha.

        END.
    
        FOR EACH tt-ord-prod. DELETE tt-ord-prod. END.
        FOR EACH tt-reapro.   DELETE tt-reapro.   END.    
        FOR EACH tt-erro.     DELETE tt-erro.     END.    

        CREATE tt-ord-prod.
        ASSIGN tt-ord-prod.nr-ord-produ          = 0
               tt-ord-prod.it-codigo             = bitem-para.it-codigo
               tt-ord-prod.qt-ordem              = saldo-estoq.qtidade-atu * btt-carga.fator-conv
               tt-ord-prod.qt-produzida          = 0
               tt-ord-prod.qt-refugada           = 0
               tt-ord-prod.un                    = bitem-para.un
               tt-ord-prod.dt-inicio             = da-ini
               tt-ord-prod.dt-termino            = da-fim
               tt-ord-prod.cd-planejaDo          = ?
               tt-ord-prod.estado                = ?
               tt-ord-prod.emite-requis          = Yes
               tt-ord-prod.emite-ordem           = Yes
               tt-ord-prod.cod-depos             = saldo-estoq.cod-depos
               tt-ord-prod.nr-req-sum            = 0
               tt-ord-prod.dt-emissao            = TODAY
               tt-ord-prod.ct-codigo             = ""
               tt-ord-prod.sc-codigo             = ""
               tt-ord-prod.qt-reportada          = 0
               tt-ord-prod.qt-requisita          = 0
               tt-ord-prod.lote-serie            = saldo-estoq.lote
               tt-ord-prod.narrativa             = "Mudanáa de c¢digo carga Integra/Klassmatt"
               tt-ord-prod.nr-linha              = ia-nr-linha 
               tt-ord-prod.tipo                  = 4 /* Retrabalho */
               tt-ord-prod.usuario-alt           = ?
               tt-ord-prod.data-alt              = TODAY
               tt-ord-prod.cod-estabel           = saldo-estoq.cod-estabel
               tt-ord-prod.dt-orig               = ?
               tt-ord-prod.valorizada            = No
               tt-ord-prod.calc-cs-mat           = 2
               tt-ord-prod.reporte-mob           = 2
               tt-ord-prod.req-emitida           = No
               tt-ord-prod.prioridade            = 99
               tt-ord-prod.val-per               = No
               tt-ord-prod.cod-refer             = saldo-estoq.cod-depos
               tt-ord-prod.cod-gr-cli            = 0
               tt-ord-prod.nr-ult-seq            = 0
               tt-ord-prod.rep-prod              = 1
               tt-ord-prod.qt-apr-cond           = 0
               tt-ord-prod.qt-perda              = 0
               tt-ord-prod.custeio-prop-mob      = 1
               tt-ord-prod.qt-inicial            = 0
               tt-ord-prod.custeio-prop-mat      = 1
               tt-ord-prod.prod-repet            = No
               tt-ord-prod.nr-ord-aber           = 0
               tt-ord-prod.nr-sequencia          = 0
               tt-ord-prod.cons-mrp              = Yes
               tt-ord-prod.cons-pmp              = Yes
               tt-ord-prod.ct-desp               = ""
               tt-ord-prod.sc-desp               = ""
               tt-ord-prod.origem                = ""
               tt-ord-prod.sit-aloc              = 1
               tt-ord-prod.nr-ficha              = 0
               tt-ord-prod.enc-mensal            = No
               tt-ord-prod.it-inspec             = ""
               tt-ord-prod.ct-imob               = ""
               tt-ord-prod.sc-imob               = ""
               tt-ord-prod.prototipo             = No
               tt-ord-prod.num-ord-inv           = 0
               tt-ord-prod.dest-manut            = 1
               tt-ord-prod.nr-entrega            = 0
               tt-ord-prod.nr-ord-refer          = 0
               tt-ord-prod.conta-ordem           = ?
               tt-ord-prod.conta-despesa         = ""
               tt-ord-prod.conta-imob            = ""
               tt-ord-prod.custeio-prop-ggf      = 1
               tt-ord-prod.calc-cs-mob           = 2
               tt-ord-prod.calc-cs-ggf           = 2
               tt-ord-prod.reporte-ggf           = 1
               tt-ord-prod.nr-estrut             = 1
               tt-ord-prod.item-cotacao          = ""
               tt-ord-prod.sequencia             = 0
               tt-ord-prod.es-codigo             = ""
               tt-ord-prod.nr-estrut-filha       = 0
               tt-ord-prod.dt-disponibilidade    = ?
               tt-ord-prod.ind-tipo-movto        = 1
               tt-ord-prod.faixa-numeracao       = 1 /* (1-manual, 2-autom†tica) */
               tt-ord-prod.verIfica-compras      = No
               tt-ord-prod.aloca-reserva         = No
               tt-ord-prod.aloca-lote            = No
               tt-ord-prod.rw-ord-prod           = ?
               tt-ord-prod.gera-relacionamentos  = NO
               tt-ord-prod.prog-Seg              = ""
               tt-ord-prod.seg-usuario           = ""
              /* tt-ord-prod.ep-codigo-usuario     = ""*/
               tt-ord-prod.cod-versao-integracao = 3
               tt-ord-prod.considera-dias-desl   = No.
            
        FOR EACH tt-erro: DELETE tt-erro. END.
        RUN cpp/cpapi301.p (INPUT-OUTPUT TABLE tt-ord-prod,
                            INPUT-OUTPUT TABLE tt-reapro,
                            INPUT-OUTPUT TABLE tt-erro,
                            INPUT YES).
        FIND FIRST tt-ord-prod.

        FIND FIRST tt-erro NO-ERROR.
        IF AVAIL tt-erro OR LOOKUP(RETURN-VALUE,",OK") = 0 THEN DO:
            FOR EACH tt-erro: 
                ASSIGN btt-carga.log-carga = btt-carga.log-carga  + "| " + STRING(tt-erro.cd-erro) + "-" + tt-erro.mensagem
                       btt-carga.executado = TRUE.
            END.
            IF VALID-HANDLE(h-cpapi001) THEN
                RUN pi-finalizar IN h-cpapi001 NO-ERROR.
            UNDO Gravar, RETURN "NOK".
        END.
                                          
        FOR EACH tt-erro: DELETE tt-erro. END.
               
        FIND FIRST tt-ord-prod NO-ERROR.
        
        
        /* Faz o reporte da ordem */
        FIND bord-prod 
            WHERE ROWID(bord-prod) = tt-ord-prod.rw-ord-prod NO-LOCK NO-ERROR.
        
        IF AVAIL bord-prod THEN DO:
            FOR EACH tt-reservas. DELETE tt-reservas. END.
            FOR EACH tt-res-neg.  DELETE tt-res-neg.  END.
            FOR EACH tt-rep-prod. DELETE tt-rep-prod. END.
            RUN cpp/cpapi001.p PERSISTENT SET h-cpapi001 (INPUT-OUTPUT TABLE tt-rep-prod,
                                                          INPUT        TABLE tt-refugo,
                                                          INPUT        TABLE tt-res-neg,
                                                          INPUT        TABLE tt-apont-mob,
                                                          INPUT-OUTPUT TABLE tt-erro,
                                                          INPUT        YES) NO-ERROR.
            CREATE tt-res-neg.
            ASSIGN tt-res-neg.nr-ord-produ = bord-prod.nr-ord-produ
                   tt-res-neg.it-codigo    = saldo-estoq.it-codigo
                   tt-res-neg.quantidade   = saldo-estoq.qtidade-atu
                   tt-res-neg.cod-depos    = saldo-estoq.cod-depos
                   tt-res-neg.cod-localiz  = saldo-estoq.cod-localiz
                   tt-res-neg.lote-serie   = saldo-estoq.lote
                   tt-res-neg.cod-refer    = saldo-estoq.cod-refer
                   tt-res-neg.dt-vali-lote = saldo-estoq.dt-vali-lote.   
            CREATE tt-reservas.
            ASSIGN tt-reservas.nr-ord-produ   = bord-prod.nr-ord-produ
                   tt-reservas.cod-refer      = saldo-estoq.cod-refer
                   tt-reservas.it-codigo      = saldo-estoq.it-codigo
                   tt-reservas.quant-orig     = saldo-estoq.qtidade-atu
                   tt-reservas.quant-aloc     = saldo-estoq.qtidade-atu
                   tt-reservas.quant-atend    = saldo-estoq.qtidade-atu
/*                    tt-reservas.quant-calc     = */
                   tt-reservas.quant-requis   = saldo-estoq.qtidade-atu
                   tt-reservas.cod-depos      = saldo-estoq.cod-depos
                   tt-reservas.cod-localiz    = saldo-estoq.cod-localiz
                   tt-reservas.lote-serie     = saldo-estoq.lote
                   tt-reservas.dt-vali-lote   = saldo-estoq.dt-vali-lote
                   tt-reservas.un             = btt-carga.un-de
                   tt-reservas.estado         = 1 /* Ativo */
                   tt-reservas.tipo-sobra     = 4 /* Normal */
                   tt-reservas.item-pai       = bord-prod.it-codigo
/*                    tt-reservas.op-codigo      = */
/*                    tt-reservas.cod-roteiro    = */
                   tt-reservas.rw-saldo-estoq = ROWID(saldo-estoq)
                   tt-reservas.sequencia      = 10.
            
            ASSIGN ia-seq = 0.
            FOR EACH tt-res-neg:
                
                ASSIGN ia-seq = ia-seq + 10.

                CREATE tt-rep-prod.
                ASSIGN tt-rep-prod.tipo                  = 1
                       tt-rep-prod.nr-ord-produ          = bord-prod.nr-ord-produ
                       tt-rep-prod.it-codigo             = bord-prod.it-codigo
                       tt-rep-prod.data                  = TODAY
                       tt-rep-prod.qt-reporte            = saldo-estoq.qtidade-atu * btt-carga.fator-conv
                       tt-rep-prod.nro-Docto             = STRING(bord-prod.nr-ord-produ)
                       tt-rep-prod.procura-saldos        = TRUE 
                       tt-rep-prod.carrega-reservas      = NO
                       tt-rep-prod.requis-automatica     = TRUE 
                       tt-rep-prod.prog-seg              = "YMCD0201"
                       tt-rep-prod.finaliza-ordem        = TRUE
                       tt-rep-prod.finaliza-oper         = TRUE 
                       tt-rep-prod.reserva               = NO
                       tt-rep-prod.cod-depos             = saldo-estoq.cod-depos
                       tt-rep-prod.cod-localiz           = saldo-estoq.cod-localiz
                       tt-rep-prod.cod-depos-sai         = saldo-estoq.cod-depos
                       tt-rep-prod.cod-local-sai         = saldo-estoq.cod-localiz
                       tt-rep-prod.sequencia             = ia-seq
                       tt-rep-prod.qt-refugo             = 0
                       tt-rep-prod.nro-ord-seq           = ia-seq
                       tt-rep-prod.lote-serie            = saldo-estoq.lote
                       tt-rep-prod.ct-codigo             = bord-prod.conta-ordem
                       tt-rep-prod.un                    = bord-prod.un
                       tt-rep-prod.cod-versao-integracao = 001.
            
            END. /* EACH tt-res-neg */

            FOR EACH tt-erro: DELETE tt-erro. END.            
/*             RUN cpp/cpapi001.p (INPUT-OUTPUT TABLE tt-rep-prod,  */
/*                                 INPUT        TABLE tt-refugo,    */
/*                                 INPUT        TABLE tt-res-neg,   */
/*                                 INPUT        TABLE tt-apont-mob, */
/*                                 INPUT-OUTPUT TABLE tt-erro,      */
/*                                 INPUT        YES).               */
            /*Enviar dados pai do reporte*/
            RUN pi-recebe-tt-rep-prod IN h-cpapi001 (INPUT TABLE tt-rep-prod).

            /* Envia tabela de reservas ajustada */ 
            RUN pi-recebe-tt-reservas IN h-cpapi001 (INPUT TABLE  tt-reservas).
        
            /* Processa os reportes */
            RUN pi-processa-reportes IN h-cpapi001 (INPUT-OUTPUT TABLE tt-rep-prod,
                                                                INPUT        TABLE tt-refugo,
                                                                INPUT        TABLE tt-res-neg,
                                                                INPUT-OUTPUT TABLE tt-erro,
                                                                INPUT YES, /* l-deleta-erros */
                                                    INPUT YES). /* l-gera-reqs */ 

            RUN pi-finalizar IN h-cpapi001.

            FIND FIRST tt-erro NO-ERROR.
            IF AVAIL tt-erro OR LOOKUP(RETURN-VALUE,",OK") = 0 THEN DO:
                FOR EACH tt-erro: 
                    ASSIGN btt-carga.log-carga = btt-carga.log-carga  + "| " + STRING(tt-erro.cd-erro) + "-" + tt-erro.mensagem
                           btt-carga.executado = TRUE.
                END.
                
                UNDO Gravar, RETURN "NOK".
            END.

            /* Terminar Ordem de Produá∆o */
            FIND CURRENT bord-prod EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN bord-prod.estado     = 8
                   bord-prod.valorizada = NO
                   &IF DEFINED (bf_man_sfc_lc) &THEN
                        bord-prod.dt-efetiv-term = TODAY
                        bord-prod.hr-efetiv-term = replace(STRING(TIME,"HH:MM:SS"), ":", "")
                   &endif.
               

        END. /* bord-prod */
        FIND CURRENT bord-prod NO-LOCK NO-ERROR.
        ASSIGN btt-carga.log-carga = btt-carga.log-carga  + "| " + "Ordem de Produá∆o: " + STRING(bord-prod.nr-ord-produ).


    END. /* saldo-estoq */
END. /* Gravar */
              


RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-replicar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-replicar Procedure 
PROCEDURE pi-replicar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-carga
    WHERE finalizado = TRUE:

    tt-emp:
    FOR EACH tt-emp:
        FIND FIRST es-de-para-it-padr
            WHERE es-de-para-it-padr.ep-codigo      = tt-emp.ep-codigo
              AND es-de-para-it-padr.it-codigo-padr = tt-carga.it-codigo-para NO-LOCK NO-ERROR.
        IF AVAIL es-de-para-it-padr THEN
            NEXT tt-emp.

        IF CAN-FIND(FIRST es-fila-rep-item
                    WHERE es-fila-rep-item.ep-codigo = tt-emp.ep-codigo       
                      AND es-fila-rep-item.it-codigo = tt-carga.it-codigo-para NO-LOCK) THEN
            NEXT tt-emp.


        CREATE es-fila-rep-item.
        ASSIGN es-fila-rep-item.ep-codigo        = tt-emp.ep-codigo
               es-fila-rep-item.it-codigo        = tt-carga.it-codigo-para
               es-fila-rep-item.class-fiscal     = tt-carga.class-fiscal
               es-fila-rep-item.cod-depto        = tt-carga.cod-depto
               es-fila-rep-item.cod-estabel      = ""
               es-fila-rep-item.cod-servico      = tt-carga.cod-servico
               es-fila-rep-item.codigo-orig      = tt-carga.cod-orig
               es-fila-rep-item.codigo-refer     = tt-carga.cod-refer
               es-fila-rep-item.dec-1            = DEC(tt-carga.ex-ncm)
               es-fila-rep-item.desc-item        = tt-carga.descricao
               es-fila-rep-item.fm-cod-com       = tt-carga.fm-cod-com
               es-fila-rep-item.fm-codigo        = tt-carga.fm-codigo
               es-fila-rep-item.ge-codigo        = 99
               es-fila-rep-item.narrativa        = replace(tt-carga.narrativa,"CHR(10)",CHR(10))
               es-fila-rep-item.quant-segur      = tt-carga.quant-segur
               es-fila-rep-item.tipo-contr       = tt-carga.controla_estoque
               es-fila-rep-item.un               = tt-carga.un-para
               es-fila-rep-item.char-2           = tt-carga.char-2
               es-fila-rep-item.data-implant     = NOW
               es-fila-rep-item.dt-criacao       = NOW
               es-fila-rep-item.mensagem-erro    = ""
               es-fila-rep-item.nr-tentativas    = 0
               es-fila-rep-item.dt-ult-tentativa = ?.

    END. /* tt-emp */

END.

RUN esp/ymcd0203.p.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-resultado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-resultado Procedure 
PROCEDURE pi-resultado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-carga
    WHERE tt-carga.executado = TRUE:

    run pi-acompanhar in h-acomp (input "Imprimindo:":U + tt-carga.it-codigo-para).

    IF tt-param.todos = 2 AND tt-carga.log-carga = "" THEN
        NEXT.

/*     EXPORT DELIMITER ";"                                                    */
/*            tt-carga.ep-codigo                                               */
/*            tt-carga.id-klassmatt                                            */
/*            tt-carga.it-codigo-orig                                          */
/*            tt-carga.it-codigo-temp                                          */
/*            tt-carga.ge-codigo                                               */
/*            tt-carga.un-de                                                   */
/*            tt-carga.un-para                                                 */
/*            tt-carga.fator-conv                                              */
/*            tt-carga.descricao                                               */
/*            tt-carga.narrativa                                               */
/*            tt-carga.fm-codigo                                               */
/*            tt-carga.cod-refer                                               */
/*            tt-carga.cod-orig                                                */
/*            tt-carga.class-fiscal                                            */
/*            tt-carga.ex-ncm                                                  */
/*            tt-carga.controla_estoque                                        */
/*            tt-carga.quant-segur                                             */
/*            tt-carga.cod-servico                                             */
/*            tt-carga.fm-cod-com                                              */
/*            tt-carga.tipo-item                                               */
/*            tt-carga.cod-depto                                               */
/*            tt-carga.it-codigo-para                                          */
/*            IF tt-carga.log-carga = THEN "Carregado" ELSE tt-carga.log-carga */
/*            .                                                                */

/*     ASSIGN tt-carga.log-carga = TRIM(tt-carga.log-carga,"| ").                     */
/*     DISP   tt-carga.ep-codigo                                                      */
/*            tt-carga.id-klassmatt                                                   */
/*            tt-carga.it-codigo-orig                                                 */
/*            tt-carga.it-codigo-temp                                                 */
/*            tt-carga.ge-codigo                                                      */
/*            tt-carga.un-de                                                          */
/*            tt-carga.un-para                                                        */
/*            tt-carga.fator-conv                                                     */
/*            tt-carga.descricao                                                      */
/* /*            tt-carga.narrativa VIEW-AS EDITOR SIZE 60 BY 2 */                    */
/*            tt-carga.fm-codigo                                                      */
/*            tt-carga.cod-refer                                                      */
/*            tt-carga.cod-orig                                                       */
/*            tt-carga.class-fiscal                                                   */
/*            tt-carga.ex-ncm                                                         */
/*            tt-carga.controla_estoque                                               */
/*            tt-carga.quant-segur                                                    */
/*            tt-carga.cod-servico                                                    */
/*            tt-carga.fm-cod-com                                                     */
/*            tt-carga.tipo-item                                                      */
/*            tt-carga.cod-depto                                                      */
/*            tt-carga.it-codigo-para                                                 */
/*            tt-carga.log-carga VIEW-AS EDITOR SIZE 50 BY 2 COLUMN-LABEL "Mensagens" */
/*                WITH WIDTH 500 STREAM-IO .                                          */

    ASSIGN tt-carga.log-carga = TRIM(tt-carga.log-carga,"| ").
    DISP   tt-carga.ep-codigo       
           tt-carga.id-klassmatt    
           tt-carga.it-codigo-orig  
           tt-carga.it-codigo-temp  
           tt-carga.it-codigo-para  
           tt-carga.ge-codigo       
           tt-carga.un-de           
           tt-carga.un-para         
           tt-carga.fator-conv      
           tt-carga.descricao       
/*            tt-carga.narrativa VIEW-AS EDITOR SIZE 60 BY 2 */
           tt-carga.log-carga VIEW-AS EDITOR SIZE 60 BY 2 COLUMN-LABEL "Mensagens" 
               WITH WIDTH 500 STREAM-IO .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-retornows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retornows Procedure 
PROCEDURE pi-retornows :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


FOR EACH tt-carga
    WHERE tt-carga.executado = TRUE:
    CREATE es-integra-retorno.
    ASSIGN es-integra-retorno.codigo        = IF tt-carga.it-codigo-para = "" THEN tt-carga.it-codigo-temp ELSE tt-carga.it-codigo-para
           es-integra-retorno.dt-carga      = TODAY
           es-integra-retorno.dt-int-erp    = TODAY
           es-integra-retorno.IdKlassmatt   = INTEGER(tt-carga.id-klassmatt)
           es-integra-retorno.log-retorno   = tt-carga.log-carga
           es-integra-retorno.statusRetorno = IF tt-carga.finalizado THEN "S" ELSE "N"
           es-integra-retorno.dt-ret        = ?. /* Lembrar de deixar ? para poder retornar WS*/
END.

RUN esp/retornows.p NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-validar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validar Procedure 
PROCEDURE pi-validar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ia-int      AS INTEGER     NO-UNDO.
DEFINE VARIABLE para-dupli  AS LOGICAL     NO-UNDO.

DEFINE BUFFER btt-carga FOR tt-carga.

    Carga:
    FOR EACH tt-carga:

        run pi-acompanhar in h-acomp (input "Validaá∆o item:":U + tt-carga.it-codigo-orig).

        /* Empresa */
        IF tt-carga.ep-codigo <> i-ep-codigo-usuario THEN DO:
            ASSIGN tt-carga.log-carga = "C¢digo de empresa diferente sess∆o do TOTVS 11"
                   tt-carga.executado = TRUE.
            NEXT Carga.
        END.

        /* Fam°lia de materiais */
        IF NOT CAN-FIND(FIRST familia
                        WHERE familia.fm-codigo = tt-carga.fm-codigo NO-LOCK) THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Sub-Grupo(Familia Material) " + QUOTER(tt-carga.fm-codigo) + " n∆o encontrada no ERP."
                   tt-carga.executado = TRUE.
        END.

        /* Fam°lia comercial */
        /* 09/04/2015 Se receber NA da planilha deixa em branco */
        IF REPLACE(REPLACE(TRIM(tt-carga.fm-cod-com),".",""),"/","") = "NA" THEN
            ASSIGN tt-carga.fm-cod-com = "".
        IF  TRIM(tt-carga.fm-cod-com) <> "" AND
            NOT CAN-FIND(FIRST fam-comerc
                        WHERE fam-comerc.fm-cod-com = tt-carga.fm-cod-com NO-LOCK) THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Fam°lia(Familia Comercial) " + QUOTER(tt-carga.fm-cod-com) + " n∆o encontrada no ERP."
                   tt-carga.executado = TRUE.
        END.

        /* Verificar se o item j† foi integrado anteriormente */
        FIND FIRST es-de-para-it-padr
            WHERE es-de-para-it-padr.ep-codigo      = tt-carga.ep-codigo
              AND es-de-para-it-padr.it-codigo-orig = tt-carga.it-codigo-orig NO-LOCK NO-ERROR.
        IF AVAIL es-de-para-it-padr THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Item " + QUOTER(tt-carga.it-codigo-orig) + " j† foi catalogado anteriormente para " + QUOTER(es-de-para-it-padr.it-codigo-padr)
                   tt-carga.executado = TRUE
                   tt-carga.it-codigo-para = es-de-para-it-padr.it-codigo-padr.
/*             NEXT Carga.  */
        END.

        /* Verificar se item origem existe e esta ativo no ERP */
        FOR FIRST bitem-orig FIELDS(it-codigo cod-obsoleto cod-estabel ge-codigo un tipo-contr desc-item codigo-orig tipo-contr quant-segur cod-servico char-2)
            WHERE bitem-orig.it-codigo = tt-carga.it-codigo-orig NO-LOCK: END.
        IF NOT AVAIL bitem-orig THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Item " + QUOTER(tt-carga.it-codigo-orig) + " n∆o encontrado no ERP."
                   tt-carga.executado = TRUE.
            NEXT Carga.
        END.
        IF bitem-orig.cod-obsoleto <> 1 /* Ativo */ THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Item " + QUOTER(tt-carga.it-codigo-orig) + " esta " + QUOTER({ininc/i17in172.i 04 bitem-orig.cod-obsoleto}) 
                   tt-carga.executado = TRUE.
            NEXT Carga.
        END.
        IF bitem-orig.tipo-contr <> 2 /* Controle total */ AND
           bitem-orig.tipo-contr <> 4 /* debito direto */ THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Item " + QUOTER(tt-carga.it-codigo-orig) + " È " + QUOTER({ininc/i09in122.i 04 bitem-orig.tipo-contr}) + " e deve ser Debito Direto ou Controle Total." 
                   tt-carga.executado = TRUE.
/*             NEXT Carga. */
        END.
        ASSIGN tt-carga.old-ge-codigo    = bitem-orig.ge-codigo
               tt-carga.ge-codigo        = bitem-orig.ge-codigo
               tt-carga.un-de            = bitem-orig.un
               tt-carga.un-para          = bitem-orig.un
               tt-carga.desc-item-orig   = bitem-orig.desc-item
               tt-carga.cod-orig         = bitem-orig.codigo-orig
               tt-carga.controla_estoque = bitem-orig.tipo-contr
               tt-carga.quant-segur      = bitem-orig.quant-segur
               tt-carga.cod-servico      = bitem-orig.cod-servico
               tt-carga.char-2           = bitem-orig.char-2
               tt-carga.fator-conv       = 1
               .
        FOR FIRST es-it-depto
            WHERE es-it-depto.it-codigo = tt-carga.it-codigo-orig NO-LOCK:
            ASSIGN tt-carga.cod-depto = es-it-depto.cod-depto.
        END.
              

        /* Verificar se tem CFA */
        FIND FIRST ext-item-cfa
            WHERE ext-item-cfa.ep-codigo   = tt-carga.ep-codigo
              AND ext-item-cfa.it-codigo   = tt-carga.it-codigo-orig NO-LOCK NO-ERROR.
        IF NOT AVAIL ext-item-cfa THEN DO:
            ASSIGN tt-carga.log-carga = tt-carga.log-carga + " | " + 
                                        "Item " + QUOTER(tt-carga.it-codigo-orig) + " n∆o tem CFA."
                   tt-carga.executado = TRUE.
            NEXT Carga.
        END.

        /* Verificar se o item j† foi integrado anteriormente em qualquer empresa */
        IF tt-carga.it-codigo-temp BEGINS "XX" THEN DO:
            FIND FIRST es-de-para-it-padr
                WHERE es-de-para-it-padr.it-codigo-temp = tt-carga.it-codigo-temp NO-LOCK NO-ERROR.
            IF AVAIL es-de-para-it-padr THEN DO:
                ASSIGN tt-carga.it-codigo-temp = ""
                       tt-carga.it-codigo-para = es-de-para-it-padr.it-codigo-padr.
            END.
            ELSE DO:
                /* Verificar se j† foi atribu°do na mesma planilha de carga */
                ASSIGN para-dupli = NO.
                btt-carga:
                FOR EACH btt-carga:
                    IF SUBSTRING(btt-carga.it-codigo-para,3) = SUBSTRING(tt-carga.it-codigo-temp,3) THEN DO:
                        ASSIGN tt-carga.it-codigo-para = btt-carga.it-codigo-para
                               tt-carga.it-codigo-temp = "".

                        ASSIGN para-dupli = TRUE.
                        LEAVE btt-carga.
                    END.
                        
                END.
                /* Atribui novo c¢digo com CFA, PRIMEIRA VEZ DO TEMP */
                IF NOT para-dupli THEN
                    ASSIGN tt-carga.it-codigo-para = SUBSTRING(ext-item-cfa.classe,1,2) +
                                                     SUBSTRING(tt-carga.it-codigo-temp,3).
            END.

        END.
        ELSE
            ASSIGN tt-carga.it-codigo-para = tt-carga.it-codigo-temp 
                   tt-carga.it-codigo-temp = "".

         FOR EACH bitem-para 
                  WHERE bitem-para.it-codigo = tt-carga.it-codigo-para 
                  NO-LOCK:
            EMPTY TEMP-TABLE tt-altera.
            EMPTY TEMP-TABLE tt-elimina.

            CREATE tt-elimina.
            ASSIGN tt-elimina.it-codigo  = bitem-para.it-codigo
                   tt-elimina.un         = bitem-para.un
                   tt-elimina.descricao  = bitem-para.desc-item.

            RUN pi-cd0205rp.

            FOR FIRST es-it-depto
                WHERE es-it-depto.it-codigo = bitem-para.it-codigo:
                DELETE es-it-depto.
            END.

            FOR EACH item-fornec
                     WHERE item-fornec.it-codigo = bitem-para.it-codigo:
               DELETE item-fornec.
            END.

         END.


        /* Verificar se item para existe e esta ativo no ERP */
        FOR FIRST bitem-para FIELDS(it-codigo cod-obsoleto cod-estabel)
            WHERE bitem-para.it-codigo = tt-carga.it-codigo-para NO-LOCK: END.
        IF NOT AVAIL bitem-para THEN
            ASSIGN tt-carga.it-sit-para = 1. /* N∆o existe */
        ELSE DO:
            IF bitem-para.cod-obsoleto = 4 /* TOTAL OBSOLETO */ THEN
                ASSIGN tt-carga.it-sit-para = 2. /* Obsoleto */
            ELSE
                ASSIGN tt-carga.it-sit-para = 3. /* Ativo */
        END.

        IF tt-carga.it-codigo-para <> "" THEN DO:
            FIND FIRST btt-carga
                WHERE btt-carga.it-codigo-para = tt-carga.it-codigo-para
                  AND ROWID(btt-carga) <> ROWID(tt-carga) NO-LOCK NO-ERROR.
            IF AVAIL btt-carga THEN
                ASSIGN tt-carga.it-sit-para = 3. /* Ativo */
        END.

        /* Cria tabela de extens∆o do item */
        FOR FIRST bitem-orig FIELDS(it-codigo cod-obsoleto cod-estabel ge-codigo un tipo-contr desc-item codigo-orig tipo-contr quant-segur cod-servico char-2)
            WHERE bitem-orig.it-codigo = tt-carga.it-codigo-orig NO-LOCK: END.
        IF AVAIL bitem-orig THEN DO:
           FOR EACH item-uni-estab 
                    WHERE item-uni-estab.it-codigo = bitem-orig.it-codigo
                    NO-LOCK:
              CREATE tt-item-uni-estab.
              BUFFER-COPY item-uni-estab TO tt-item-uni-estab.
              ASSIGN tt-item-uni-estab.it-codigo-para = tt-carga.it-codigo-para.
           END.
        END.




    END. /* tt-carga */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


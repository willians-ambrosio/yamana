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
{include/i-prgvrs.i YMCD0202RP 11.5.12.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMCD0202RP 11.5.12.000}
&ENDIF

/* ***************************  Definitions  ************************** */
&global-define programa YMCD0202RP

/* Vari veis Globais */    
{utp/ut-glob.i}

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.
DEFINE VARIABLE c-item-altera AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt-emp NO-UNDO
    FIELD ep-codigo   AS CHARACTER.

Define Temp-table ttErrosConexao No-undo
    Field i-sequen As Integer
    Field cd-erro As Integer
    Field mensagem As Character Format "x(255)".

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.
 
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
    tt-param.arquivo    no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PAR¶METROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELE€ÇO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÇO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usu rio * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

find ems2cadme.empresa
    where empresa.ep-codigo = v_cdn_empres_usuar
    no-lock no-error.
find first param-global no-lock no-error.

{utp/ut-liter.i Cadastros * }
assign c-sistema = return-value.
{utp/ut-liter.i Implantar Item replicado * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "11.5.11":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.


def temp-table tt-versao-integr no-undo
       field cod-versao-integracao as integer format "999"
       field ind-origem-msg        as integer format "99".
 
def temp-table tt-erros-geral no-undo
       field identif-msg           as char    format "x(60)"
       field num-sequencia-erro    as integer format "999"
       field cod-erro              as integer format "99999"   
       field des-erro              as char    format "x(60)"
       field cod-maq-origem        as integer format "999"
       field num-processo          as integer format "999999999".
 

/* {cdp/cdapi244.i} */
def temp-table tt-item no-undo like item 
    field cod-maq-origem   as   integer format "9999"
    field num-processo     as   integer format ">>>>>>>>9" initial 0
    field num-sequencia    as   integer format ">>>>>9"    initial 0
    field ind-tipo-movto   as   integer format "99"        initial 1
    field cod-erro         as   integer format "99999" 
    field des-erro         as   char    format "x(60)"
    INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                num-processo
                                num-sequencia.

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
         HEIGHT             = 9.46
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

do on stop undo, leave:
    {include/i-rpout.i}
    view frame f-cabec.
    view frame f-rodape.    
    run utp/ut-acomp.p persistent set h-acomp.  
    
/*     {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c} */
    
    run pi-inicializar in h-acomp (input "Implanta‡Æo de item...":U). 
    
    /*:T --- Colocar aqui o código de impressão --- */
    run pi-acompanhar in h-acomp (input "Lendo fila de implanta‡Æo/altera‡Æo":U).
    RUN pi-implantar.

    run pi-finalizar in h-acomp.
    {include/i-rpclo.i}

     

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-implantar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-implantar Procedure 
PROCEDURE pi-implantar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-log    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-limite AS INTEGER     NO-UNDO.
DEFINE BUFFER bitem  FOR ITEM.
DEFINE BUFFER bbitem FOR ITEM.

ASSIGN c-item-altera = "".

CREATE tt-versao-integr. 
ASSIGN tt-versao-integr.cod-versao-integracao = 001     
       tt-versao-integr.ind-origem-msg        = 01.     

ASSIGN i-limite = 0.
Gravar:
FOR EACH es-fila-rep-item
    WHERE es-fila-rep-item.ep-codigo = v_cdn_empres_usuar EXCLUSIVE-LOCK
    TRANSACTION ON ERROR UNDO Gravar, NEXT Gravar:

    ASSIGN i-limite = i-limite + 1.
    IF i-limite >= 1000 THEN
        LEAVE Gravar.

    run pi-acompanhar in h-acomp (input "Item: ":U + es-fila-rep-item.it-codigo).

    EMPTY TEMP-TABLE tt-item.
    EMPTY TEMP-TABLE tt-erros-geral.    

    FOR FIRST bitem
        WHERE bitem.it-codigo = es-fila-rep-item.it-codigo NO-LOCK: END.

                

    /* Criar item obsoleto */
    IF NOT AVAIL bitem THEN DO:
        CREATE tt-item.

        BUFFER-COPY es-fila-rep-item TO tt-item
        ASSIGN tt-item.cod-obsoleto   = 4.                                                                   
               tt-item.ind-tipo-movto = 1. /*1 inclusao / 2 altera‡Æo*/                                      
        FOR FIRST familia FIELDS(deposito-pad)
            WHERE familia.fm-codigo = tt-item.fm-codigo NO-LOCK:
            ASSIGN tt-item.deposito-pad = familia.deposito-pad.
        END.
        IF tt-item.cod-estabel <> "" THEN
            FOR FIRST estabelec FIELDS(cod-estabel)
                WHERE estabelec.cod-estabel = tt-item.cod-estabel
                  AND estabelec.ep-codigo   = v_cdn_empres_usuar NO-LOCK:
            END.
        IF tt-item.cod-estabel = "" OR
           NOT AVAIL estabelec      THEN DO:
            FOR FIRST fam-uni-estab FIELDS (cod-estabel)
                WHERE fam-uni-estab.fm-codigo = tt-item.fm-codigo NO-LOCK:
                ASSIGN tt-item.cod-estabel = fam-uni-estab.cod-estabel.
            END.
        END.

        RUN cdp/cdapi344.p ( INPUT        TABLE tt-versao-integr,          
                             OUTPUT       TABLE tt-erros-geral,           
                             INPUT-OUTPUT TABLE tt-item).
    END.
    /* Altera‡Æo do item */
    ELSE DO:

        ASSIGN c-item-altera = c-item-altera + es-fila-rep-item.it-codigo + ",".

        FIND bbitem 
             WHERE bbitem.it-codigo = bitem.it-codigo
             EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE(bbitem) THEN DO:
           ASSIGN bbitem.desc-item        = es-fila-rep-item.desc-item
                  bbitem.narrativa        = es-fila-rep-item.narrativa
                  bbitem.quant-segur      = es-fila-rep-item.quant-segur 
                  bbitem.fm-codigo        = es-fila-rep-item.fm-codigo   
                  bbitem.codigo-refer     = es-fila-rep-item.codigo-refer
                  bbitem.codigo-orig      = es-fila-rep-item.codigo-orig 
                  bbitem.class-fiscal     = es-fila-rep-item.class-fiscal
                  bbitem.dec-1            = es-fila-rep-item.dec-1       
                  bbitem.cod-servico      = es-fila-rep-item.cod-servico 
                  bbitem.fm-cod-com       = es-fila-rep-item.fm-cod-com
                  bbitem.tipo-contr       = es-fila-rep-item.tipo-contr.

           OVERLAY(bbitem.char-2,212,1)  = SUBSTRING(es-fila-rep-item.char-2,212,1).

           /* Begins Jan/2018 - Willians Ambrosio DKP */
           FIND FIRST es-klassmatt-integr WHERE
                      es-klassmatt-integr.idklassmatt = 999999999                                   AND
                      es-klassmatt-integr.dt-trans    = TODAY                                       AND
                      es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","")     EXCLUSIVE-LOCK NO-ERROR.
           IF NOT AVAIL es-klassmatt-integr THEN
           DO:
              CREATE es-klassmatt-integr.
              BUFFER-COPY bbitem               TO es-klassmatt-integr
                                           ASSIGN es-klassmatt-integr.idklassmatt = 999999999                               
                                                  es-klassmatt-integr.dt-trans    = TODAY                                   
                                                  es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","") 
                                                  es-klassmatt-integr.log-retorno   = "YMCD0202-01RP.P - TIPO DE CONTROLE ATUALIZADO Tipo-Contr = "  + STRING(bbitem.tipo-contr)      
                                                  es-klassmatt-integr.statusRetorno = "N".                                              
           END.
           /* End Jan/2018 - Willians Ambrosio DKP */
        END.

/*         BUFFER-COPY bitem TO tt-item.                                                                       */
/*                                                                                                             */
/*         ASSIGN tt-item.desc-item = es-fila-rep-item.desc-item.                                              */
/*                                                                                                             */
/*                                                                                                             */
/* /*         BUFFER-COPY es-fila-rep-item EXCEPT ge-codigo cod-estabel dt-criacao data-implant TO tt-item. */ */
/*         ASSIGN tt-item.ind-tipo-movto = 2. /*1 inclusao / 2 altera‡Æo */                                    */
    END.

    ASSIGN c-log = "".
    FIND FIRST  tt-erros-geral NO-LOCK  NO-ERROR.
    IF AVAIL tt-erros-geral THEN DO:
        FOR EACH tt-erros-geral:
            ASSIGN c-log = c-log  + "|" + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro.

        END.
        ASSIGN es-fila-rep-item.dt-ult-tentativa = NOW
               es-fila-rep-item.mensagem-erro    = TRIM(c-log,"|")
               es-fila-rep-item.nr-tentativas    = es-fila-rep-item.nr-tentativas + 1.
    END.
    ELSE DO:
        FIND ITEM WHERE ITEM.it-codigo = es-fila-rep-item.it-codigo EXCLUSIVE-LOCK.
        IF AVAIL ITEM THEN DO:
            ASSIGN ITEM.class-fiscal = ems5_esp.es-fila-rep-item.class-fiscal.

            IF LOOKUP(ITEM.it-codigo,c-item-altera) = 0 THEN DO:
                FOR EACH item-uni-estab FIELDS(it-codigo cod-obsoleto)
                   WHERE item-uni-estab.it-codigo = ITEM.it-codigo EXCLUSIVE-LOCK:
                    ASSIGN item-uni-estab.cod-obsoleto = 4.
                END.
            END.

        END.

        FIND FIRST es-it-depto 
             WHERE es-it-depto.it-codigo = es-fila-rep-item.it-codigo 
             EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL es-it-depto THEN DO:
            CREATE es-it-depto.
            ASSIGN es-it-depto.it-codigo  = es-fila-rep-item.it-codigo
                   es-it-depto.data       = TODAY
                   es-it-depto.hora       = STRING(TIME,"hh:mm")
                   es-it-depto.origem     = "Carga via replica‡Æo"
                   es-it-depto.usuario    = "Sistema".
        END.

        ASSIGN  es-it-depto.cod-depto      = INTEGER(es-fila-rep-item.cod-depto)
                es-it-depto.data-altera    = TODAY
                es-it-depto.hr-altera      = STRING(TIME,"hh:mm")
                es-it-depto.usuar-altera   = "Sistema".
       
                
        DELETE es-fila-rep-item.
    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


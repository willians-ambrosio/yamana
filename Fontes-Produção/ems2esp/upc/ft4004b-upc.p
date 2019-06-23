/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i FT4004B-UPC 1.00.00.000}  /*** 010000 ***/
{include/i-epc200.i1}

/***************** DefiniØ o de Parametros ************************************/
def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as rowid         no-undo.

/* V†riaveis Globais */
DEF NEW GLOBAL SHARED VAR wgh-fiItCodigo-FT4004b         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-fiDescItem-FT4004b         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-fpage0-FT4004b             AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-fiUn2-FT4004b              AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-fiDescItem-FT4004b         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-fiNatOperacao-FT4004b      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-ficDescNatureza-ft4004b    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-wgh-object-ft4004            as handle        no-undo.

/*Variaveis do Leave Padr∆o*/
def var c-char-aux              as char    no-undo.
def var h-bodi317sd-upc         as handle  no-undo.
def var i-wt-docto-sequencia    AS INTEGER NO-UNDO.
def var i-wt-it-docto-sequencia AS INTEGER NO-UNDO.
DEF VAR l-habilita-campo        AS LOGICAL NO-UNDO.
DEF VAR l-proc-ok-aux           AS LOGICAL NO-UNDO.
DEF VAR c-un                    AS CHAR    NO-UNDO.
/*Fim Variaveis do Leave Padr∆o*/

DEF VAR h-boesp AS HANDLE NO-UNDO.
DEF VAR c-natur-oper-nota  AS CHARACTER NO-UNDO.

/* Buffers */
DEF BUFFER bf-wt-it-docto FOR wt-it-docto.
DEF BUFFER bf-wt-docto    FOR wt-docto.

/*
message 
        "Evento..........:" string(p-ind-event)         skip  
        "Objeto..........:" string(p-ind-object)        skip      
        "Handle do Objeto:" string(p-wgh-object:NAME)   skip 
        "Handle da Frame.:" string(p-wgh-frame:NAME)    skip 
        "Tabela..........:" p-cod-table                 skip      
        "Rowid...........:" string(p-row-table)         skip      
        "p-wgh-object....:" p-wgh-object:file-name.
*/


IF p-ind-event = "AFTER-INITIALIZE" THEN DO:

    /*RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-Event,                                                              
                  INPUT "FRAME",        /*** Type ***/                                             
                  INPUT "fpage0",       /*** Name ***/                                             
                  INPUT NO,             /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,              /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-fpage0-FT4004b).*/

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-Event,                                                              
                  INPUT "FILL-IN",      /*** Type ***/                                             
                  INPUT "it-codigo",    /*** Name ***/                                             
                  INPUT NO,             /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,              /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-fiItCodigo-FT4004b).

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-Event,                                                              
                  INPUT "FILL-IN",      /*** Type ***/                                             
                  INPUT "c-desc-item",  /*** Name ***/                                             
                  INPUT NO,             /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,              /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-fiDescItem-FT4004b).

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-Event,                                                              
                  INPUT "FILL-IN",      /*** Type ***/                                             
                  INPUT "un-2",         /*** Name ***/                                             
                  INPUT NO,             /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,              /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-fiUn2-FT4004b).
    
    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-Event,                                                              
                  INPUT "FILL-IN",      /*** Type ***/                                             
                  INPUT "nat-operacao", /*** Name ***/                                             
                  INPUT NO,             /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,              /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-fiNatOperacao-FT4004b).
     
    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-Event,                                                              
                  INPUT "FILL-IN",             /*** Type ***/                                             
                  INPUT "c-desc-nat-operacao", /*** Name ***/                                             
                  INPUT NO,                    /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,                     /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-ficDescNatureza-ft4004b).
END.

IF VALID-HANDLE(wgh-fiNatOperacao-FT4004b) THEN DO:
    ON 'LEAVE':U OF wgh-fiItCodigo-FT4004b PERSISTENT RUN upc\ft4004b-upc.p ( INPUT "Leave-ItCodigo" ,
                                                                              INPUT p-ind-object,
                                                                              INPUT p-wgh-object,
                                                                              INPUT p-wgh-frame ,
                                                                              INPUT p-cod-table ,
                                                                              INPUT p-row-table ).
END.                

IF p-ind-event = "Leave-ItCodigo" THEN DO:

    RUN buscaChaveWtItDocto IN p-wgh-object-ft4004 (OUTPUT i-wt-docto-sequencia, OUTPUT i-wt-it-docto-sequencia). 

    FOR FIRST bf-wt-docto EXCLUSIVE-LOCK
        WHERE bf-wt-docto.seq-wt-docto = i-wt-docto-sequencia:

            RUN esbo/boes001.p PERSISTENT SET h-boesp.

            RUN getNaturezaItemNota IN h-boesp (INPUT  i-wt-docto-sequencia,
                                                INPUT  wgh-fiItCodigo-FT4004b:SCREEN-VALUE,
                                                OUTPUT c-natur-oper-nota).

            IF c-natur-oper-nota <> "" THEN
                ASSIGN wgh-fiNatOperacao-FT4004b:SCREEN-VALUE = c-natur-oper-nota.

            DELETE OBJECT h-boesp.
            ASSIGN h-boesp = ?.

    END.

    /** Replica o Leave da tela padr∆o**/
    run dibo/bodi317sd.p    persistent set h-bodi317sd-upc.

    run getDescriptionField in h-bodi317sd-upc(input  "it-codigo",
                                               input  wgh-fiItCodigo-FT4004b:SCREEN-VALUE,
                                               output c-char-aux).
    assign wgh-fiDescItem-FT4004b:screen-value = c-char-aux.

    run searchDefaultUOM in h-bodi317sd-upc (input wgh-fiItCodigo-FT4004b:SCREEN-VALUE,
                                             input  i-wt-docto-sequencia,
                                             output c-un,
                                             output l-proc-ok-aux).
    ASSIGN wgh-fiUn2-FT4004b:screen-value  = c-un.
        
    run getEnableFieldUn2 in h-bodi317sd-upc (input wgh-fiItCodigo-FT4004b:SCREEN-VALUE,
                                              output l-habilita-campo).
    assign wgh-fiUn2-FT4004b:sensitive = l-habilita-campo.
    
    if  valid-handle(h-bodi317sd-upc)    then DELETE PROCEDURE h-bodi317sd-upc.
    /** Replica o Leave da tela padr∆o**/
END.

PROCEDURE tela-upc:

    DEFINE INPUT  PARAMETER  pWghFrame    AS WIDGET-HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER  pIndEvent    AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pObjType     AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pObjName     AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pApresMsg    AS LOGICAL       NO-UNDO.
    DEFINE INPUT  PARAMETER  pAux         AS INTEGER       NO-UNDO.
    DEFINE OUTPUT PARAMETER  phObj        AS HANDLE        NO-UNDO.
    
    DEFINE VARIABLE wgh-obj AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE i-aux   AS INTEGER       NO-UNDO.

    ASSIGN wgh-obj = pWghFrame:FIRST-CHILD
           i-aux   = 0.

    DO WHILE VALID-HANDLE(wgh-obj):              
        
        IF pApresMsg = YES THEN
            MESSAGE "Nome do Objeto" wgh-obj:NAME SKIP
                    "Type do Objeto" wgh-obj:TYPE SKIP
                    "P-Ind-Event"    pIndEvent VIEW-AS ALERT-BOX.
        
        IF wgh-obj:TYPE = pObjType AND
           wgh-obj:NAME = pObjName THEN DO:
            ASSIGN phObj = wgh-obj:HANDLE
                   i-aux = i-aux + 1.

            IF i-aux = pAux THEN
                LEAVE.
        END.
        IF wgh-obj:TYPE = "field-group" THEN
            ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
        ELSE 
            ASSIGN wgh-obj = wgh-obj:NEXT-SIBLING.
    END.

END PROCEDURE.


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : esp/esce012rp.p
    Description : Etiqueta de Estoque
    Author(s)   : Paulo Henrique Monte
    Created     : 12/06/2007
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
def buffer empresa for ems2cadme.empresa.

/* ***************************  Definitions  ************************** */
{include/i-prgvrs.i esce012rp 2.06.00.000}.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD item-ini         AS CHAR
    FIELD item-fim         AS CHAR
    FIELD cod-estabel-ini  AS CHAR FORMAT "X(03)"
    FIELD cod-estabel-fim  AS CHAR FORMAT "X(03)".

define temp-table tt-digita no-undo
    FIELD id                AS CHAR
    field it-codigo         as CHAR FORMAT "x(16)"
    FIELD cod-estabel       AS CHAR FORMAT "X(03)"
    field qtde              as DEC  FORMAT ">>,>>9.9999"    
    FIELD qtde-etiqueta     AS INT 
    FIELD observacao        AS CHAR FORMAT "x(24)"
/*     index id it-codigo */
    .


def var witem        as c format "x(8)".
def var wtot         as dec format  ">>>,>>>,>>9.9999" init 0.
def var wcont        as int init 0.


DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS raw.

/***********************************************************************
                              PAR¶METROS
 ***********************************************************************/
DEFINE INPUT PARAMETER raw-param AS raw NO-UNDO.
DEFINE INPUT PARAMETER table FOR tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param NO-ERROR.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

/***********************************************************************
                         VARIµVEIS INTERNAS
 ***********************************************************************/
/* acompanhamento */
DEFINE VARIABLE h-acomp AS HANDLE NO-UNDO.                
                                                                
DEFINE VARIABLE i-cont AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-etiq AS INTEGER    NO-UNDO.

DEFINE VARIABLE c-linha1 AS CHAR FORMAT "x(36)"  EXTENT 4  NO-UNDO.
DEFINE VARIABLE c-linha2 AS CHAR FORMAT "x(35)"  EXTENT 4  NO-UNDO.
DEFINE VARIABLE c-linha3 AS CHAR FORMAT "x(36)"  EXTENT 4  NO-UNDO.
DEFINE VARIABLE c-linha4 AS CHAR FORMAT "x(36)"  EXTENT 4  NO-UNDO.
DEFINE VARIABLE c-linha5 AS CHAR FORMAT "x(36)"  EXTENT 4  NO-UNDO.
DEFINE VARIABLE c-linha6 AS CHAR FORMAT "x(36)"  EXTENT 4  NO-UNDO.

DEF VAR c-emp  AS CHAR FORMAT "x(36)".

DEFINE VARIABLE i AS INTEGER    NO-UNDO.

{utp/ut-glob.i}
{include/i-rpvar.i}

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
   Type: Procedure
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
         HEIGHT             = 7.25
         WIDTH              = 29.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
find empresa no-lock 
    where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  not avail empresa then
    return "ADM-ERROR":U.

find first param-global no-lock no-error.
if  available param-global then
    assign c-empresa = param-global.grupo.

assign c-programa     = "ESCE012RP":U
       c-versao       = "2.06":U
       c-revisao      = ".00.000":U
       c-empresa      = empresa.razao-social
       c-emp          = param-global.grupo.

{include/tt-edit.i}

{include/i-rpout.i &pagesize = 0}

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}

run pi-inicializar in h-acomp (input RETURN-VALUE).

/*
ASSIGN i = 1.
FOR EACH b-digita
    WHERE b-digita.qtde-etiqueta > 1:

    run pi-acompanhar in h-acomp (input b-digita.it-codigo).

    DO i = 1 TO (b-digita.qtde-etiqueta - 1):
        CREATE tt-digita.
        ASSIGN tt-digita.it-codigo     = b-digita.it-codigo
               tt-digita.qtde          = b-digita.qtde
               tt-digita.observacao    = b-digita.observacao
               tt-digita.qtde-etiqueta = 0.
    END.
END.
*/
RUN pi-executar.

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-executar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar Procedure 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-localiz AS CHAR  NO-UNDO.

ASSIGN i-cont = 0.

FOR EACH tt-digita NO-LOCK,
    FIRST ITEM
    WHERE ITEM.it-codigo = tt-digita.it-codigo NO-LOCK
    BREAK BY tt-digita.id:

        FIND FIRST item-uni-estab
             WHERE item-uni-estab.cod-estabel   = tt-digita.cod-estabel
               AND item-uni-estab.it-codigo     = ITEM.it-codigo
             NO-LOCK NO-ERROR.
        IF AVAIL item-uni-estab THEN
            ASSIGN c-localiz = item-uni-estab.cod-localiz.
        ELSE
            ASSIGN c-localiz = ITEM.cod-localiz.

        run pi-acompanhar in h-acomp (input tt-digita.it-codigo).

        DO i = 1 TO (tt-digita.qtde-etiqueta):

            ASSIGN i-cont           = i-cont + 1.
            ASSIGN c-linha1[i-cont] = c-emp   /*.razao-social */
                   c-linha2[i-cont] = "Codigo: "   + tt-digita.it-codigo 
                   c-linha3[i-cont] = "Descricao: "   + item.desc-item 
                   c-linha4[i-cont] = "Dep: " + ITEM.deposito-pad +  "     Localiz.: " + c-localiz
                   c-linha5[i-cont] = "Quantidade: " + string(tt-digita.qtde,">>>,>>9.99") + "  UN.: " + ITEM.un
                   c-linha6[i-cont] = "Observacao: " + tt-digita.observacao.


            IF i-cont = 3 THEN DO:
                PUT c-linha1[1]  AT 2
                    c-linha1[2]  AT 44 /*39*/
                    c-linha1[3]  AT 87 /* 76 */
                    c-linha1[4]  AT 124 SKIP
                    c-linha2[1]  AT 2
                    c-linha2[2]  AT 44 /*39*/
                    c-linha2[3]  AT 87 /*76 */
                    c-linha2[4]  AT 124 SKIP
                    c-linha3[1]  AT 2
                    c-linha3[2]  AT 44 /*39*/
                    c-linha3[3]  AT 87 /*76*/
                    c-linha3[4]  AT 124 SKIP
                    c-linha4[1]  AT 2
                    c-linha4[2]  AT 44 /*39*/
                    c-linha4[3]  AT 87 /*76 */
                    c-linha4[4]  AT 124 SKIP
                    c-linha5[1]  AT 2
                    c-linha5[2]  AT 44 /*39 */
                    c-linha5[3]  AT 87 /*76 */
                    c-linha5[4]  AT 124 SKIP
                    c-linha6[1]  AT 2
                    c-linha6[2]  AT 44 /*39 */
                    c-linha6[3]  AT 87 /*76*/
                    c-linha6[4]  AT 124 SKIP(3).

                DO i-cont = 1 TO 3:
                    ASSIGN c-linha1[i-cont] = ""
                           c-linha2[i-cont] = ""
                           c-linha3[i-cont] = ""
                           c-linha4[i-cont] = ""
                           c-linha5[i-cont] = ""
                           c-linha6[i-cont] = "".
                END.
                ASSIGN i-cont = 0.
            END.
        END.
        
        IF LAST(tt-digita.id) AND i-cont > 0 THEN DO:

            PUT c-linha1[1]  AT 2
                c-linha1[2]  AT 44
                c-linha1[3]  AT 87 
                c-linha1[4]  AT 124 SKIP
                c-linha2[1]  AT 2
                c-linha2[2]  AT 44
                c-linha2[3]  AT 87 
                c-linha2[4]  AT 124 SKIP
                c-linha3[1]  AT 2
                c-linha3[2]  AT 44
                c-linha3[3]  AT 87 
                c-linha3[4]  AT 124 SKIP
                c-linha4[1]  AT 2
                c-linha4[2]  AT 44
                c-linha4[3]  AT 87 
                c-linha4[4]  AT 124 SKIP
                c-linha5[1]  AT 2
                c-linha5[2]  AT 44
                c-linha5[3]  AT 87 
                c-linha5[4]  AT 124 SKIP
                c-linha6[1]  AT 2
                c-linha6[2]  AT 44
                c-linha6[3]  AT 87 
                c-linha6[4]  AT 124 SKIP(3).
        END.
        
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


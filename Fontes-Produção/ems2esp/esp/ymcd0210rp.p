&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* --------------------------------------------------------------------------------------- 
                                                                                        
   Sistema................: TOTVS                                                       
   Modulo.................: cadastros gerais                                                 
                                                                                           
   Programa...............:                                               
   Sub Programa...........:                                                                
                                                                                            
   Descricao..............: Importar classificaá∆o fiscal
   programa para consultar: 
                             
   Entidade Desenvolvedora: DSC PRAXIS
   
   tabelas usadas.........: 
   importante.............:  
                                                                                           
   Hist∏rico Programa -------------------------------------------------------------------+ 
   | Data       | Autor               | Descricao                                        | 
   +----------- +---------------------+--------------------------------------------------+ 
   | 01/09/2016 | Marcos A.Souza      | Desenvolvimento do Programa                      | 
   +------------+---------------------+--------------------------------------------------+ 
*/
{include/i-prgvrs.i ymcd0210rp 2.12.10.001}

define temp-table tt-importar NO-UNDO
    field it-codigo             like ITEM.it-codigo               /*C¢digo do Item*/
    field class-fiscal          like item.class-fiscal
    FIELD estado                AS INTEGER
    FIELD char-1                AS CHARACTER.
  

define temp-table tt-log NO-UNDO
    field it-codigo             like item.it-codigo               /*C¢digo do Item*/
    field class-fiscal          like item.class-fiscal
    FIELD estado                AS CHARACTER.

 define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.


DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita       AS RAW.

/* definicao de parametros */
DEF INPUT PARAM raw-param  AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

/* transferencia de parametros */
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* Ativar somente quando tiver (tt-digita)      */
/* FOR EACH tt-raw-digita:                                 */
/*     create tt-digita.                                   */
/*     RAW-TRANSFER tt-raw-digita.raw-digita to tt-digita. */
/* END.                                                    */
/*                                                         */
{include/i-rpvar.i} 

/* definiá∆o de vari†veis  */

DEFINE VARIABLE h_acomp           AS HANDLE                            NO-UNDO. 
DEFINE VARIABLE c-modelo          AS CHAR FORMAT "X(256)"              NO-UNDO.
DEFINE VARIABLE excelappl         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE workbooks         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE worksheets        AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE i-lin             AS INTEGER                           NO-UNDO.
DEFINE VARIABLE i-col             AS INTEGER                           NO-UNDO.
DEFINE VARIABLE c-linha           AS CHARACTER                         NO-UNDO.
define stream   s-arq-ent.

DEFINE VARIABLE l-error AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hInstance      AS INTEGER                   NO-UNDO.

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
         HEIGHT             = 14.83
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
FIND FIRST tt-param NO-LOCK NO-ERROR.
IF NOT AVAIL tt-param THEN NEXT.

FIND FIRST param-global NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h_acomp.
RUN pi-inicializar IN h_acomp (INPUT "Gerando Resumo").

run pi-importar.        

RUN pi-finalizar IN h_acomp.
{include\i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-importar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar Procedure 
PROCEDURE pi-importar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
input from value(tt-param.arq-entrada) CONVERT SOURCE 'iso8859-1'.

REPEAT:        
        CREATE tt-importar.
        IMPORT DELIMITER ";" tt-importar.
        ASSIGN tt-importar.char-1 = ";".
END.

input close.

ASSIGN l-error = NO.

FOR EACH tt-importar
    WHERE tt-importar.it-codigo > "".

    ASSIGN tt-importar.class-fiscal = REPLACE (tt-importar.class-fiscal,".","").


    FIND FIRST classif-fisc NO-LOCK
        WHERE classif-fisc.class-fiscal = tt-importar.class-fiscal
        NO-ERROR.
    IF NOT AVAIL classif-fisc THEN
    DO:
        CREATE tt-log.
        ASSIGN tt-log.it-codigo    = tt-importar.it-codigo
               tt-log.class-fiscal = tt-importar.class-fiscal
               tt-log.estado       = "Classif.Fiscal inv†lida".
        ASSIGN l-error = YES.
    END.
END.

IF l-error THEN
DO:
    OUTPUT TO c:\temp\tt-log.csv.

    PUT "Item;Class.Fiscal;Estado" SKIP.
    PUT "Erros detectados" SKIP.

    FOR EACH tt-log.
        PUT tt-log.it-codigo ";"
            tt-log.class-fiscal ";" SKIP.
    END.
    OUTPUT TO CLOSE.

    ASSIGN c-modelo = SEARCH("c:\temp\tt-log.csv").

    RUN ShellExecuteA (0,
                       "open",                   
                       c-modelo,
                       "",
                       "",
                       1,
                       OUTPUT hInstance).

END.
ELSE
DO:
    DISABLE TRIGGERS FOR LOAD OF ITEM.

    FOR EACH tt-importar
        WHERE  tt-importar.it-codigo > "".

        RUN pi-acompanhar IN h_acomp (INPUT ' Item: ' + tt-importar.it-codigo).
    
        FOR EACH ITEM EXCLUSIVE-LOCK
            WHERE ITEM.it-codigo = tt-importar.it-codigo.
            ASSIGN ITEM.class-fiscal = tt-importar.class-fiscal.
    
            FOR EACH classif-fisc NO-LOCK
                WHERE classif-fisc.class-fiscal =  ITEM.class-fiscal.
                ASSIGN  ITEM.aliquota-ipi = classif-fisc.aliquota-ipi.
            END.

            ASSIGN tt-importar.estado = 1.
        END.
    END.

    ASSIGN l-error = NO.

    FOR EACH tt-importar
        WHERE tt-importar.estado = 0.

        CREATE tt-log.
        ASSIGN tt-log.it-codigo    = tt-importar.it-codigo
               tt-log.class-fiscal = tt-importar.class-fiscal
               tt-log.estado       = "Item n∆o atualizado".
        ASSIGN l-error = YES.
    END.

    IF l-error THEN
    DO:
        OUTPUT TO c:\temp\tt-log.csv.

        PUT "Item;Class.Fiscal;Estado" SKIP.

        PUT "Erros detectados" SKIP.

        FOR EACH tt-log.
            PUT tt-log.it-codigo ";"
                tt-log.class-fiscal ";" SKIP.
        END.
        OUTPUT TO CLOSE.

        ASSIGN c-modelo = SEARCH("c:\temp\tt-log.csv").

        RUN ShellExecuteA (0,
                           "open",                   
                           c-modelo,
                           "",
                           "",
                           1,
                           OUTPUT hInstance).
    END.
END.


END PROCEDURE.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
def buffer empresa     for ems2cadme.empresa.

/* ***************************  Definitions  ************************** */

{include/i-prgvrs.i esft202rp 2.06.00.000} 
{utp/ut-glob.i} 
{utp/utapi019.i}
/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD i-ini-emp        AS char
    FIELD i-fim-emp        AS char
    FIELD c-ini-gr         AS CHAR FORMAT "x(3)"
    FIELD c-fim-gr         AS CHAR FORMAT "x(3)"
    FIELD c-ini-usuario    AS CHAR FORMAT 'x(12)'
    FIELD c-fim-usuario    AS CHAR FORMAT 'x(12)'
    FIELD l-ativo          AS LOGICAL
    FIELD l-inativo        AS LOGICAL.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE temp-table tt-raw-digita
    field raw-digita as raw.

DEF INPUT PARAM raw-param as raw no-undo.
DEF INPUT PARAM table for tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param to tt-param.


def var excelappl as com-handle no-undo.

DEFINE VARIABLE c-dir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.



/* Ativar somente quando tiver (tt-digita)      */
/* FOR EACH tt-raw-digita:                      */
/*     create tt-digita.                        */
/*     RAW-TRANSFER tt-raw-digita to tt-digita. */
/* END.                                         */
/*                                              */



DEFINE BUFFER bf-usuar_mestre FOR usuar_mestre.
DEFINE BUFFER bf-segur-emp   FOR segur_empres_usuar.

{include/i-rpvar.i} 
/*{esp/api-excel.i}*/

DEF VAR h_acomp_rp     as   handle                  no-undo.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

find first empresa no-lock no-error.

ASSIGN c-programa     = "rnnesxxmmmrp"
       c-versao       = "2.04"
       c-revisao      = "00.000"
       c-empresa      =  empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "Titulo do Relatorio".    

{include\i-rpout.i}
{include\i-rpcab.i}


RUN utp/ut-acomp.p PERSISTENT SET h_acomp_rp.
RUN pi-inicializar IN h_acomp_rp (INPUT "Gerando Excel Aguarde...").

/* Se tiver uma montagem de temp-table */
/* RUN pi-monta. */

ASSIGN c-dir = SESSION:TEMP-DIRECTORY + "RPSegurEmpresa" + STRING(TODAY,"99999999") + ".xls".

RUN pi-imprime.

run pi-finalizar in h_acomp_rp.

{include\i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-by-empresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-by-empresa Procedure 
PROCEDURE pi-by-empresa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

excelappl:range("A1"):VALUE = "EMPRESA".
excelappl:range("B1"):VALUE = "DESCRI€ÇO EMPRESA".
excelappl:range("C1"):VALUE = "USUµRIO".
excelappl:range("D1"):VALUE = "NOME".

ASSIGN i = 2.

FOR EACH empresa NO-LOCK
    WHERE empresa.ep-codigo >= tt-param.i-ini-emp  
    AND   empresa.ep-codigo <= tt-param.i-fim-emp: 

        FIND FIRST segur_empres_usuar NO-LOCK
             WHERE segur_empres_usuar.cod_empresa = STRING(empresa.ep-codigo) NO-ERROR.
        IF AVAIL   segur_empres_usuar THEN
        DO:
            FOR EACH   segur_empres_usuar NO-LOCK
                 WHERE segur_empres_usuar.cod_empresa = STRING(empresa.ep-codigo) AND
                       segur_empres_usuar.cod_usuario >= tt-param.c-ini-usuario   AND
                       segur_empres_usuar.cod_usuario <= tt-param.c-fim-usuario.

                RUN pi-acompanhar IN h_acomp_rp (INPUT "Emp: " + STRING(empresa.ep-codigo) + " - Usuario: " + segur_empres_usuar.cod_usuario).
                
                FIND bf-usuar_mestre OF segur_empres_usuar NO-LOCK NO-ERROR.
                IF NOT AVAIL bf-usuar_mestre THEN NEXT.

                IF tt-param.l-ativo = NO THEN
                    IF bf-usuar_mestre.dat_fim_valid >= TODAY AND 
                       bf-usuar_mestre.dat_inic_valid <= TODAY THEN NEXT.

                IF tt-param.l-inativo = NO THEN
                    IF bf-usuar_mestre.dat_fim_valid <  TODAY OR  
                       bf-usuar_mestre.dat_inic_valid >  TODAY THEN NEXT.

                excelappl:range("A" + STRING(i)):VALUE = empresa.ep-codigo.
                excelappl:range("B" + STRING(i)):VALUE = trim(empresa.nome).
                excelappl:range("C" + STRING(i)):VALUE = bf-usuar_mestre.cod_usuario.
                excelappl:range("D" + STRING(i)):VALUE = bf-usuar_mestre.nom_usuario.
                ASSIGN i = i + 1.

            END.
        END.
        ELSE
        DO:
            FOR EACH bf-usuar_mestre NO-LOCK:
                     bf-usuar_mestre.cod_usuario >= tt-param.c-ini-usuario   AND
                     bf-usuar_mestre.cod_usuario <= tt-param.c-fim-usuario.

                     IF tt-param.l-ativo = NO THEN
                         IF bf-usuar_mestre.dat_fim_valid >= TODAY AND 
                            bf-usuar_mestre.dat_inic_valid <= TODAY THEN NEXT.

                     IF tt-param.l-inativo = NO THEN
                         IF bf-usuar_mestre.dat_fim_valid <  TODAY OR  
                            bf-usuar_mestre.dat_inic_valid >  TODAY THEN NEXT.

                RUN pi-acompanhar IN h_acomp_rp (INPUT "Emp: " + STRING(empresa.ep-codigo) + " - Usuario: " + bf-usuar_mestre.cod_usuario).

                excelappl:range("A" + STRING(i)):VALUE = empresa.ep-codigo.
                excelappl:range("B" + STRING(i)):VALUE = trim(empresa.nome).
                excelappl:range("C" + STRING(i)):VALUE = bf-usuar_mestre.cod_usuario.
                excelappl:range("D" + STRING(i)):VALUE = bf-usuar_mestre.nom_usuario.
                ASSIGN i = i + 1.
            END.

        END.

END.


/* FOR EACH segur_empres_usuar NO-LOCK                                                                                */
/*     WHERE segur_empres_usuar.cod_empresa >= string(tt-param.i-ini-emp)                                             */
/*     AND   segur_empres_usuar.cod_empresa <= string(tt-param.i-fim-emp):                                            */
/*                                                                                                                    */
/*     FIND FIRST empresa WHERE empresa.ep-codigo = INT(segur_empres_usuar.cod_empresa) NO-LOCK NO-ERROR.             */
/*                                                                                                                    */
/*     excelappl:range("A" + STRING(i)):VALUE = segur_empres_usuar.cod_empresa.                                       */
/*     excelappl:range("B" + STRING(i)):VALUE = trim(empresa.nome).                                                   */
/*                                                                                                                    */
/*                                                                                                                    */
/*                                                                                                                    */
/*             FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = usuar_grp_usuar.cod_usuario NO-LOCK NO-ERROR. */
/*                                                                                                                    */
/*             excelappl:range("E" + STRING(i)):VALUE = usuar_grp_usuar.cod_usuario.                                  */
/*             IF AVAIL usuar_mestre THEN                                                                             */
/*                 excelappl:range("F" + STRING(i)):VALUE = usuar_mestre.nom_usuario.                                 */
/*                                                                                                                    */
/*             ASSIGN i = i + 1.                                                                                      */
/*                                                                                                                    */
/*                                                                                                                    */
/*         END.                                                                                                       */
/*                                                                                                                    */
/*     END.                                                                                                           */
/*                                                                                                                    */
/* END.                                                                                                               */

RUN pi-finaliza-excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-by-usuario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-by-usuario Procedure 
PROCEDURE pi-by-usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE c-empresa AS CHARACTER   NO-UNDO.
ASSIGN c-empresa = "".

excelappl:range("A1"):VALUE = "USUµRIO".
excelappl:range("B1"):VALUE = "NOME".
excelappl:range("C1"):VALUE = "GRUPO".
excelappl:range("D1"):VALUE = "DESCRI€ÇO GRUPO".
excelappl:range("E1"):VALUE = "EMPRESAS".


ASSIGN i = 2.


FOR EACH  usuar_grp_usuar NO-LOCK
    WHERE usuar_grp_usuar.cod_grp_usuar >= tt-param.c-ini-gr
    AND   usuar_grp_usuar.cod_grp_usuar <= tt-param.c-fim-gr
    AND   usuar_grp_usuar.cod_usuario   >= tt-param.c-ini-usuario 
    AND   usuar_grp_usuar.cod_usuario   <= tt-param.c-fim-usuario
 BREAK BY usuar_grp_usuar.cod_usuario:

        RUN pi-acompanhar IN h_acomp_rp (INPUT " - Usuario: " + usuar_grp_usuar.cod_usuario).

        ASSIGN c-empresa = "".

        FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = usuar_grp_usuar.cod_usuario NO-LOCK NO-ERROR.

        IF tt-param.l-ativo = NO THEN
            IF usuar_mestre.dat_fim_valid >= TODAY AND 
               usuar_mestre.dat_inic_valid <= TODAY THEN NEXT.

        IF tt-param.l-inativo = NO THEN
            IF usuar_mestre.dat_fim_valid <  TODAY OR  
               usuar_mestre.dat_inic_valid >  TODAY THEN NEXT.

        excelappl:range("A" + STRING(i)):VALUE = usuar_grp_usuar.cod_usuario.

        IF AVAIL usuar_mestre THEN            
           excelappl:range("B" + STRING(i)):VALUE = usuar_mestre.nom_usuario.

        FIND FIRST grp_usuar WHERE grp_usuar.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar NO-LOCK NO-ERROR.
            
        excelappl:range("C" + STRING(i)):VALUE = usuar_grp_usuar.cod_grp_usuar.

        IF AVAIL grp_usuar THEN                                                      
                 excelappl:range("D" + STRING(i)):VALUE = trim(grp_usuar.des_grp_usuar).

        IF NOT CAN-FIND(FIRST segur_empres_usuar
                  WHERE segur_empres_usuar.cod_empresa >= string(tt-param.i-ini-emp) 
                  AND   segur_empres_usuar.cod_empresa <= string(tt-param.i-fim-emp) 
                   AND  segur_empres_usuar.cod_usuario   = usuar_mestre.cod_usuario) THEN
        DO:
            FOR EACH empresa NO-LOCK WHERE empresa.ep-codigo >= tt-param.i-ini-emp 
                                     AND   empresa.ep-codigo <= tt-param.i-fim-emp:
                 ASSIGN c-empresa = c-empresa + string(empresa.ep-codigo) + " - " + TRIM(empresa.nome) + ", ". 
            END.
        END.
        ELSE
        DO:
            FOR EACH segur_empres_usuar NO-LOCK WHERE 
                     segur_empres_usuar.cod_empresa >= string(tt-param.i-ini-emp)
               AND   segur_empres_usuar.cod_empresa <= string(tt-param.i-fim-emp)
                AND  segur_empres_usuar.cod_usuario   = usuar_mestre.cod_usuario:
    
                FIND FIRST   empresa WHERE empresa.ep-codigo = segur_empres_usuar.cod_empresa NO-LOCK NO-ERROR.
                IF NOT AVAIL empresa THEN NEXT.
    
                ASSIGN c-empresa = c-empresa + string(empresa.ep-codigo) + " - " + TRIM(empresa.nome) + ", ".
    
            END.


        END.

        excelappl:range("E" + STRING(i)):VALUE = substring(C-Empresa,1,LENGTH(c-empresa) - 1).
        

        ASSIGN i = i + 1.                    


END.


/* FOR EACH segur_empres_usuar NO-LOCK                                                                                */
/*      WHERE segur_empres_usuar.cod_empresa >= string(tt-param.i-ini-emp)                                            */
/*      AND   segur_empres_usuar.cod_empresa <= string(tt-param.i-fim-emp):                                           */
/*                                                                                                                    */
/*         FOR EACH  usuar_grp_usuar NO-LOCK                                                                          */
/*             WHERE usuar_grp_usuar.cod_grp_usuar >= tt-param.c-ini-gr                                               */
/*             AND   usuar_grp_usuar.cod_grp_usuar <= tt-param.c-fim-gr:                                              */
/*                                                                                                                    */
/*                                                                                                                    */
/*             FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = usuar_grp_usuar.cod_usuario NO-LOCK NO-ERROR. */
/*                                                                                                                    */
/*             excelappl:range("A" + STRING(i)):VALUE = usuar_grp_usuar.cod_usuario.                                  */
/*                                                                                                                    */
/*             IF AVAIL usuar_mestre THEN                                                                             */
/*                 excelappl:range("B" + STRING(i)):VALUE = usuar_mestre.nom_usuario.                                 */
/*                                                                                                                    */
/*                                                                                                                    */
/*             FIND FIRST grp_usuar WHERE grp_usuar.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar NO-LOCK NO-ERROR.   */
/*                                                                                                                    */
/*             excelappl:range("C" + STRING(i)):VALUE = usuar_grp_usuar.cod_grp_usuar.                                */
/*                                                                                                                    */
/*             IF AVAIL grp_usuar THEN                                                                                */
/*                     excelappl:range("D" + STRING(i)):VALUE = trim(grp_usuar.des_grp_usuar).                        */
/*                                                                                                                    */
/*             FIND FIRST empresa WHERE empresa.ep-codigo = int(segur_empres_usuar.cod_empresa) NO-LOCK NO-ERROR.     */
/*                                                                                                                    */
/*             excelappl:range("E" + STRING(i)):VALUE = segur_empres_usuar.cod_empresa.                               */
/*             excelappl:range("F" + STRING(i)):VALUE = trim(empresa.nome).                                           */
/*                                                                                                                    */
/*             ASSIGN i = i + 1.                                                                                      */
/*                                                                                                                    */
/*         END.                                                                                                       */
/*                                                                                                                    */
/* END.                                                                                                               */

RUN pi-finaliza-excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-finaliza-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-finaliza-excel Procedure 
PROCEDURE pi-finaliza-excel :
/*------------------------------------------------------------------------------
  Purpose: Fechar a handle do excel!
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ExcelAppl:range("A2"):SELECT.
excelappl:activeWindow:FreezePanes = True.

Excelappl:activeWindow:Zoom = 80.
excelappl:range("A1:F1"):Font:Bold = TRUE.
excelappl:range("A1:F1"):AutoFilter(,,,).
excelappl:cells:SELECT.
excelappl:cells:EntireColumn:AutoFit.
ExcelAppl:range("A1"):SELECT.

excelappl:Workbooks:Item(1):SaveAs(c-dir,,,,,,).
excelappl:VISIBLE = TRUE.
RELEASE OBJECT excelappl NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-imprime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime Procedure 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST tt-param NO-LOCK NO-ERROR.

RUN pi-inicia-excel.

IF tt-param.classifica = 1 THEN RUN pi-by-empresa.
ELSE RUN pi-by-usuario.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-inicia-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inicia-excel Procedure 
PROCEDURE pi-inicia-excel :
/*------------------------------------------------------------------------------
  Purpose:     inicializar a planilha excel a ser trabalhada, com a template correta
  Parameters:  caminho da template a ser utilizada
  Notes:       
------------------------------------------------------------------------------*/

/* INICIALIZACAO DA PLANILHA EXCEL */

    CREATE "Excel.Application" excelappl.    
    excelappl:VISIBLE = FALSE.
    excelappl:APPLICATION:DisplayAlerts = FALSE.                                                                  
    excelappl:Workbooks:ADD().                                           
    excelappl:worksheets:ITEM(1):SELECT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


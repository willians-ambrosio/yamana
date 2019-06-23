&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* --------------------------------------------------------------------------------------- 
                                                                                        
   Sistema................: TOTVS                                                       
   Modulo.................: Manuten‡Æo Industrial                                                 
                                                                                                     
   Programa...............: Ficha de Manuten‡Æo                                                 
   Sub Programa...........:                                                                                             
                                                                                            
   Descricao..............: 
   programa para consultar: 
                             
   Entidade Desenvolvedora: DSC PRAXIS
   
   tabelas usadas.........: 
   importante.............:  
                                                                                           
   Historico Programa -------------------------------------------------------------------+ 
   | Data       | Autor               | Descricao                                        | 
   +----------- +---------------------+--------------------------------------------------+ 
   | 01/07/2015 | Marcos A.Souza      | Desenvolvimento do Programa                      | 
   +------------+---------------------+--------------------------------------------------+ */
{include/i-prgvrs.i esmi0304rp 2.08.10.002}

DEF VAR h-procextimpr                               AS HANDLE   NO-UNDO. 
DEF VAR i-num_lin_pag                               AS INT      NO-UNDO.    
DEF VAR c_process-impress                           AS CHAR     NO-UNDO.   
DEF VAR c-cod_pag_carac_conver                      AS CHAR     NO-UNDO.
DEF VAR v_num_count                                 AS INT      NO-UNDO.
define var c-arq-control                            as CHAR     NO-UNDO.

{include/i_fnctrad.i}
{cdp/cdcfgmnt.i}
{cdp/cd9911.i} /* Verifica Unidade Negocio MMI */
{utp/ut-glob.i}


define temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field i-ordem-ini      like ord-manut.nr-ord-produ
    field i-ordem-fim      like ord-manut.nr-ord-produ
    field d-dt-man-ini     as date format 99/99/9999
    field d-dt-man-fim     as date format 99/99/9999
    field c-custo-ini      as char format "x(8)"   
    field c-custo-fim      as char format "x(8)"
    field c-equipto-ini    as char format "x(16)"
    field c-equipto-fim    as char format "x(16)"
    field c-equipe-ini     as char format "x(08)" 
    field c-equipe-fim     as char format "x(08)"
    field i-tipo-manut-ini like manut.cd-tipo
    field i-tipo-manut-fim like manut.cd-tipo
    field c-planejado-ini  as char format "x(12)"
    field c-planejado-fim  as char format "x(12)"        
    field c-plano-parada-ini as char format "x(8)"
    field c-plano-parada-fim as char format "x(8)"
    FIELD imprimir         AS INTEGER
    field l-item           as log
    field l-ferr           as log
    field l-epi            as log
    field l-espec          as log
 &if defined (bf_mnt_ems204) &then
    field l-turno          as log
    field l-ficha          as log
 &endif   
    field l-imp            as log
    field l-met            as log
    field l-tarefa         as log
    field l-nar-ord        as log
    field l-nar-tar        as log
    field rs-ordem-plano   as integer
    field c-estabel-ini    as char format "x(03)"
    field c-estabel-fim    as char format "x(03)"
    FIELD l-equipto        AS LOG
    FIELD c-cod-unid-negoc-ini AS CHAR FORMAT "x(03)"
    FIELD c-cod-unid-negoc-fim AS CHAR FORMAT "x(03)".

def temp-table tt-raw-digita
    field raw-digita as raw.

DEF TEMP-TABLE tt-narrativa
  FIELD ordem AS INT
  FIELD texto AS CHAR
  INDEX idx01 AS UNIQUE PRIMARY ordem.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.


DEFINE VARIABLE h_acomp           AS HANDLE      NO-UNDO. 

DEFINE VARIABLE c-modelo          AS CHAR FORMAT "X(256)"              NO-UNDO.
DEFINE VARIABLE excelappl         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE workbooks         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE worksheets        AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE i-lin             AS INTEGER                           NO-UNDO.
DEFINE VARIABLE i-lin1            AS INTEGER                           NO-UNDO.
DEFINE VARIABLE i-col             AS INTEGER                           NO-UNDO.

DEFINE VARIABLE c-seguranca AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-tam-linha AS INTEGER     NO-UNDO.
DEFINE VARIABLE i           AS INTEGER     NO-UNDO.
DEFINE VARIABLE j           AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-tam       AS LOGICAL     NO-UNDO.

define variable xlEdgeLeft         as decimal no-undo initial 7. 
define variable xlEdgeRight        as decimal no-undo initial 10. 
define variable xlEdgeTop          as decimal no-undo initial 8. 
define variable xlEdgeBottom       as decimal no-undo initial 9. 
define variable xlInsideHorizontal as decimal no-undo initial 12. 
define variable xlInsideVertical   as decimal no-undo initial 11. 
define variable xlDiagonalDown     as decimal no-undo initial 5. 
define variable xlDiagonalUp       as decimal no-undo initial 6. 

/* DEF VAR c-msg-exp     LIKE msg-ord-man.msg-exp NO-UNDO.  */
DEF VAR i-avanca-char AS INT                   NO-UNDO.
DEF VAR i-ultimo-pos  AS INT                   NO-UNDO.
DEF VAR c-narrativa   AS CHAR                  NO-UNDO.
DEF VAR i-contador    AS INT                   NO-UNDO.
/* --- XlLineStyle --- */ 
define variable xlContinuous       as decimal no-undo initial 1. 

/* --- XlWidth --- */ 
define variable xlThin             as decimal no-undo initial 2. 
define variable xlNone             as decimal no-undo initial -4142. 

/* --- color index --- */ 
define variable xlAutomatic        as decimal no-undo initial -4105.

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
         HEIGHT             = 13.92
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

RUN pi-inicializar IN h_acomp (INPUT "Relatorio ").

RUN pi-monta.


RUN pi-finalizar IN h_acomp.
/* {include\i-rpclo.i}  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-excel-abrir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-abrir Procedure 
PROCEDURE pi-excel-abrir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN c-modelo = search ('modelos\esmi0304.xlt').
CREATE "excel.application" excelappl.
excelappl:VISIBLE = TRUE.
excelappl:workbooks:ADD(c-modelo).
excelappl:worksheets:ITEM(1):SELECT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-excel-fechar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-fechar Procedure 
PROCEDURE pi-excel-fechar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

excelAppl:APPLICATION:DisplayAlerts = FALSE.

IF tt-param.imprimir = 2 THEN
DO:
    excelAppl:ActiveSheet:PrintOut.

    excelappl:VISIBLE = FALSE.
END.
ELSE
    excelappl:VISIBLE = TRUE.
    
excelappl:range("R2"):SELECT.
/* excelappl:Cells:SELECT.                                          */
/* excelAppl:Workbooks:ITEM(1):SaveAs(c-arq-excel,,,,,,) NO-ERROR.  */
/* excelAppl:QUIT().  */
RELEASE OBJECT excelappl   NO-ERROR.
RELEASE OBJECT workbooks    NO-ERROR.
RELEASE OBJECT worksheets   NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-excel-imprimir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-imprimir Procedure 
PROCEDURE pi-excel-imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       lin,col
------------------------------------------------------------------------------*/

/* MESSAGE "esmi0304rp.p"                 */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */



ASSIGN excelappl:cells(2,18)    = ord-manut.nr-ord-produ     /*Nr. Ordem*/
       excelappl:cells(2,11)    = ord-manut.dt-prev-manut.   /*data programada*/
       excelappl:cells(16,22)   = DATE (SUBSTRING (Ord-Manut.char-1,1,10)). /*emissao da Ordem*/

ASSIGN excelappl:cells(17,22)   = ord-manut.tempo-para.

FOR EACH movto-tag NO-LOCK
    WHERE movto-tag.cd-equipto = ord-manut.cd-equipto.
    ASSIGN excelappl:cells(43,21) =  movto-tag.cd-tag
           excelappl:cells(43,12) =  movto-tag.cd-tag-orig.
END.

FOR EACH centro-custo NO-LOCK        
    WHERE SUBSTRING (centro-custo.cc-codigo,3,8) = ord-manut.sc-desp.
    ASSIGN excelappl:cells(19,7) = centro-custo.cc-codigo + " " +  centro-custo.descricao.
END.

FOR EACH tipo-manut NO-LOCK
    /* WHERE  tipo-manut.tp-manut = ord-manut.tp-manut. */
    WHERE tipo-manut.cd-tipo = ord-manut.cd-tipo:
    CASE tipo-manut.tipo:
        WHEN 1 THEN
            ASSIGN excelappl:cells(18,7) = "Preventiva".
        WHEN 2 THEN
            ASSIGN excelappl:cells(18,7) = "Corretiva".
        WHEN 3 THEN
            ASSIGN excelappl:cells(18,7) = "Preditiva".
        OTHERWISE
            ASSIGN excelappl:cells(18,7) = "Outros".
    END CASE.
END.

ASSIGN c-seguranca = "EPI: ".
       
FOR EACH  ord-epi NO-LOCK
    WHERE ord-epi.nr-ord-produ = ord-manut.nr-ord-produ. 

/*     ASSIGN c-seguranca = c-seguranca + ord-epi.cd-epi + ";".  */
    FOR EACH epi NO-LOCK
        WHERE epi.cd-epi = ord-epi.cd-epi.
        ASSIGN c-seguranca = c-seguranca + epi.descricao + ",".
     END.
END.

IF tt-param.l-epi THEN
    ASSIGN excelappl:cells(12,1) = c-seguranca.

ASSIGN c-seguranca = "Tipo Ferr: ".

FOR EACH ord-ferr NO-LOCK
    WHERE ord-ferr.nr-ord-produ = ord-manut.nr-ord-produ. 
    
/*     ASSIGN c-seguranca = c-seguranca  + ord-ferr.cd-tp-ferr + ";".  */

    FOR EACH tp-ferr NO-LOCK
        WHERE tp-ferr.cd-tp-ferr  = ord-ferr.cd-tp-ferr.

        ASSIGN c-seguranca = c-seguranca  + tp-ferr.descricao.
    END.
END.

IF tt-param.l-epi THEN
    ASSIGN excelappl:cells(13,1) = c-seguranca.

ASSIGN c-seguranca = "Ficha Metodo: ".

FOR EACH ord-fich-met NO-LOCK
    WHERE ord-fich-met.nr-ord-produ = ord-manut.nr-ord-produ. 

/*     ASSIGN c-seguranca = c-seguranca  + STRING (ord-fich-met.fi-codigo) + ";".  */
    FOR EACH mnt-ficha-metodo NO-LOCK
        WHERE mnt-ficha-metodo.fi-codigo = ord-fich-met.fi-codigo.

        ASSIGN c-seguranca = c-seguranca  +  mnt-ficha-metodo.descricao.
    END.
END.

IF tt-param.l-epi THEN
    ASSIGN excelappl:cells(14,1) = c-seguranca.

FOR EACH planejad NO-LOCK
    WHERE planejad.cd-planejado = ord-manut.cd-planejado.
    
    ASSIGN excelappl:cells(15,7) = planejad.nome. 
END.

FOR EACH mnt-planejador NO-LOCK
    WHERE mnt-planejador.cd-planejado = ord-manut.cd-planejado.
         
    ASSIGN excelappl:cells(16,7) = ord-manut.cd-planejado + " " + mnt-planejador.nome .
END.

FOR EACH equipe NO-LOCK
    WHERE equipe.cd-equipe = ord-manut.cd-equip-res.

    ASSIGN excelappl:cells(17,7) = ord-manut.cd-equip-res + " " + equipe.desc-equipe.

END.

ASSIGN excelappl:cells(22,6) = ord-manut.des-man-corr
       excelappl:cells(23,5) = "".

/* FIND FIRST equipto NO-LOCK                                                    */
/*     WHERE equipto.cd-tag = ord-manut.cd-tag                                   */
/*                                                                               */
/*     NO-ERROR.                                                                 */
/* IF AVAIL equipto THEN                                                         */
/*     ASSIGN excelappl:cells(20,7) = equipto.cd-tag + " " + equipto.descricao.  */
/* ELSE                                                                          */
/*     ASSIGN excelappl:cells(20,7) = ord-manut.cd-tag + "Erro CD0910 " .        */

FIND FIRST equipto NO-LOCK
    WHERE equipto.cd-equipto = ord-manut.cd-equipto
    NO-ERROR.
IF AVAIL equipto THEN
DO:
    ASSIGN excelappl:cells(7,1)  = equipto.cd-equipto + ":" + equipto.descricao.

    FOR EACH tag NO-LOCK
        WHERE tag.cd-tag = equipto.cd-tag.

        ASSIGN excelappl:cells(20,7) = equipto.cd-tag + " " + tag.descricao.
    END.
           
END.
            
/* FOR EACH ord-taref NO-LOCK                                 */
/*     WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ. */
/*                                                            */
/*     ASSIGN excelappl:cells(25,1) =  ord-taref.descricao.   */
/* END.                                                       */
/*                                                            */

/* IF tt-param.l-nar-ord THEN                                                                                            */
/*     FOR EACH  msg-ord-man NO-LOCK                                                                                     */
/*         WHERE  msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ.                                                     */
/*                                                                                                                       */
/*         ASSIGN l-tam = NO.                                                                                            */
/*         ASSIGN                                                                                                        */
/*           c-msg-exp = msg-ord-man.msg-exp                                                                             */
/*           c-msg-exp = REPLACE(c-msg-exp,CHR(13),"#")                                                                  */
/*           c-msg-exp = REPLACE(c-msg-exp,CHR(10),"@").                                                                 */
/*                                                                                                                       */
/*         MESSAGE "c-msg-exp: " c-msg-exp                                                                               */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                        */
/*         IF LENGTH (msg-ord-man.msg-exp) > 0 THEN                                                                      */
/*         DO:                                                                                                           */
/*             ASSIGN i-tam-linha = ROUND (LENGTH (msg-ord-man.msg-exp) / 83,1) + 0.49999999.                            */
/*             ASSIGN i-tam-linha = 50.                                                                                  */
/*             MESSAGE "i-tam-linha: " i-tam-linha                                                                       */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                    */
/*             ASSIGN i-lin = 26                                                                                         */
/*                    l-tam = YES.                                                                                       */
/*                                                                                                                       */
/*             IF i-tam-linha > 2 THEN                                                                                   */
/*             DO:                                                                                                       */
/*                 ASSIGN i-tam-linha = i-tam-linha - 2.                                                                 */
/*                                                                                                                       */
/*                 DO i = 1 TO i-tam-linha.                                                                              */
/*                    excelappl:range(STRING(i-lin, "999999") + ":" + STRING(i-lin, "999999")):INSERT().                 */
/*                    ASSIGN i-lin = i-lin + 1.                                                                          */
/*                 END.                                                                                                  */
/*                                                                                                                       */
/*                 excelappl:range('A' + STRING (25) + ':' + 'AA' + STRING (i-lin - 1)):merge().   /* mesclar */         */
/*                 excelappl:range('A' + STRING (25)):HorizontalAlignment = 2.                     /* alinha esquerda */ */
/*             END.                                                                                                      */
/*             ELSE                                                                                                      */
/*             DO:                                                                                                       */
/*                 excelappl:range('A25' + ':' + 'AA26'):merge().                /* mesclar */                           */
/*                 excelappl:range('A' + STRING (25)):HorizontalAlignment = 2.   /* alinha esquerda */                   */
/*             END.                                                                                                      */
/*                                                                                                                       */
/*         END.                                                                                                          */
/*         ASSIGN excelappl:cells(25,1) =  msg-ord-man.msg-exp.                                                          */
/*                excelappl:range('A' + STRING (25)):VerticalAlignment = 1.                                              */
/*     END.                                                                                                              */


IF tt-param.l-nar-ord THEN  
  DO:
    FOR EACH tt-narrativa: DELETE tt-narrativa. END.
    FOR FIRST msg-ord-man
      NO-LOCK 
      WHERE msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ:
        ASSIGN
          c-narrativa = msg-ord-man.msg-exp
          c-narrativa = REPLACE(c-narrativa,"#"," ")
          c-narrativa = REPLACE(c-narrativa,CHR(10),"#").
        /*
        MESSAGE STRING(LENGTH(msg-ord-man.msg-exp))
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */    
        ASSIGN i-ultimo-pos = 1 i-contador = 0.
        IF LENGTH ( c-narrativa) > 0 THEN
          DO:
            DO i-avanca-char = 1 TO LENGTH ( c-narrativa):
              IF SUBSTRING(c-narrativa,i-avanca-char,1) = "#" THEN
                DO:
                  /*
                  MESSAGE
                    SUBSTRING( c-narrativa,i-ultimo-pos,i-avanca-char - i-ultimo-pos) SKIP
                    " inicio: " i-ultimo-pos     SKIP
                    " fim:    " i-avanca-char - i-ultimo-pos    SKIP
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  */  
                  CREATE tt-narrativa.
                  ASSIGN 
                    tt-narrativa.ordem = i-avanca-char
                    tt-narrativa.texto = SUBSTRING( c-narrativa,i-ultimo-pos,i-avanca-char - i-ultimo-pos).
                  ASSIGN i-ultimo-pos = i-avanca-char + 1.
                END.
              

            END.
          END.
      END.
  END.
/*
OUTPUT TO c:\temp\testem.txt.
FOR EACH tt-narrativa:
  DISP 
    tt-narrativa.ordem
    tt-narrativa.texto FORMAT "X(80)"
    WITH WIDTH 120.
END.
OUTPUT CLOSE.
*/
ASSIGN i-lin = 25.
FOR EACH tt-narrativa:
   ASSIGN i-lin = i-lin + 1 l-tam = YES. 
   excelappl:range(STRING(i-lin, "999999") + ":" + STRING(i-lin, "999999")):INSERT().  
   excelappl:range('A' + STRING (i-lin) + ':' + 'AA' + STRING (i-lin)):merge().   /* mesclar */
   excelappl:cells(i-lin,1) = tt-narrativa.texto.
END.


IF l-tam = NO THEN
    ASSIGN i-lin = 26.


/* excelappl:Rows(STRING(i-lin) + ":" + STRING(i-lin)):SELECT. */
/* excelappl:SELECTION:INSERT.                                 */

ASSIGN i-lin = i-lin + 3.

FOR EACH ord-taref NO-LOCK
    WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ.

    ASSIGN i-lin = i-lin + 1.
    
    excelappl:Rows(STRING(i-lin + 1) + ":" + STRING(i-lin + 1)):SELECT.
    excelappl:SELECTION:INSERT.

    excelappl:Range("A" + STRING (i-lin) + ":" + "K" + string(i-lin)):Borders(7):LineStyle = 1.
    excelappl:Range("A" + STRING (i-lin) + ":" + "K" + string(i-lin)):Borders(8):LineStyle = 1.
    excelappl:Range("A" + STRING (i-lin) + ":" + "K" + string(i-lin)):Borders(9):LineStyle = 1.
    excelappl:Range("A" + STRING (i-lin) + ":" + "K" + string(i-lin)):Borders(10):LineStyle = 1.

    excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(7):LineStyle = 1.
    excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(8):LineStyle = 1.
    excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(9):LineStyle = 1.
    excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(10):LineStyle = 1.

    excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(7):LineStyle = 1.
    excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(8):LineStyle = 1.
    excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(9):LineStyle = 1.
    excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(10):LineStyle = 1.

    ASSIGN excelappl:cells(i-lin,1) = STRING (ord-taref.cd-tarefa) + " " + ord-taref.descricao.

    excelappl:range('A' + STRING (i-lin) + ':' + 'K' + STRING (i-lin)):merge().  
    excelappl:range('A' + STRING (i-lin)):HorizontalAlignment = 2.   


    FOR EACH reservas NO-LOCK
        WHERE reservas.nr-ord-produ = ord-manut.nr-ord-produ
        AND   reservas.op-codigo    = ord-taref.cd-tarefa.

        FOR EACH ITEM NO-LOCK
            WHERE ITEM.it-codigo = reservas.it-codigo.
            ASSIGN excelappl:cells(i-lin,12) = reservas.it-codigo + " " + ITEM.desc-item
                   excelappl:cells(i-lin,26) = reservas.quant-orig.

            excelappl:range('L' + STRING (i-lin) + ':' + 'X' + STRING (i-lin)):merge().  
            excelappl:range('L' + STRING (i-lin)):HorizontalAlignment = 2. 
            excelappl:range('Y' + STRING (i-lin) + ':' + 'Z' + STRING (i-lin)):merge().  
            excelappl:range('Y' + STRING (i-lin)):HorizontalAlignment = 1.   


            excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(7):LineStyle = 1.
            excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(8):LineStyle = 1.
            excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(9):LineStyle = 1.
            excelappl:Range("L" + STRING (i-lin) + ":" + "X" + string(i-lin)):Borders(10):LineStyle = 1.

            excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(7):LineStyle = 1.
            excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(8):LineStyle = 1.
            excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(9):LineStyle = 1.
            excelappl:Range("Y" + STRING (i-lin) + ":" + "AA" + string(i-lin)):Borders(10):LineStyle = 1.

            ASSIGN i-lin = i-lin + 1.
        
            excelappl:Rows(STRING(i-lin) + ":" + STRING(i-lin)):SELECT.
            excelappl:SELECTION:INSERT.
        END.
    END.
    excelappl:Range('A':u + STRING (i-lin)):Borders(xlDiagonalDown):LineStyle = xlNone. 
END.

IF l-tam THEN
DO:
    DO i = 1 TO 3.
        excelappl:Rows(STRING(i-lin) + ":" + STRING(i-lin)):SELECT.
        excelappl:SELECTION:DELETE.
    END.
END.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-monta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta Procedure 
PROCEDURE pi-monta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* MESSAGE                                                        */
/*     tt-param.i-tipo-manut-ini  tt-param.i-tipo-manut-ini  SKIP */
/*     tt-param.i-tipo-manut-fim  tt-param.i-tipo-manut-fim  SKIP */

/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
blk_ord_manut:
FOR EACH ord-manut 
    WHERE  ord-manut.nr-ord-produ     >= tt-param.i-ordem-ini 
    AND    ord-manut.nr-ord-produ     <= tt-param.i-ordem-fim
    AND    ord-manut.dt-prev-manut    >= tt-param.d-dt-man-ini
    AND    ord-manut.dt-prev-manut    <= tt-param.d-dt-man-fim
/*     AND    ord-manut.tp-manut         >= tt-param.i-tipo-manut-ini */
/*     AND    ord-manut.tp-manut         <= tt-param.i-tipo-manut-fim */

    AND    ord-manut.cd-tipo           >= tt-param.i-tipo-manut-ini 
    AND    ord-manut.cd-tipo           <= tt-param.i-tipo-manut-fim 

    AND    ord-manut.cd-parada        >= tt-param.c-plano-parada-ini
    AND    ord-manut.cd-parada        <= tt-param.c-plano-parada-fim
    AND    ord-manut.cd-equip-res     >= tt-param.c-equipe-ini 
    AND    ord-manut.cd-equip-res     <= tt-param.c-equipe-fim.
    
    IF ord-manut.estado = 7 THEN NEXT blk_ord_manut. /* finalizada */
    IF ord-manut.estado = 8 THEN NEXT blk_ord_manut. /* terminada */
        
    RUN pi-acompanhar IN h_acomp (INPUT "Nro. Ordem.: " + STRING(ord-manut.nr-ord-produ )).

    RUN pi-excel-abrir.
    RUN pi-excel-imprimir.
    RUN pi-excel-fechar.

END.
        
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


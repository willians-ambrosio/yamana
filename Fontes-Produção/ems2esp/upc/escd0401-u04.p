/******************************************************************************
**  Programa: upc/escd0401-u04.p
**  Data....: Agosto de 2016
**  Autor...: Sergio Luiz Neto da Silveira - DSC PRAXIS
**  Objetivo: UPC da tela CD0401
******************************************************************************/
 
{include/i-prgvrs.i escd0401-u04 2.12.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}
{utp/ut-glob.i}

/*-----> Define de Parametros <---------------------------------------*/
def input parameter p-ind-event              as char          no-undo.
def input parameter p-ind-object             as char          no-undo.
def input parameter p-wgh-object             as handle        no-undo.
def input parameter p-wgh-frame              as widget-handle no-undo.
def input parameter p-cod-table              as char          no-undo.
def input parameter p-row-table              as rowid         no-undo.

/*-----> Define de Variaveis <----------------------------------------*/

def new global shared var wh-escd0401-rs-idi-sit-fornec  as widget-handle   no-undo.
def new global shared var wh-escd0401-dt-dat-vigenc-ini  as widget-handle   no-undo.
def new global shared var wh-escd0401-dt-dat-vigenc-fim  as widget-handle   no-undo.

DEFINE NEW GLOBAL SHARED VARIABLE l-autorizado AS LOGICAL NO-UNDO.

DEFINE VARIABLE i-cont AS INTEGER NO-UNDO.

DEFINE VARIABLE c-handle-obj        as character        no-undo.

/*-----> Main Block <-------------------------------------------------*/
IF p-ind-event = "INITIALIZE" THEN DO:

   IF NOT VALID-HANDLE(wh-escd0401-rs-idi-sit-fornec) THEN
      ASSIGN c-handle-obj                  = fc-handle-obj("rs-idi-sit-fornec", p-wgh-frame)
             wh-escd0401-rs-idi-sit-fornec = widget-handle(entry(1, c-handle-obj)) NO-ERROR.
   
   IF NOT VALID-HANDLE(wh-escd0401-dt-dat-vigenc-ini) THEN
      ASSIGN c-handle-obj                  = fc-handle-obj("dt-dat-vigenc-ini", p-wgh-frame)
             wh-escd0401-dt-dat-vigenc-ini = widget-handle(entry(1, c-handle-obj)) NO-ERROR.
   
   IF NOT VALID-HANDLE(wh-escd0401-dt-dat-vigenc-fim) THEN DO:
      ASSIGN c-handle-obj                  = fc-handle-obj("dt-dat-vigenc-fim", p-wgh-frame)
             wh-escd0401-dt-dat-vigenc-fim = widget-handle(entry(1, c-handle-obj)) NO-ERROR.
   END.

   /* Verifica se o usuario est  no grupo cadastrado */
   ASSIGN l-autorizado = NO.

   FOR EACH es_parametro
            WHERE es_parametro.cod_prog_dtsul = "CD0401" AND
                  es_parametro.cod_referencia = "GRUPO-ALTERA-SITUACAO"   
            NO-LOCK:
      blk:
      DO i-cont = 1 TO NUM-ENTRIES(es_parametro.cod_parametro,","):
         IF CAN-FIND(FIRST usuar_grp_usuar
                     WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario AND
                           usuar_grp_usuar.cod_grp_usuar = ENTRY(i-cont,es_parametro.cod_parametro,",")
                     NO-LOCK) THEN DO:
            ASSIGN l-autorizado = YES.
            LEAVE blk.
         END.
      END.
   END.
END.

IF l-autorizado = NO THEN DO:
   IF VALID-HANDLE(wh-escd0401-rs-idi-sit-fornec) THEN
      ASSIGN wh-escd0401-rs-idi-sit-fornec:SENSITIVE = NO.
   
   IF  VALID-HANDLE(wh-escd0401-dt-dat-vigenc-ini) THEN
      ASSIGN wh-escd0401-dt-dat-vigenc-ini:SENSITIVE = NO.
   
   IF  VALID-HANDLE(wh-escd0401-dt-dat-vigenc-fim) THEN
      ASSIGN wh-escd0401-dt-dat-vigenc-fim:SENSITIVE = NO.
   
   RETURN "OK":U.
END.


IF p-ind-event  = "VALIDATE"                     and
   p-ind-object = "VIEWER"                       and
   p-wgh-object:FILE-NAME MATCHES "*cd0401-v04*" then do:

   IF l-autorizado = NO THEN DO:
      RUN utp/ut-msgs.p (INPUT "show",
                         INPUT 17006,
                         INPUT "Usu rio nÆo possui permissÆo para alterar a Situa‡Æo do Fornecedor").
   END.
END.


RETURN "OK":U.

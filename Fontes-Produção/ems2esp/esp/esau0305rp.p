/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESAU0305RP 0.12.00.000}  /*** 010012 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESAU0305RP MAU}
&ENDIF

/******************************************************************************
**
**   Programa: ESAU0305RP.P
**
**   Autor...: Datasul S/A.
**
**   Objetivo: Relat¢rio Resultado Atributos Monitorados
*******************************************************************************/
/* BEG - Defini‡Æo de Pr‚-Processadores */

{utp/utapi019.i}

{btb/btb912zb.i}

DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "x(200)"
    FIELD lSearch   AS LOGICAL.

DEFINE TEMP-TABLE tt-erros-zip NO-UNDO
    FIELD cod-erro  AS INTEGER FORMAT ">>>>9"
    FIELD desc-erro AS CHAR    FORMAT "x(70)".

DEFINE VARIABLE h-zip AS HANDLE NO-UNDO.
DEFINE VARIABLE c-arquivo-zip AS CHARACTER NO-UNDO.

DEFINE STREAM ST-LOG.

DEFINE VARIABLE c-arq-log AS CHARACTER NO-UNDO.

ASSIGN c-arq-log = "D:\TEMP\RPW\VERIFICA_ESAU0305_" + REPLACE(STRING(TODAY,"99/99/9999"),"/","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".TXT".

OUTPUT STREAM ST-LOG TO VALUE (c-arq-log).

run utp/ut-zip.p persistent set h-zip.

/*---- Vari vel para buscar empresa */
def new global shared var v_nom_razao_social    AS Character    no-undo.

DEFINE VARIABLE d-data AS DATE NO-UNDO.


/* BEG - Defini‡Æo das Temp-tables */
    define temp-table tt-param no-undo
        field destino          as integer
        field arquivo          as char format "x(35)"
        field usuario          as char format "x(12)"
        field data-exec        as date
        field hora-exec        as integer
        field classifica       as integer
        field desc-classifica  as char format "x(40)"
        field c-user-ini       like tabela_vrf_monitor.cod_usuario
        field c-user-fim       like tabela_vrf_monitor.cod_usuario
        field c-base-ini       like tabela_vrf_monitor.cod_base_dados
        field c-base-fim       like tabela_vrf_monitor.cod_base_dados
        field c-tab-ini        like tabela_vrf_monitor.cod_tabela
        field c-tab-fim        like tabela_vrf_monitor.cod_tabela
        field c-atr-ini        like atrib_vrf_monitor.cod_atributo
        field c-atr-fim        like atrib_vrf_monitor.cod_atributo
        field c-dat-ini        like tabela_vrf_monitor.dat_atualiz
        field c-dat-fim        like tabela_vrf_monitor.dat_atualiz
        field l-val-alt        as log
        field c-prog-ini       as char
        field c-prog-fim       as char
        field l-seq-prog       as log
        field l-create         as log
        field l-write          as log
        field l-delete         as log
        Field gera-excel       As LOG INITIAL NO
        Field arq-csv          As CHARACTER
        Field l-imp-param      As Log
        field envia-email      as LOG
        FIELD rs-tipo-execucao AS INTEGER.

DEF var c-des-val-ant       like atrib_vrf_monitor.des_val_ant format "x(60)" NO-UNDO.
DEF var c-des-val-new       like atrib_vrf_monitor.des_val_nov format "x(60)" NO-UNDO.
DEF VAR cSeparador          AS CHARACTER                                      NO-UNDO.

def temp-table tt-raw-digita NO-UNDO
    	field raw-digita	as raw.

{include/tt-edit.i}
def temp-table tt-editor-tmp no-undo
    field linha         as integer
    field conteudo-old  as character format 'x(60)':U
    field conteudo-new  as character format 'x(60)':U
    index editor-id is primary unique 
          linha.

def  buffer bf-base_dados for base_dados.
DEF STREAM s-audit.

/* END - Defini‡Æo das Temp-tables */

/* BEG - Defini‡Æo de Parametros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/* END - Defini‡Æo de Parametros */

/* BEG - Defini‡Æo de Pr‚-Processadores */
{include/i_dbvers.i} /*vers’o do banco de dados*/
{include/i_dbtype.i} /*tipo do banco de dados*/

&IF '{&mgadt_version}' >= '2.07' &THEN
    &SCOPED-DEFINE cdn_tip_banco bf-base_dados.cdn_tip_banco
&ELSE
    &SCOPED-DEFINE cdn_tip_banco bf-base_dados.int-1
&ENDIF
/* END - Defini‡Æo de Pr‚-Processadores */

create tt-param.
raw-transfer raw-param to tt-param.

/* BEG - Defini‡Æo de Vari veis Globais */
define new global shared var l-autconlist as logical no-undo.
/* END - Defini‡Æo de Vari veis Globais */

/* BEG - Defini‡Æo de Vari veis Locais */
DEF var h-acomp             as handle                                    no-undo.
DEF VAR i-cont              AS INTEGER                                   NO-UNDO.
DEF VAR l-oracle            AS LOGICAL                                   NO-UNDO.
DEF var c-prmgems           as char                                      no-undo.
Def Var iLinha              As Integer Initial 1                         NO-UNDO.
DEF var c-lab-base          like tabela_vrf_monitor.cod_base_dados       NO-UNDO.
DEF var c-lab-tab           like tabela_vrf_monitor.cod_tabela           NO-UNDO.
DEF var c-lab-atr           like atrib_vrf_monitor.cod_atributo          NO-UNDO.
DEF var c-lab-dat           as char format "X(4)"                        NO-UNDO.
DEF var c-lab-hra           as char format "X(4)"                        NO-UNDO.
DEF var c-lab-user          like tabela_vrf_monitor.cod_usuario          NO-UNDO.
DEF var c-lab-chave         like tabela_vrf_monitor.cod_chave            NO-UNDO.
DEF var c-lab-user-prog     like tabela_vrf_monitor.cod_usuario_progress NO-UNDO.
DEF var c-lab-term          like tabela_vrf_monitor.nom_terminal         NO-UNDO.
DEF var c-lab-prog          as char format "X(15)"                       NO-UNDO.
DEF var c-lab-evento        as char format "x(6)"                        NO-UNDO.
DEF var c-lab-vlr-ant       as CHAR                                      NO-UNDO.
DEF var c-lab-vlr-nov       as CHAR                                      NO-UNDO.
DEF var c-des-val-nov       like atrib_vrf_monitor.des_val_nov format "x(60)" NO-UNDO.
def var c-arquivo           as char                                      no-undo.
/* END - Defini‡Æo de Vari veis Locais */

DEFINE NEW GLOBAL SHARED VARIABLE c-ambiente AS CHARACTER NO-UNDO.

def var c-lit-selecao    as char format "x(07)" no-undo.
def var c-destino        as char format "x(10)" no-undo.

form
    skip(2)
    c-lit-selecao               no-label colon 55
    Skip
    tt-param.c-user-ini         Colon 30   space(15) "|< >|"
    tt-param.c-user-fim         no-label
    skip
    tt-param.c-base-ini         colon 30   space(20) "|< >|"
    tt-param.c-base-fim         no-label   
    skip
    tt-param.c-tab-ini          colon 30   space(03) "|< >|"
    tt-param.c-tab-fim          no-label   
    skip
    tt-param.c-atr-ini          colon 30   space(03) "|< >|"
    tt-param.c-atr-fim          no-label   
    skip
    tt-param.c-dat-ini          colon 30   space(25) "|< >|"
    tt-param.c-dat-fim          no-label   
    skip
    tt-param.c-prog-ini         colon 30   space(27) "|< >|"
    tt-param.c-prog-fim         no-label   
    skip
    tt-param.l-seq-prog         colon 30   
    tt-param.l-create           colon 30   
    tt-param.l-write            colon 30   
    tt-param.l-delete           colon 30   
    tt-param.gera-excel         COLON 30
    tt-param.arq-csv            COLON 30
    skip(1)
    c-destino                   colon 30 "-"
    tt-param.arquivo            no-label format "x(30)"
    tt-param.usuario            colon 30
    with side-label stream-io width 132 frame f-fim.


{utp/ut-liter.i Sele‡Æo * r}
assign c-lit-selecao = trim(return-value).

{utp/ut-liter.i Destino * r}
assign c-destino:label in frame f-fim = trim(return-value).



{utp/ut-liter.i Base_Dados * R}
ASSIGN c-lab-base = TRIM(RETURN-VALUE).
{utp/ut-liter.i Tabela * R}
ASSIGN c-lab-tab = TRIM(RETURN-VALUE).
{utp/ut-liter.i Atributo * R}
ASSIGN c-lab-atr = TRIM(RETURN-VALUE).
{utp/ut-liter.i Data_Atu * R}
ASSIGN c-lab-dat = TRIM(RETURN-VALUE).
{utp/ut-liter.i Hora_Atu * R}
ASSIGN c-lab-hra = TRIM(RETURN-VALUE).
{utp/ut-liter.i Chave * R}
ASSIGN c-lab-chave = TRIM(RETURN-VALUE).
{utp/ut-liter.i Usu rioEMS * R}
ASSIGN c-lab-user = TRIM(RETURN-VALUE).
{utp/ut-liter.i Usu rioPGS * R}
ASSIGN c-lab-user-prog = TRIM(RETURN-VALUE).
{utp/ut-liter.i Evento * R}
ASSIGN c-lab-evento = TRIM(RETURN-VALUE).
{utp/ut-liter.i "Terminal" * R}
ASSIGN c-lab-term = TRIM(RETURN-VALUE).
{utp/ut-liter.i Programa * R}
ASSIGN c-lab-prog = TRIM(RETURN-VALUE).
{utp/ut-liter.i "Valor Anterior:" * R}
ASSIGN c-lab-vlr-ant = TRIM(RETURN-VALUE).
{utp/ut-liter.i "Valor Novo:" * R}
ASSIGN c-lab-vlr-nov = TRIM(RETURN-VALUE).


/******************************************************************************************************************************************/
/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpc255.i &STREAM="str-rp"}

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input TRIM(RETURN-VALUE)).

/* --- Bloco principal do programa - */

/* Tratamneto Datasul 11, para tratar as diferen‡as no foundation */
&IF "{&mguni_version} " < "2.08" &THEN 
    {btb/btapi523.i h-param-global}

    If  Connected("dthrpyc") 
        Then  Run prghur/fpp/fpapi523.p Persistent Set h-param-global.
        Else  Do:
              If  (Connected("mgind") And 
                  connected("mgadm")) Or 
                  l-autconlist = Yes  Then Do:
                
                  /* --- Verifica se base ‚ Magnus ou EMS - */
                  Run  cdp/cdapi526.p (Output c-prmgems).
                  If  c-prmgems = 'EMS' 
                      Then  Run cdp/cdapi523.p Persistent Set h-param-global. /* EMS */
                      Else  Run cdp/cdapi525.p Persistent Set h-param-global. /* Magnus */
              End. /* if */
        End.
    
    
    If  Valid-handle(h-param-global) Then Do:
        If  Return-value = "OK" 
            Then  Assign c-empresa = grupo().
            Else  Assign c-empresa = "".
    
        Delete Procedure h-param-global.
    End.
&ELSE
    &IF "{&emsfnd_version} " >= "1.02" &THEN
        ASSIGN c-empresa = v_nom_razao_social.
    &ELSE
        find first param-global no-lock no-error.
        assign c-empresa  = (if avail param-global then param-global.grupo else "").
    &ENDIF  
&ENDIF

assign  c-programa 	= "ESAU0305RP"
	    c-sistema	= "Audit Trail".

{utp/ut-liter.i Relat¢rio_Resultado_Atributos_Monitorados * C}
assign c-titulo-relat = TRIM(RETURN-VALUE).

view stream str-rp frame f-cabec-255.
view stream str-rp frame f-rodape-255.

If i-num-ped-exec-rpw <> 0 then do:

    if month(today) = 1 then
        assign tt-param.c-dat-ini = date(12,01,year(today) - 1)
               tt-param.c-dat-fim = date(01,01,year(today)) - 1.
    else
        assign tt-param.c-dat-ini = date(month(today) - 1,01,year(today))
               tt-param.c-dat-fim = date(month(today),01,year(today)) - 1.

end.


/**Cabecalho Arquivo CSV Excel**/
IF  tt-param.gera-excel
THEN DO:

    If i-num-ped-exec-rpw <> 0
    Then Assign tt-param.arq-csv = c-dir-spool-servid-exec + "~/" + tt-param.arq-csv.

    OUTPUT STREAM s-audit TO VALUE(tt-param.arq-csv) Convert Target "ISO8859-1".
    PUT STREAM s-audit UNFORMATTED
        '"' + c-lab-base + '";"' + c-lab-tab + '";"' + c-lab-atr + '";"' + c-lab-dat + '";"' + c-lab-hra + '";"' + c-lab-chave + '";"' + 
        c-lab-user + '";"' + c-lab-evento + '";"' + c-lab-term + '";"' + c-lab-prog + '";"' + c-lab-vlr-ant + '";"' + c-lab-vlr-nov + '"'
        SKIP.
End.



IF tt-param.classifica = 1 THEN DO:
   PUT STREAM str-rp UNFORMATTED
       c-lab-user  SPACE(10)   c-lab-dat   SPACE(3)    c-lab-hra   SPACE(1)    c-lab-base      SPACE(06)   
       c-lab-tab   SPACE(25)   c-lab-atr   SPACE(23)   c-lab-chave SPACE(46)   c-lab-evento    SPACE(2)
       c-lab-term  SPACE(13)   c-lab-prog  SKIP.

   PUT STREAM str-rp UNFORMATTED
       "------------------- ---------- -------- --------------- ------------------------------ ------------------------------ "
       "-------------------------------------------------- ------  -------------------- ---------------"
       SKIP.

   DO d-data = tt-param.c-dat-ini TO tt-param.c-dat-fim:
      FOR  EACH tabela_vrf_monitor USE-INDEX tbrstdmn-04 
                WHERE tabela_vrf_monitor.dat_atualiz      = d-data              AND
                      tabela_vrf_monitor.cod_base_dados  >= tt-param.c-base-ini AND 
                      tabela_vrf_monitor.cod_base_dados  <= tt-param.c-base-fim AND 
                      tabela_vrf_monitor.cod_tabela      >= tt-param.c-tab-ini  AND 
                      tabela_vrf_monitor.cod_tabela      <= tt-param.c-tab-fim
                NO-LOCK,
                EACH atrib_vrf_monitor 
                     WHERE atrib_vrf_monitor.num_seq_evento  = tabela_vrf_monitor.num_seq_evento AND
                           atrib_vrf_monitor.cod_atributo   >= tt-param.c-atr-ini                AND 
                           atrib_vrf_monitor.cod_atributo   <= tt-param.c-atr-fim
                NO-LOCK:
           
         IF (tabela_vrf_monitor.cod_evento = 'C' AND tt-param.l-create = NO)
         OR (tabela_vrf_monitor.cod_evento = 'W' AND tt-param.l-write  = NO)
         OR (tabela_vrf_monitor.cod_evento = 'D' AND tt-param.l-delete = NO)
         THEN NEXT.
   
         IF tabela_vrf_monitor.cod_usuario     < tt-param.c-user-ini OR
            tabela_vrf_monitor.cod_usuario     > tt-param.c-user-fim THEN 
            NEXT.
       
         /* Oracle Nativo possui apenas um programa, entÆo deve verificar o primeiro item */
         FIND FIRST bf-base_dados
             where bf-base_dados.cod_base_dados = tabela_vrf_monitor.cod_base_dados
             no-lock no-error.
         if  avail bf-base_dados THEN DO:
             /* Se Oracle ou SQL Server nativo ent’o desativa o bot’o de check syntax */
             ASSIGN l-oracle = ({&cdn_tip_banco} = 3 OR {&cdn_tip_banco} = 5).
         
             IF l-oracle THEN DO:
                 IF NOT (tabela_vrf_monitor.des_prog_atualiz[1]    >= tt-param.c-prog-ini
                     AND tabela_vrf_monitor.des_prog_atualiz[1]    <= tt-param.c-prog-fim) THEN
                     NEXT.
             END.
             ELSE DO:
                 IF NOT (tabela_vrf_monitor.des_prog_atualiz[2]    >= tt-param.c-prog-ini
                     AND tabela_vrf_monitor.des_prog_atualiz[2]    <= tt-param.c-prog-fim) THEN
                     NEXT.
             END.
         END.

         
         RUN pi-acompanhar IN h-acomp (input String(iLinha,"9999999") + " : " + tabela_vrf_monitor.cod_tabela).
         
         ASSIGN iLinha = iLinha + 1.

         PUT STREAM str-rp
             tabela_vrf_monitor.cod_usuario          FORMAT "X(19)"
             SPACE(01)
             tabela_vrf_monitor.dat_atualiz          FORMAT "99/99/9999"
             SPACE(01)
             STRING(tabela_vrf_monitor.hra_atualiz,"99:99:99")
             SPACE(01)
             tabela_vrf_monitor.cod_base_dados       FORMAT "X(15)"
             SPACE(01)
             tabela_vrf_monitor.cod_tabela           FORMAT "X(30)"
             SPACE(01)
             atrib_vrf_monitor.cod_atributo          FORMAT "X(30)"
             SPACE(01)
             tabela_vrf_monitor.cod_chave            FORMAT "X(50)"
             SPACE(01)
             tabela_vrf_monitor.cod_evento 
             SPACE(07)
             tabela_vrf_monitor.nom_terminal         FORMAT "X(20)"
             SPACE(01)
             (IF ({&cdn_tip_banco} = 3 OR {&cdn_tip_banco} = 5) THEN
             tabela_vrf_monitor.des_prog_atualiz[1]
             ELSE
             tabela_vrf_monitor.des_prog_atualiz[2]) FORMAT "X(15)"
             SKIP.
   
         Run pi-imprime.
      END.  
   END.
END.

/* ASSIGN c-destino = "AHR-SOXReportDatasul@Yamana.com". */
/*                                                       */
/* ASSIGN c-destino = "sergio.silveira@dsc.com.br".      */

FOR EACH es_parametro
         WHERE es_parametro.cod_prog_dtsul = "ESAU0305" AND
               es_parametro.cod_referencia = "E-MAIL"   
         NO-LOCK:
   ASSIGN c-destino = es_parametro.cod_parametro.
END.

IF tt-param.gera-excel THEN 
   OUTPUT STREAM s-audit CLOSE.

run pi-finalizar in h-acomp.

If tt-param.l-imp-param = YES Then Do:
    Display STREAM str-rp
            c-lit-selecao          
            tt-param.c-user-ini    
            tt-param.c-user-fim    
            tt-param.c-base-ini    
            tt-param.c-base-ini    
            tt-param.c-tab-ini     
            tt-param.c-tab-fim     
            tt-param.c-atr-ini     
            tt-param.c-atr-fim     
            tt-param.c-dat-ini     
            tt-param.c-dat-fim     
            tt-param.c-prog-ini    
            tt-param.c-prog-fim    
            tt-param.l-seq-prog    
            tt-param.l-create      
            tt-param.l-write       
            tt-param.l-delete      
            tt-param.gera-excel
            tt-param.arq-csv
            c-destino              
            tt-param.arquivo       
            tt-param.usuario       
            With frame f-fim.

End.


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}

if tt-param.envia-email then do:

    if tt-param.gera-excel then
        assign c-arquivo = tt-param.arq-csv.
    else
        assign c-arquivo = tt-param.arquivo.

/*     IF i-num-ped-exec-rpw <> 0 Then                                          */
/*        Assign c-arquivo = c-dir-spool-servid-exec + "~/" + tt-param.arquivo. */
    
    IF i-num-ped-exec-rpw <> 0 Then
       ASSIGN c-arquivo-zip = "d:\temp\rpw\esau0305_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".zip".
    ELSE
       ASSIGN c-arquivo-zip = "d:\temp\esau0305_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".zip".

    RUN pi-zip-file (INPUT-OUTPUT c-arquivo,
                     INPUT-OUTPUT c-arquivo-zip).

    find first param_email no-lock no-error.

    create tt-envio2.
    assign tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = param_email.log_servid_exchange
           tt-envio2.servidor          = param_email.cod_servid_e_mail
           tt-envio2.porta             = param_email.num_porta
           tt-envio2.destino           = c-destino
           tt-envio2.assunto           = "Relat¢rio Resultado Monitoramento Audit - ESAU0305"
           tt-envio2.remetente         = "adm@yamana.com"
           tt-envio2.copia             = ""
           tt-envio2.mensagem          = ""
           tt-envio2.importancia       = 1
           tt-envio2.log-enviada       = no
           tt-envio2.log-lida          = no
           tt-envio2.acomp             = no
           tt-envio2.arq-anexo         = c-arquivo-zip.

    RUN utp/utapi019.p PERSISTENT SET h-utapi019.

    IF CONNECTED("hresp") THEN
       ASSIGN c-ambiente = "HCM-TOTVS 12".
    ELSE
       ASSIGN c-ambiente = "ERP-TOTVS 12".

    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 1
           tt-mensagem.mensagem     = "Envio automatizado de reportes Datasul - Controles Mensais SOX" + CHR(10) + CHR(13).

    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 2
           tt-mensagem.mensagem     = "Controle : GC AC KY13" + CHR(10) + CHR(13).
    
    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 3
           tt-mensagem.mensagem     = "Ambiente : " + c-ambiente + CHR(10) + CHR(13).

    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 4
           tt-mensagem.mensagem     = "Vocˆ est  recebendo este email pois foi executada a rotina ESAU0305 do produto Totvs 12".

    FOR EACH tt-envio2:
       RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                      INPUT  TABLE tt-mensagem,
                                      OUTPUT TABLE tt-erros).
  END.

  FOR EACH tt-erros :
     PUT STREAM ST-LOG "ERROS ENVIO EMAIL |"  TODAY "|"  TIME  "|" tt-erros.cod-erro "|" tt-erros.desc-erro SKIP.
  END.

  IF NOT CAN-FIND(FIRST tt-erros) THEN
     PUT STREAM ST-LOG "ENVIADO O ARQUIVO |"  c-arquivo-zip "| PARA O EMAIL |" c-destino SKIP.

  DELETE PROCEDURE h-utapi019.  

   OUTPUT STREAM ST-LOG CLOSE. 

end.

if tt-param.rs-tipo-execucao = 2 then
   run agendarExecucaoAutomatica.

RETURN "OK":U.



/**********************************************************************************************************************************
**********[ PROCEDURE INTERNAS] ***************************************************************************************************
**********************************************************************************************************************************/
PROCEDURE pi-print-editor:
    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(254) + chr(254).

    if c-editor = "" then do:
        create tt-editor.
        assign tt-editor.linha    = 1
               tt-editor.conteudo = "".
    end.

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(254)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
END PROCEDURE.


/**********************************************************************************************************************************
**********[ PROCEDURE INTERNAS] ***************************************************************************************************
**********************************************************************************************************************************/
Procedure Pi-Imprime:
        /*Quebra valor anterior em varias linhas com tamanho 60 */
    ASSIGN c-des-val-ant = "".
    DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_ant,chr(254)):
        ASSIGN c-des-val-ant = c-des-val-ant + ENTRY(i-cont,atrib_vrf_monitor.des_val_ant,chr(254)) + ",".
    END.

    ASSIGN c-des-val-ant = Replace(Replace(SUBSTRING(c-des-val-ant,1,LENGTH(c-des-val-ant) - 1),CHR(10),""),CHR(13),"").

    RUN pi-print-editor (c-des-val-ant, 87).
    FOR EACH tt-editor:
        CREATE tt-editor-tmp.
        ASSIGN tt-editor-tmp.linha          = tt-editor.linha
               tt-editor-tmp.conteudo-old   = tt-editor.conteudo.
    END.

    /*Quebra valor novo em varias linhas com tamanho 60 */
    ASSIGN c-des-val-new = "".
    DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_nov,chr(254)):
        ASSIGN c-des-val-new = c-des-val-new + ENTRY(i-cont,atrib_vrf_monitor.des_val_nov,chr(254)) + ",".
    END.
    ASSIGN c-des-val-new = Replace(Replace(SUBSTRING(c-des-val-new,1,LENGTH(c-des-val-new) - 1),CHR(10),""),CHR(13),"").

    RUN pi-print-editor (c-des-val-new, 80).
    FOR EACH tt-editor:
        FIND FIRST tt-editor-tmp EXCLUSIVE-LOCK 
             WHERE tt-editor-tmp.linha = tt-editor.linha NO-ERROR.
        IF  NOT AVAIL tt-editor-tmp
        THEN DO:
            CREATE tt-editor-tmp.
            ASSIGN tt-editor-tmp.linha = tt-editor.linha.
        END.
        ASSIGN tt-editor-tmp.conteudo-new = tt-editor.conteudo.
    END.
    Assign i-cont = 1.
    FOR EACH tt-editor-tmp:
        IF i-cont = 1
        THEN DO:
            PUT STREAM str-rp UNFORMATTED
                SPACE(20)
                c-lab-vlr-ant SPACE(01)
                tt-editor-tmp.conteudo-old  FORMAT "X(80)"
                SPACE(05)
                c-lab-vlr-nov SPACE(01)
                tt-editor-tmp.conteudo-new  FORMAT "X(80)"
                Skip.
            ASSIGN i-cont = 2.
        END.
        ELSE DO:
            PUT STREAM str-rp UNFORMATTED
                SPACE(36)
                tt-editor-tmp.conteudo-old  FORMAT "X(80)"
                Space(17)
                tt-editor-tmp.conteudo-new  FORMAT "X(80)"
                SKIP.
        End.
        DELETE tt-editor-tmp.
    END.

    PUT STREAM str-rp UNFORMATTED SKIP(1).

    IF  tt-param.gera-excel
    THEN DO:
        PUT STREAM s-audit UNFORMATTED
            '"' + tabela_vrf_monitor.cod_base_dados + '";' +
            '"' + tabela_vrf_monitor.cod_tabela + '";' +
            '"' + atrib_vrf_monitor.cod_atributo + '";' +
            '"' + string(tabela_vrf_monitor.dat_atualiz) + '";' +
            '"' + STRING(tabela_vrf_monitor.hra_atualiz,"99:99:99") + '";' +
            '"' + tabela_vrf_monitor.cod_chave + '";' +
            '"' + tabela_vrf_monitor.cod_usuario + '";' +
            '"' + tabela_vrf_monitor.cod_evento + '";' +
            '"' + tabela_vrf_monitor.nom_terminal + '";' +
            '"' + tabela_vrf_monitor.des_prog_atualiz[2] + '";' +
            '"' + Replace(Replace(atrib_vrf_monitor.des_val_ant,chr(10),""),chr(13),"") + '";' +
            '"' + Replace(Replace(atrib_vrf_monitor.des_val_nov,chr(10),""),chr(13),"") + '"'
            SKIP.
    END.
    EMPTY TEMP-TABLE tt-editor-tmp.
    EMPTY TEMP-TABLE tt-editor.
End Procedure.

procedure agendarExecucaoAutomatica private:
    /*******************************************************************************************************************
     Gera nova chamada RPW levando em consideracao a hora atual mais o intervalo em segundos gravado nos Parametros da 
     Engenharia
    *******************************************************************************************************************/
    DEFINE VARIABLE i-tempo-rpw AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-hra-rpw   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE d-dat-rpw   AS DATE        NO-UNDO.

    EMPTY TEMP-TABLE tt_param_segur.
    EMPTY TEMP-TABLE tt_ped_exec.
    EMPTY TEMP-TABLE tt_ped_exec_param.

    ASSIGN i-tempo-rpw          = TIME
           c-hra-rpw            = REPLACE (STRING (i-tempo-rpw, "HH:MM:SS"),":","")
           tt-param.data-exec   = TODAY.
    
    ASSIGN tt-param.data-exec = IF MONTH(TODAY) = 12 THEN ( DATE(01,01,YEAR(TODAY) + 1)) ELSE (DATE(MONTH(TODAY) + 1,01,YEAR(TODAY))).

    ASSIGN d-dat-rpw = tt-param.data-exec.

    FIND FIRST ped_exec NO-LOCK WHERE ped_exec.num_ped_exec = i-num-ped-exec-rpw NO-ERROR.
    IF NOT AVAIL ped_exec THEN RETURN.

    FIND FIRST ped_exec_param NO-LOCK WHERE ped_exec_param.num_ped_exec = ped_exec.num_ped_exec NO-ERROR.
    IF NOT AVAIL ped_exec_param THEN RETURN.

    CREATE tt_param_segur.
    ASSIGN tt_param_segur.tta_num_vers_integr_api      = 3
           tt_param_segur.tta_cod_aplicat_dtsul_corren = ped_exec_param.cod_aplicat_dtsul
           tt_param_segur.tta_cod_empres_usuar         = ped_exec_param.cod_empresa
           tt_param_segur.tta_cod_modul_dtsul_corren   = ped_exec_param.cod_modul_dtsul
           tt_param_segur.tta_cod_pais_empres_usuar    = ped_exec_param.cod_pais
           tt_param_segur.tta_cod_usuar_corren         = ped_exec.cod_usuario
           tt_param_segur.tta_cod_usuar_corren_criptog = ped_exec_param.cod_usuar_criptog.
    
    CREATE tt_ped_exec.
    ASSIGN tt_ped_exec.tta_num_seq                     = 1
           tt_ped_exec.tta_cod_usuario                 = ped_exec.cod_usuario
           tt_ped_exec.tta_cod_prog_dtsul              = "ESAU0305"
           tt_ped_exec.tta_cod_prog_dtsul_rp           = "esp/esau0305rp.p"
           tt_ped_exec.tta_cod_release_prog_dtsul      = "0.12.00.000"
           tt_ped_exec.tta_dat_exec_ped_exec           = d-dat-rpw
           tt_ped_exec.tta_hra_exec_ped_exec           = c-hra-rpw
           tt_ped_exec.tta_cod_servid_exec             = ped_exec.cod_servid_exec
           tt_ped_exec.tta_cdn_estil_dwb               = 97.

    ASSIGN tt_ped_exec.tta_log_exec_prog_depend = YES
           tt_ped_exec.tta_num_ped_exec_pai     = i-num-ped-exec-rpw.
           
    CREATE tt_ped_exec_param.
    ASSIGN tt_ped_exec_param.tta_num_seq               = 1
           tt_ped_exec_param.tta_cod_dwb_file          = "esp/esau0305rp.p"
           tt_ped_exec_param.tta_cod_dwb_output        = (IF tt-param.destino = 1 THEN "Impressora" ELSE "Arquivo")
           tt_ped_exec_param.tta_nom_dwb_printer       = tt-param.arquivo
           tt_ped_exec_param.tta_cod_dwb_print_layout  = tt-param.arquivo.

    RAW-TRANSFER tt-param TO tt_ped_exec_param.tta_raw_param_ped_exec.

    RUN btb/btb912zb.p (INPUT-OUTPUT TABLE tt_param_segur,
                        INPUT-OUTPUT TABLE tt_ped_exec,
                        INPUT TABLE tt_ped_exec_param,
                        INPUT TABLE tt_ped_exec_param_aux,
                        INPUT TABLE tt_ped_exec_sel).
end procedure.

PROCEDURE pi-zip-file:
   DEFINE INPUT-OUTPUT PARAMETER ip-arquivo     AS CHARACTER FORMAT "x(100)" NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ip-arquivo-zip AS CHARACTER FORMAT "x(100)" NO-UNDO.

   EMPTY TEMP-TABLE tt-listFiles.

   CREATE tt-listFiles.
   ASSIGN tt-listFiles.cFile = ip-arquivo.

   

   PUT STREAM ST-LOG "entrando|" TODAY "|"  TIME  "|" ip-arquivo-zip SKIP.

   FOR EACH tt-listFiles:
       PUT STREAM ST-LOG
           tt-listFiles.cFile   "|"
           tt-listFiles.lSearch "|" SKIP.

      IF NOT tt-listFiles.cFile  MATCHES "*AU0305*CSV*" THEN
         DELETE tt-listFiles.
   END.

   IF SEARCH(ip-arquivo-zip) <> ? THEN
      OS-DELETE VALUE(ip-arquivo-zip) NO-ERROR.
    
   RUN zipFiles IN h-zip (INPUT ip-arquivo-zip, 
                          INPUT TABLE tt-listFiles,
                          INPUT YES,
                          OUTPUT TABLE tt-erros-zip).

   FOR EACH tt-erros-zip :
      PUT STREAM ST-LOG "ERROS|"  TODAY "|"  TIME  "|" tt-erros-zip.cod-erro "|" tt-erros-zip.desc-erro SKIP.
   END.

   PUT STREAM ST-LOG "saindo|"  TODAY "|"  TIME  "|" ip-arquivo-zip SKIP.
END.


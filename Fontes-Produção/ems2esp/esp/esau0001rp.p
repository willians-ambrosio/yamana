
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESAU0001RP 0.12.00.000}  /*** 010012 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESAU0305RP MAU}
&ENDIF

/******************************************************************************
**
**   Programa: ESAU0305RP.P
**
**   Autor...: Datasul S/A.
**
**   Objetivo: Relat½rio Resultado Atributos Monitorados
*******************************************************************************/
/* BEG - Defini»’o de Pr²-Processadores */
{include/i_dbvers.i} /*vers’o do banco de dados*/
{include/i_dbtype.i} /*tipo do banco de dados*/

DEFINE STREAM ST-LOG.

&IF '{&mgadt_version}' >= '2.07' &THEN
    &SCOPED-DEFINE cdn_tip_banco bf-base_dados.cdn_tip_banco
&ELSE
    &SCOPED-DEFINE cdn_tip_banco bf-base_dados.int-1
&ENDIF

DEFINE BUFFER bf-base_dados FOR base_dados.

DEFINE VARIABLE c-arquivo AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-ambiente AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-erros-zip NO-UNDO
    FIELD cod-erro  AS INTEGER FORMAT ">>>>9"
    FIELD desc-erro AS CHAR    FORMAT "x(70)".

DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "x(200)"
    FIELD lSearch   AS LOGICAL.


DEFINE VARIABLE h-zip AS HANDLE NO-UNDO.
DEFINE VARIABLE c-arquivo-zip AS CHARACTER NO-UNDO.

DEFINE VARIABLE c-arq-log AS CHARACTER NO-UNDO.

ASSIGN c-arq-log = "D:\TEMP\RPW\VERIFICA_ESAU0001_" + REPLACE(STRING(TODAY,"99/99/9999"),"/","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".TXT".

OUTPUT STREAM ST-LOG TO VALUE (c-arq-log).


run utp/ut-zip.p persistent set h-zip.

{include/i-rpvar.i}
{include/tt-edit.i}
{include/pi-edit.i}
{utp/ut-glob.i}
{btb/btb912zb.i}
{utp/utapi019.i}

def var h-acomp                  as handle               no-undo.


define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD tt-dat-ini       AS DATE
    FIELD tt-dat-fim       AS DATE
    FIELD tt-c-base-ini    AS CHAR 
    FIELD tt-c-base-fim    AS CHAR 
    FIELD tt-c-atr-ini     AS CHAR
    FIELD tt-c-atr-fim     AS CHAR
    FIELD tt-c-prog-ini    AS CHAR
    FIELD tt-c-prog-fim    AS CHAR
    FIELD tt-l-super       AS LOGICAL
    field tt-l-create      as logical
    field tt-l-write       as logical
    field tt-l-delete      as LOGICAL
    FIELD rs-tipo-execucao AS INTEGER.

define temp-table tt-digita no-undo
    FIELD selec     AS LOG FORMAT "*/"
    FIELD tipo      AS INT /* 1 - Usuario / 2 - Tabela  */
    field campo-1   as character format "x(30)"
    field campo-2   as character format "x(30)"
    .

def temp-table tt-raw-digita
    field raw-digita as raw.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

for each tt-raw-digita no-lock:
    create tt-digita.
    raw-transfer raw-digita TO tt-digita.
end.

create tt-param.
raw-transfer raw-param to tt-param.

ASSIGN tt-param.arquivo = REPLACE(tt-param.arquivo,'tmp','csv').
ASSIGN tt-param.arquivo = REPLACE(tt-param.arquivo,'txt','csv').
ASSIGN tt-param.arquivo = REPLACE(tt-param.arquivo,'lst','csv').

DEFINE VARIABLE c-destino AS CHARACTER NO-UNDO.

DEFINE VARIABLE l-oracle AS LOGICAL NO-UNDO INITIAL NO.

DEF VAR c-ender-arq AS CHAR NO-UNDO.
DEF VAR c-titulo    AS CHAR NO-UNDO.
def var i-linha     AS INT INIT 4 no-undo.
DEF VAR l-achou     AS LOG        NO-UNDO.
DEF TEMP-TABLE tt_usuar_mestre NO-UNDO
  FIELD cod_usuario LIKE usuar_mestre.cod_usuario
  INDEX idx01 AS PRIMARY UNIQUE cod_usuario.
DEF TEMP-TABLE tt_tabela_monitor NO-UNDO
  FIELD cod_base_dados LIKE tabela_monitor.cod_base_dados
  FIELD cod_tabela     LIKE tabela_monitor.cod_tabela.
/* DEF TEMP-TABLE tt_usu_bco_tab                                */
/*   FIELD cod_base_dados LIKE tt_tabela_monitor.cod_base_dados */
/*   FIELD cod_tabela     LIKE tt_tabela_monitor.cod_tabela     */
/*   FIELD cod_usuario    LIKE tt_usuar_mestre.cod_usuario.     */
/* DEF STREAM s-excel. */

{include/i-rpcab.i}

{include/i-rpout.i}
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input string(c-titulo-relat)).
/*                                                               */
assign c-programa = "ESAU0001RP"
       c-versao   = "0.12"
       c-revisao  = "00.000"
       c-sistema  = "ESP".
/*----Logica Principal*/
find first param-global no-lock no-error.
RUN pi-processa.
/*----Fim Logica Principal*/
{include/i-rpclo.i}
run pi-finalizar in h-acomp. 

/* ASSIGN c-destino = "AHR-SOXReportDatasul@Yamana.com". */
/*                                                       */
/* ASSIGN c-destino = "sergio.silveira@dsc.com.br".      */

FOR EACH es_parametro
         WHERE es_parametro.cod_prog_dtsul = "ESAU0001" AND
               es_parametro.cod_referencia = "E-MAIL"   
         NO-LOCK:
   ASSIGN c-destino = es_parametro.cod_parametro.
END.

ASSIGN c-arquivo     = tt-param.arquivo.
       

IF i-num-ped-exec-rpw <> 0 Then 
   Assign c-arquivo = c-dir-spool-servid-exec + "~/" + tt-param.arquivo.

IF i-num-ped-exec-rpw <> 0 Then
   ASSIGN c-arquivo-zip = "d:\temp\rpw\esau0001_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".zip".
ELSE
   ASSIGN c-arquivo-zip = "d:\temp\esau0001_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".zip".

RUN pi-zip-file (INPUT-OUTPUT c-arquivo,
                 INPUT-OUTPUT c-arquivo-zip).

    find first param_email no-lock no-error.

    create tt-envio2.
    assign tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = param_email.log_servid_exchange
           tt-envio2.servidor          = param_email.cod_servid_e_mail
           tt-envio2.porta             = param_email.num_porta
           tt-envio2.destino           = c-destino
           tt-envio2.assunto           = "Relat½rio Resultado Monitoramento Audit - ESAU0001"
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
           tt-mensagem.mensagem     = "Controle : GC AC KY10" + CHR(10) + CHR(13).
    
    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 3
           tt-mensagem.mensagem     = "Ambiente : " + c-ambiente + CHR(10) + CHR(13).

    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 4
           tt-mensagem.mensagem     = "Voc¼ estÿ recebendo este email pois foi executada a rotina ESAU0001 do produto Totvs 12".

    FOR EACH tt-envio2:

        /*FOR EACH tt-envio2: DELETE tt-envio2. END.
        CREATE tt-envio2.
        BUFFER-COPY tt-envio2 TO tt-envio2.*/
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

if tt-param.rs-tipo-execucao = 2 then
   run agendarExecucaoAutomatica.

return 'ok'.

/*----procedures internas---------------------*/
PROCEDURE pi-processa:
  RUN pi-carrega-dados.
  RUN pi-seleciona-dados.
END PROCEDURE.
PROCEDURE pi-carrega-dados:
  /*pi-carrega-dados*/
  FOR EACH tt_usuar_mestre:   DELETE tt_usuar_mestre.   END.
  FOR EACH tt_tabela_monitor: DELETE tt_tabela_monitor. END.
  FOR EACH tt-digita 
    NO-LOCK
    WHERE tt-digita.tipo  = 1 /* 1 - Usuario  */
    AND   tt-digita.selec = YES:
      CREATE tt_usuar_mestre.
      ASSIGN tt_usuar_mestre.cod_usuario = tt-digita.campo-1.
    END. /* FOR EACH tt-digita 1 */
  FOR EACH tt-digita 
    NO-LOCK
    WHERE tt-digita.tipo  = 2 /* 2 - Tabela  */
    AND   tt-digita.selec = YES:
      CREATE tt_tabela_monitor.
      ASSIGN 
        tt_tabela_monitor.cod_base_dados = tt-digita.campo-1
        tt_tabela_monitor.cod_tabela     = tt-digita.campo-2.
    END. /* FOR EACH tt-digita 2 */
END PROCEDURE.
PROCEDURE pi-seleciona-dados:
  /*pi-seleciona-dados*/
  FIND FIRST tt-param NO-LOCK NO-ERROR.
  /* ASSIGN c-ender-arq = SESSION:TEMP-DIRECTORY + STRING(TIME) + STRING(TODAY,"99999999") + ".csv". */
  ASSIGN c-ender-arq = tt-param.arquivo.
  /* FOR EACH tt_usu_bco_tab: DELETE tt_usu_bco_tab. END. */
  /* run utp/ut-acomp.p persistent set h-acomp.                      */
  /* run pi-inicializar in h-acomp(input "Aguarde ...Verificando."). */
  assign i-linha = 0.
  assign i-linha = i-linha + 1.
  /* run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)). */
  /*-----------------------------------------------------------*/
  DEF VAR i-x AS INT NO-UNDO.
  ASSIGN i-x = 0.
  FOR EACH tt_tabela_monitor:
    ASSIGN i-x = i-x + 1.
  END.
  /*
  FOR EACH tt_usuar_mestre
    NO-LOCK /*,
    EACH tt_tabela_monitor
    NO-LOCK
    WHERE  tt_tabela_monitor.cod_base_dados    >= tt-param.tt-c-base-ini
    AND    tt_tabela_monitor.cod_base_dados    <= tt-param.tt-c-base-fim */:
      ASSIGN i-x = i-x + 1.
      /*
      /*------------------------------------------------------------*/
      FIND tt_usu_bco_tab
        NO-LOCK
        WHERE tt_usu_bco_tab.cod_base_dados = tt_tabela_monitor.cod_base_dados
        AND   tt_usu_bco_tab.cod_tabela     = tt_tabela_monitor.cod_tabela
        AND   tt_usu_bco_tab.cod_usuario    = tt_usuar_mestre.cod_usuario
        NO-ERROR.
      IF NOT AVAIL tt_usu_bco_tab THEN
        DO:
          CREATE tt_usu_bco_tab.
          ASSIGN
            tt_usu_bco_tab.cod_base_dados = tt_tabela_monitor.cod_base_dados
            tt_usu_bco_tab.cod_tabela     = tt_tabela_monitor.cod_tabela
            tt_usu_bco_tab.cod_usuario    = tt_usuar_mestre.cod_usuario.
        END. /* IF NOT AVAIL tt_usu_bco_tab THEN */
      */  
      /*------------------------------------------------------------*/
    END. /* FOR EACH tt_usuar_mestre */
  */  
  assign i-linha = i-linha + 1.
  /* run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)). */
  assign i-linha = i-linha + 1.
  /* run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)). */
  /* OUTPUT STREAM s-excel TO VALUE(c-ender-arq) NO-CONVERT. */
  ASSIGN
    c-titulo = "Base de dados;Tabela;Atributo;Programa;Programa;Data Atualiza»’o;Hora Atualiza»’o;Evento;Usuÿrio;".
  PUT UNFORMATTED /* STREAM s-excel */
    c-titulo FORMAT "X(600)"
    SKIP.
  assign i-linha = i-linha + 1.
  /* run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)). */
  ASSIGN l-achou = NO.
  /*----*/
  /*----*/

   If i-num-ped-exec-rpw <> 0 then do:
      if month(today) = 1 then
         assign tt-param.tt-dat-ini = date(12,01,year(today) - 1)
                tt-param.tt-dat-fim = date(01,01,year(today)) - 1.
      else
         assign tt-param.tt-dat-ini = date(month(today) - 1,01,year(today))
                tt-param.tt-dat-fim = date(month(today),01,year(today)) - 1.
   end.

   DEF VAR dt-data AS DATE NO-UNDO. 

   FOR EACH tt_tabela_monitor
            NO-LOCK:
      assign i-linha = i-linha + 1.                                          
           
      
      DO dt-data = tt-param.tt-dat-ini TO tt-param.tt-dat-fim:
         blk-tabela:
         FOR EACH tabela_vrf_monitor USE-INDEX tabela_data
                  WHERE tabela_vrf_monitor.cod_tabela      = tt_tabela_monitor.cod_tabela AND   
                        tabela_vrf_monitor.dat_atualiz     = dt-data  
                  NO-LOCK:

            run pi-acompanhar in h-acomp(input "Tabela/Data : " + tt_tabela_monitor.cod_tabela + " - " + string(dt-data,'99/99/9999')). 

            FIND tt_usuar_mestre
                 WHERE tt_usuar_mestre.cod_usuario = tabela_vrf_monitor.cod_usuario
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL tt_usuar_mestre THEN 
               NEXT blk-tabela.

            assign i-linha = i-linha + 1.
            /*         run pi-acompanhar in h-acomp(input "*Linha* : " + string(i-linha)). */
            blk-atrib:
            FOR EACH  atrib_vrf_monitor OF tabela_vrf_monitor
              WHERE atrib_vrf_monitor.cod_atributo >= tt-param.tt-c-atr-ini
              AND   atrib_vrf_monitor.cod_atributo <= tt-param.tt-c-atr-fim:
                /*-------------------------------------------------------------*/
                assign i-linha = i-linha + 1.
                /*             run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)). */
                /*-------------------------------------------------------------*/
                IF (tabela_vrf_monitor.cod_evento = 'C' AND tt-param.tt-l-create = NO)
                OR (tabela_vrf_monitor.cod_evento = 'W' AND tt-param.tt-l-write  = NO)
                OR (tabela_vrf_monitor.cod_evento = 'D' AND tt-param.tt-l-delete = NO)
                THEN NEXT.
                /*-------------------------------------------------------------*/

                /* Oracle Nativo possui apenas um programa, ent’o deve verificar o primeiro item */
                FIND FIRST bf-base_dados
                    where bf-base_dados.cod_base_dados = tabela_vrf_monitor.cod_base_dados
                    no-lock no-error.
                if  avail bf-base_dados THEN DO:
                    /* Se Oracle ou SQL Server nativo entÊo desativa o botÊo de check syntax */
                    ASSIGN l-oracle = ({&cdn_tip_banco} = 3 OR {&cdn_tip_banco} = 5).
                
                    IF l-oracle THEN DO:
                        IF NOT (tabela_vrf_monitor.des_prog_atualiz[1]    >= tt-param.tt-c-prog-ini
                            AND tabela_vrf_monitor.des_prog_atualiz[1]    <= tt-param.tt-c-prog-fim) THEN
                            NEXT blk-atrib.
                    END.
                    ELSE DO:
                        IF NOT (tabela_vrf_monitor.des_prog_atualiz[2]    >= tt-param.tt-c-prog-ini
                            AND tabela_vrf_monitor.des_prog_atualiz[2]    <= tt-param.tt-c-prog-fim) THEN
                            NEXT blk-atrib.
                    END.
                END.

                /*-------------------------------------------------------------*/
                ASSIGN l-achou = YES.
                RUN pi-imprime.
                /*-------------------------------------------------------------*/
              END. /* FOR EACH  atrib_vrf_monitor OF tabela_vrf_monitor */
    
          END. /* FOR EACH tabela_vrf_monitor */
      END. /* DO dt-data = tt-param.tt-dat-ini TO tt-param.tt-dat-fim: */
    END. /* FOR EACH tt_tabela_vrf_monitor */
  /*------*/  
  IF l-achou = NO THEN
    PUT UNFORMATTED "Nenhum Registro Foi Encontrado".
  /* OUTPUT STREAM s-excel CLOSE. */
  /* run pi-finalizar in h-acomp. */
  /* DOS SILENT START excel.exe VALUE(c-ender-arq). */

END PROCEDURE.
PROCEDURE pi-imprime:
  /* "Base de dados;Tabela;Atributo;Programa;Programa;Data Atualiza»’o;Hora Atualiza»’o;Evento;". */
  PUT UNFORMATTED 
    tabela_vrf_monitor.cod_base_dados        ";"  
    tabela_vrf_monitor.cod_tabela            ";"
    atrib_vrf_monitor.cod_atributo           ";"
    tabela_vrf_monitor.des_prog_atualiz[2]   ";"
    tabela_vrf_monitor.des_prog_atualiz[1]   ";"
    tabela_vrf_monitor.dat_atualiz           ";"
    STRING(INT(tabela_vrf_monitor.hra_atualiz),"HH:MM:SS") ";"
    tabela_vrf_monitor.cod_evento            ";"
    tt_usuar_mestre.cod_usuario              ";"
    SKIP.
END PROCEDURE.
/*----fim procedures internas-----------------*/

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
           tt_ped_exec.tta_cod_prog_dtsul              = "ESAU0001"
           tt_ped_exec.tta_cod_prog_dtsul_rp           = "esp/esau0001rp.p"
           tt_ped_exec.tta_cod_release_prog_dtsul      = "0.12.00.000"
           tt_ped_exec.tta_dat_exec_ped_exec           = d-dat-rpw
           tt_ped_exec.tta_hra_exec_ped_exec           = c-hra-rpw
           tt_ped_exec.tta_cod_servid_exec             = ped_exec.cod_servid_exec
           tt_ped_exec.tta_cdn_estil_dwb               = 97.

    ASSIGN tt_ped_exec.tta_log_exec_prog_depend = YES
           tt_ped_exec.tta_num_ped_exec_pai     = i-num-ped-exec-rpw.
           
    CREATE tt_ped_exec_param.
    ASSIGN tt_ped_exec_param.tta_num_seq               = 1
           tt_ped_exec_param.tta_cod_dwb_file          = "esp/esau0001rp.p"
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

      IF NOT tt-listFiles.cFile  MATCHES "*ESAU0001*CSV*" THEN
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


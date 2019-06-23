/*------------------------------------------------------------------------------
  Purpose...: Emissao relatorio Acesso Minas
  Parameters:  <none>
  Notes.....: HCM Consulting / Setembro-2017
------------------------------------------------------------------------------*/
{include/i-prgvrs.i YMSA0003RP 1.00.00.001}
{prghur/esp/YMSA0003tt.i}

DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
   CREATE tt-digita.
   RAW-TRANSFER raw-digita TO tt-digita.
END.

DEFINE VARIABLE h-acomp             AS HANDLE                           NO-UNDO.
DEFINE VARIABLE Cnxn0               AS COM-HANDLE                       NO-UNDO.
DEFINE VARIABLE rs0                 AS COM-HANDLE                       NO-UNDO.
DEFINE VARIABLE ObjCommand0         AS COM-HANDLE                       NO-UNDO.
DEFINE VARIABLE odbc-query          AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE ado-null            AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE ado-recordcont      AS INTEGER                          NO-UNDO.
DEFINE VARIABLE ado-registro        AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_cdn_disp_aux      AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_cod_ip            AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE v_cod_dat_ini       AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE v_cod_dat_fim       AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE v_num_linha         AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_qti_horas         AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_qti_hra_aux       AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_log_par           AS LOGICAL                          NO-UNDO.
DEFINE VARIABLE v_des_batidas       AS CHARACTER                        NO-UNDO.

DEFINE VARIABLE chExcel             AS office.iface.excel.ExcelWrapper  NO-UNDO.
DEFINE VARIABLE chWBook             AS office.iface.excel.Workbook      NO-UNDO.
DEFINE VARIABLE chWSheet            AS office.iface.excel.WorkSheet     NO-UNDO.  
DEFINE VARIABLE chWSheetRange       AS office.iface.excel.WorkSheet     NO-UNDO.

DEFINE TEMP-TABLE tt-registros NO-UNDO
   FIELD cod_pis            AS CHARACTER FORMAT "x(12)"
   FIELD cod_externo        AS CHARACTER FORMAT "x(99)"
   FIELD cod_nro_pessoa     AS CHARACTER FORMAT "x(20)"
   FIELD des_data           AS CHARACTER FORMAT "x(20)"
   FIELD des_hora           AS CHARACTER FORMAT "x(20)"
   FIELD cod_ip_disp        AS CHARACTER FORMAT "x(40)"
   INDEX reg_id AS UNIQUE PRIMARY cod_pis des_data des_hora.

DEFINE TEMP-TABLE tt-resumo NO-UNDO
   FIELD dat_batida         AS DATE
   FIELD qti_func           AS INTEGER
   FIELD hra_total          AS INTEGER
   INDEX idprim AS UNIQUE PRIMARY dat_batida.

DEFINE TEMP-TABLE tt-dados NO-UNDO
   FIELD cdn_empresa        LIKE funcionario.cdn_empresa
   FIELD cdn_estab          LIKE funcionario.cdn_estab
   FIELD cdn_funcionario    LIKE funcionario.cdn_funcionario
   FIELD nom_funcionario    LIKE funcionario.nom_pessoa_fisic
   FIELD cod_rh_ccusto      LIKE funcionario.cod_rh_ccusto
   FIELD dat_batida         AS DATE
   FIELD hra_batida         AS INTEGER
   FIELD cod_ip_disp        AS CHARACTER
   INDEX idprim AS UNIQUE PRIMARY cdn_empresa cdn_estab cdn_funcionario dat_batida hra_batida.

&scoped-define xlEdgeLeft 7
&scoped-define xlEdgeTop 8
&scoped-define xlEdgeBottom 9
&scoped-define xlEdgeRight 10

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Conectando Banco TSA":U).

/* Busca dados TSA */
CREATE "ADODB.Connection" Cnxn0.
CREATE "ADODB.RecordSet"  rs0.
CREATE "ADODB.Command"    ObjCommand0.

/*
ASSIGN Cnxn0:ConnectionString = "Server=YDMSQLTESTE\TSA;Provider=SQLNCLI10;Server=YDMSQLTESTE;User Id=SVC.TSA;Password=1325YOK@;".
*/
ASSIGN Cnxn0:ConnectionString = "Server=YDMPRIMA02\TSA;Provider=SQLNCLI10;Server=YDMPRIMA02;User Id=SVC.TSA;Password=9872HLK%;".

Cnxn0:OPEN(,,,) NO-ERROR.
IF (ERROR-STATUS:NUM-MESSAGES > 0) THEN DO:
    RUN pi-finalizar IN h-acomp.
    MESSAGE "Erro: Conex∆o com banco SQL n∆o pìde ser efetuada." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN "NOK":U.
END.

RUN pi-inicializar IN h-acomp (INPUT "Processando...":U).

FOR EACH tt-digita,
   FIRST disposit_control_aces NO-LOCK
   WHERE disposit_control_aces.cdn_planta_edificio = tt-digita.cdn_planta
     AND disposit_control_aces.cdn_local_aces      = tt-digita.cdn_local
      BY disposit_control_aces.cdn_disposit_control_aces:

   RUN pi-acompanhar IN h-acomp (INPUT "Dispositivo: ":U + STRING(disposit_control_aces.cdn_disposit_control_aces)).

   IF v_cdn_disp_aux <> disposit_control_aces.cdn_disposit_control_aces THEN DO:
      IF v_cod_ip = "" THEN
         ASSIGN v_cod_ip = disposit_control_aces.cod_ender_disposit.
      ELSE
         ASSIGN v_cod_ip = v_cod_ip + " or " + disposit_control_aces.cod_ender_disposit.

      ASSIGN v_cdn_disp_aux = disposit_control_aces.cdn_disposit_control_aces.
   END.
END.

ASSIGN v_cod_dat_ini = STRING(YEAR(tt-param.dat_bat_ini),"9999") + "-" + 
                       STRING(MONTH(tt-param.dat_bat_ini),"99") + "-" +
                       STRING(DAY(tt-param.dat_bat_ini),"99")
       v_cod_dat_fim = STRING(YEAR(tt-param.dat_bat_fim),"9999") + "-" + 
                       STRING(MONTH(tt-param.dat_bat_fim),"99") + "-" +
                       STRING(DAY(tt-param.dat_bat_fim),"99").

ASSIGN odbc-query = "select USER_SA.PIS, USER_SA.EXTERNAL_KEY, USER_SA.ID_NATURAL_PERSON, MARK.RECORD_TIME_DATE, MARK.RECORD_TIME_HOUR, DEVICE.NETWORK_ADDRESS" +
                    "  from MARK" +
                    "  left join DEVICE on MARK.ID_DEVICE = DEVICE.ID" +
                    "  left join USER_ROLE_BADGE on MARK.ID_USER_ROLE_BADGE = USER_ROLE_BADGE.ID" +
                    "  left join USER_SA on USER_ROLE_BADGE.ID_USER_SA = USER_SA.ID" +
                    " where MARK.RECORD_TIME_DATE >= '" + v_cod_dat_ini + "'" +
                    "   and MARK.RECORD_TIME_DATE <= '" + v_cod_dat_fim + "'" +
                    "   and CHARINDEX(DEVICE.NETWORK_ADDRESS,'" + v_cod_ip + "',1) > 0;".

ObjCommand0:ActiveConnection = Cnxn0.
ObjCommand0:CommandText      = odbc-query.
objCommand0:CommandType      = 1.
Cnxn0:CursorLocation         = 3.
rs0:CursorType               = 1.
rs0:LockType                 = 3.
rs0                          = ObjCommand0:EXECUTE(OUTPUT ado-null,"",32).
ado-recordcont               = rs0:RecordCount.

EMPTY TEMP-TABLE tt-registros NO-ERROR.

DO TRANSACTION:
   IF (ado-recordcont > 0) AND NOT(ado-recordcont = ?) THEN DO:
      rs0:MoveFirst NO-ERROR.

      DO WHILE ado-registro < ado-recordcont:

         RUN pi-acompanhar IN h-acomp (INPUT "TSA: ":U + rs0:FIELDS("ID_NATURAL_PERSON"):VALUE + 
                                             ", " + rs0:FIELDS("RECORD_TIME_DATE"):VALUE).

         CREATE tt-registros.
         ASSIGN tt-registros.cod_pis        = rs0:FIELDS("PIS"):VALUE
                tt-registros.cod_externo    = rs0:FIELDS("EXTERNAL_KEY"):VALUE
                tt-registros.cod_nro_pessoa = rs0:FIELDS("ID_NATURAL_PERSON"):VALUE
                tt-registros.des_data       = rs0:FIELDS("RECORD_TIME_DATE"):VALUE
                tt-registros.des_hora       = rs0:FIELDS("RECORD_TIME_HOUR"):VALUE
                tt-registros.cod_ip_disp    = rs0:FIELDS("NETWORK_ADDRESS"):VALUE.

         ASSIGN ado-registro = ado-registro + 1.
         rs0:MoveNext.
      END.
   END.
END.

ObjCommand0:CLOSE           NO-ERROR.
RELEASE OBJECT Cnxn0        NO-ERROR.
RELEASE OBJECT ObjCommand0  NO-ERROR.
RELEASE OBJECT rs0          NO-ERROR.

ASSIGN Cnxn0       = ?
       ObjCommand0 = ?
       rs0         = ?.    

/* Consolida Dados */
FOR EACH tt-registros:

   RUN pi-acompanhar IN h-acomp (INPUT "Consolidaá∆o: ":U + tt-registros.cod_externo).


   CREATE tt-dados.
   ASSIGN tt-dados.dat_batida  = DATE(INTEGER(SUBSTRING(tt-registros.des_data,04,02)),
                                      INTEGER(SUBSTRING(tt-registros.des_data,01,02)),
                                      INTEGER(SUBSTRING(tt-registros.des_data,07,04)))
          tt-dados.hra_batida  = (INTEGER(SUBSTRING(tt-registros.des_hora,12,02)) * 3600) +
                                 (INTEGER(SUBSTRING(tt-registros.des_hora,15,02)) * 60) +
                                  INTEGER(SUBSTRING(tt-registros.des_hora,18,02))
          tt-dados.cod_ip_disp = tt-registros.cod_ip_disp.


   IF NUM-ENTRIES(tt-registros.cod_externo,";") = 3 THEN DO:
      FIND FIRST funcionario NO-LOCK
           WHERE funcionario.cdn_empresa     = ENTRY(01,tt-registros.cod_externo,";")
             AND funcionario.cdn_estab       = ENTRY(02,tt-registros.cod_externo,";")
             AND funcionario.cdn_funcionario = INTEGER(ENTRY(03,tt-registros.cod_externo,";")) NO-ERROR.
      IF AVAIL funcionario THEN DO:
         ASSIGN tt-dados.cdn_empresa     = funcionario.cdn_empresa
                tt-dados.cdn_estab       = funcionario.cdn_estab
                tt-dados.cdn_funcionario = funcionario.cdn_funcionario
                tt-dados.nom_funcionario = funcionario.nom_pessoa_fisic.

         FIND FIRST func_ccusto OF funcionario NO-LOCK
              WHERE func_ccusto.dat_inic_lotac_func <= tt-dados.dat_batida
                AND func_ccusto.dat_fim_lotac_func  >= tt-dados.dat_batida NO-ERROR.
         IF AVAIL func_ccusto THEN
            ASSIGN tt-dados.cod_rh_ccusto = func_ccusto.cod_rh_ccusto.
      END.
   END.
   ELSE DO:
      FOR EACH funcionario NO-LOCK
         WHERE funcionario.cod_pis = tt-registros.cod_pis,
         FIRST func_ccusto OF funcionario NO-LOCK
         WHERE func_ccusto.dat_inic_lotac_func <= tt-dados.dat_batida
           AND func_ccusto.dat_fim_lotac_func  >= tt-dados.dat_batida:
         ASSIGN tt-dados.cdn_empresa     = funcionario.cdn_empresa
                tt-dados.cdn_estab       = funcionario.cdn_estab
                tt-dados.cdn_funcionario = funcionario.cdn_funcionario
                tt-dados.nom_funcionario = funcionario.nom_pessoa_fisic
                tt-dados.cod_rh_ccusto   = func_ccusto.cod_rh_ccusto.
      END.
   END.
END.

/* Impressao Excel */
{office/office.i Excel chExcel}
chExcel:SheetsInNewWorkbook = 1.
chExcel:Visible = FALSE.

chWBook = chExcel:Workbooks:Add().
chWSheet = chWBook:Sheets:Item(1).
chExcel:Workbooks:Item(1):Sheets(1):Name = "Detalhes".

chWSheet:Range("A1"):Value = "Empresa".
chWSheet:Range("B1"):Value = "Estabelecimento".
chWSheet:Range("C1"):Value = "Matr°cula".
chWSheet:Range("D1"):Value = "Nome".
chWSheet:Range("E1"):Value = "Centro Custo".
chWSheet:Range("F1"):Value = "Data Batida".
chWSheet:Range("G1"):Value = "Batidas".
chWSheet:Range("H1"):Value = "Qtd. Horas".
chWSheet:Range("A1:H1"):Font:Bold = TRUE.
chWSheet:Range("A1:H1"):Borders({&xlEdgeRightLeftTopBottom}):LineStyle = 1.

ASSIGN v_num_linha = 1.

FOR EACH tt-dados
   BREAK BY tt-dados.cdn_empresa
         BY tt-dados.cdn_estab
         BY tt-dados.cdn_funcionario
         BY tt-dados.dat_batida
         BY tt-dados.hra_batida:

   RUN pi-acompanhar IN h-acomp (INPUT "Impress∆o: ":U + STRING(tt-dados.cdn_funcionario) + 
                                       ", Data " + STRING(tt-dados.dat_batida,"99/99/9999")).

   IF FIRST-OF(tt-dados.dat_batida) THEN
      ASSIGN v_qti_horas   = 0
             v_qti_hra_aux = 0
             v_log_par     = YES
             v_des_batidas = STRING(tt-dados.hra_batida,"HH:MM:SS").
   ELSE
      ASSIGN v_des_batidas = v_des_batidas + " - " + STRING(tt-dados.hra_batida,"HH:MM:SS").

   IF v_log_par THEN
      ASSIGN v_log_par     = NO
             v_qti_hra_aux = tt-dados.hra_batida.
   ELSE
      ASSIGN v_log_par     = YES
             v_qti_horas   = v_qti_horas + (tt-dados.hra_batida - v_qti_hra_aux).

   IF LAST-OF(tt-dados.dat_batida) THEN DO:
      ASSIGN v_num_linha = v_num_linha + 1.

      chWSheet:Range("A" + STRING(v_num_linha)):Value = tt-dados.cdn_empresa.
      chWSheet:Range("B" + STRING(v_num_linha)):Value = tt-dados.cdn_estab.
      chWSheet:Range("C" + STRING(v_num_linha)):Value = STRING(tt-dados.cdn_funcionario).
      chWSheet:Range("D" + STRING(v_num_linha)):Value = tt-dados.nom_funcionario.
      chWSheet:Range("E" + STRING(v_num_linha)):Value = tt-dados.cod_rh_ccusto.
      chWSheet:Range("F" + STRING(v_num_linha)):NumberFormat = "@".
      chWSheet:Range("F" + STRING(v_num_linha)):Value = STRING(tt-dados.dat_batida,"99/99/9999").
      chWSheet:Range("G" + STRING(v_num_linha)):Value = v_des_batidas.
      chWSheet:Range("H" + STRING(v_num_linha)):Value = STRING(v_qti_horas,"HH:MM:SS").

      FIND FIRST tt-resumo
           WHERE tt-resumo.dat_batida = tt-dados.dat_batida NO-ERROR.
      IF NOT AVAIL tt-resumo THEN DO:
         CREATE tt-resumo.
         ASSIGN tt-resumo.dat_batida = tt-dados.dat_batida
                tt-resumo.qti_func   = 1
                tt-resumo.hra_total  = v_qti_horas.
      END.
      ELSE
         ASSIGN tt-resumo.qti_func   = tt-resumo.qti_func + 1
                tt-resumo.hra_total  = tt-resumo.hra_total + v_qti_horas.
   END.
END.

IF VALID-OBJECT(chWSheet)      THEN DELETE OBJECT chWSheet.
chWSheet = chWBook:Sheets:Item().
chWSheet = chWBook:Sheets:Add().
chExcel:Workbooks:Item(1):Sheets(1):Name = "Resumo".

chWSheet:Range("A1"):Value = "Data Batida".
chWSheet:Range("B1"):Value = "Total de Funcion†rios".
chWSheet:Range("C1"):Value = "Total de Horas".
chWSheet:Range("A1:C1"):Font:Bold = TRUE.
chWSheet:Range("A1:C1"):Borders({&xlEdgeRightLeftTopBottom}):LineStyle = 1.

ASSIGN v_num_linha = 1.

FOR EACH tt-resumo:
   ASSIGN v_num_linha = v_num_linha + 1.

   chWSheet:Range("A" + STRING(v_num_linha)):NumberFormat = "@".
   chWSheet:Range("A" + STRING(v_num_linha)):Value = STRING(tt-resumo.dat_batida,"99/99/9999").
   chWSheet:Range("B" + STRING(v_num_linha)):Value = STRING(tt-resumo.qti_func).
   chWSheet:Range("C" + STRING(v_num_linha)):Value = TRIM(STRING(TRUNCATE(tt-resumo.hra_total / 3600,0),">>99")) + ":" +
                                                     STRING(((tt-resumo.hra_total - INTEGER(tt-resumo.hra_total MOD 60)) / 60) MOD 60,"99") + ":" +
                                                     STRING(tt-resumo.hra_total MOD 60,"99").
END.

chExcel:Visible = TRUE.

IF VALID-OBJECT(chWSheet)      THEN DELETE OBJECT chWSheet.
IF VALID-OBJECT(chWSheetRange) THEN DELETE OBJECT chWSheetRange.
IF VALID-OBJECT(chWBook)       THEN DELETE OBJECT chWBook.
IF VALID-OBJECT(chExcel)       THEN DELETE OBJECT chExcel.

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.

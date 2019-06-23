/*------------------------------------------------------------------------------
  Purpose...: Emissao relatorio Tipo Refeicao
  Parameters:  <none>
  Notes.....: HCM Consulting / Setembro-2017
------------------------------------------------------------------------------*/
{include/i-prgvrs.i YMSA0002RP 1.00.00.001}
{prghur/esp/YMSA0002tt.i}

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
DEFINE VARIABLE v_val_ref           AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE v_qti_ref           AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_val_tip_ref       AS DECIMAL                          NO-UNDO.

DEFINE VARIABLE chExcel             AS office.iface.excel.ExcelWrapper  NO-UNDO.
DEFINE VARIABLE chWBook             AS office.iface.excel.Workbook      NO-UNDO.
DEFINE VARIABLE chWSheet            AS office.iface.excel.WorkSheet     NO-UNDO.  
DEFINE VARIABLE chWSheetRange       AS office.iface.excel.WorkSheet     NO-UNDO.

DEFINE TEMP-TABLE tt-registros NO-UNDO
   FIELD cod_pis           AS CHARACTER FORMAT "x(12)"
   FIELD cod_externo       AS CHARACTER FORMAT "x(99)"
   FIELD cod_nro_pessoa    AS CHARACTER FORMAT "x(20)"
   FIELD des_data          AS CHARACTER FORMAT "x(20)"
   FIELD des_hora          AS CHARACTER FORMAT "x(20)"
   FIELD cod_ip_disp       AS CHARACTER FORMAT "x(40)"
   INDEX reg_id AS UNIQUE PRIMARY cod_pis des_data des_hora.

DEFINE TEMP-TABLE tt-resumo NO-UNDO
   FIELD cdn_tip_ref    AS INTEGER
   FIELD des_tip_ref    AS CHARACTER
   FIELD val_ref        AS DECIMAL
   FIELD qti_ref        AS INTEGER
   FIELD val_tot_ref    AS DECIMAL
   INDEX idprim AS UNIQUE PRIMARY cdn_tip_ref.

DEFINE TEMP-TABLE tt-dados NO-UNDO
   FIELD cdn_empresa        LIKE funcionario.cdn_empresa
   FIELD cdn_estab          LIKE funcionario.cdn_estab
   FIELD cdn_funcionario    LIKE funcionario.cdn_funcionario
   FIELD nom_funcionario    LIKE funcionario.nom_pessoa_fisic
   FIELD cod_rh_ccusto      LIKE funcionario.cod_rh_ccusto
   FIELD dat_batida         AS DATE
   FIELD hra_batida         AS INTEGER
   FIELD cdn_tip_ref        AS INTEGER
   FIELD des_tip_ref        AS CHARACTER
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
    MESSAGE "Erro: ConexÆo com banco SQL nÆo p“de ser efetuada." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN "NOK":U.
END.

RUN pi-inicializar IN h-acomp (INPUT "Processando...":U).

FOR EACH tt-digita,
   FIRST tip_refeicao NO-LOCK
   WHERE tip_refeicao.cdn_tip_refeicao = tt-digita.cdn_tip_refeicao,
    EACH tip_refeicao_disp OF tip_refeicao NO-LOCK
   WHERE tip_refeicao_disp.cdn_disposit_control_aces >= tt-param.cdn_disp_ini
     AND tip_refeicao_disp.cdn_disposit_control_aces <= tt-param.cdn_disp_fim,
   FIRST disposit_control_aces NO-LOCK
   WHERE disposit_control_aces.cdn_disposit_control_aces = tip_refeicao_disp.cdn_disposit_control_aces
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

ASSIGN v_cod_dat_ini = STRING(YEAR(tt-param.dat_ref_ini),"9999") + "-" + 
                       STRING(MONTH(tt-param.dat_ref_ini),"99") + "-" +
                       STRING(DAY(tt-param.dat_ref_ini),"99")
       v_cod_dat_fim = STRING(YEAR(tt-param.dat_ref_fim),"9999") + "-" + 
                       STRING(MONTH(tt-param.dat_ref_fim),"99") + "-" +
                       STRING(DAY(tt-param.dat_ref_fim),"99").

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

   RUN pi-acompanhar IN h-acomp (INPUT "Consolida‡Æo: ":U + tt-registros.cod_externo).

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

   FIND FIRST tip_refeicao NO-LOCK
        WHERE tip_refeicao.num_hra_period_ini <= tt-dados.hra_batida
          AND tip_refeicao.num_hra_period_fim >= tt-dados.hra_batida NO-ERROR.
   IF AVAIL tip_refeicao THEN
      ASSIGN tt-dados.cdn_tip_ref = tip_refeicao.cdn_tip_refeicao
             tt-dados.des_tip_ref = tip_refeicao.des_tip_refeicao.
END.

/* Impressao Excel */
{office/office.i Excel chExcel}
chExcel:SheetsInNewWorkbook = 1.
chExcel:Visible = FALSE.

chWBook = chExcel:Workbooks:Add().
chWSheet = chWBook:Sheets:Item(1).
chExcel:Workbooks:Item(1):Sheets(1):Name = "Detalhes".

chWSheet:Range("A1"):Value = "Tipo Refei‡Æo".
chWSheet:Range("B1"):Value = "Descri‡Æo".
chWSheet:Range("C1"):Value = "Data Batida".
chWSheet:Range("D1"):Value = "Hora Batida".
chWSheet:Range("E1"):Value = "Dispositivo".
chWSheet:Range("F1"):Value = "Empresa".
chWSheet:Range("G1"):Value = "Estab.".
chWSheet:Range("H1"):Value = "Matr¡cula".
chWSheet:Range("I1"):Value = "Nome Funcion rio".
chWSheet:Range("J1"):Value = "Centro Custo".
chWSheet:Range("A1:J1"):Font:Bold = TRUE.
chWSheet:Range("A1:J1"):Borders({&xlEdgeRightLeftTopBottom}):LineStyle = 1.

ASSIGN v_num_linha = 1.

FOR EACH tt-dados
   BREAK BY tt-dados.cdn_tip_ref
         BY tt-dados.dat_batida
         BY tt-dados.hra_batida:

   RUN pi-acompanhar IN h-acomp (INPUT "ImpressÆo: ":U + STRING(tt-dados.cdn_funcionario) + 
                                       ", Data " + STRING(tt-dados.dat_batida,"99/99/9999")).

   IF FIRST-OF(tt-dados.cdn_tip_ref) THEN DO:
      ASSIGN v_val_ref     = 0
             v_val_tip_ref = 0
             v_qti_ref     = 0.

      FIND FIRST tip_refeicao NO-LOCK
           WHERE tip_refeicao.cdn_tip_refeicao = tt-dados.cdn_tip_ref NO-ERROR.
      IF AVAIL tip_refeicao THEN
         ASSIGN v_val_ref = tip_refeicao.val_refeicao.
   END.

   ASSIGN v_num_linha   = v_num_linha + 1
          v_qti_ref     = v_qti_ref + 1
          v_val_tip_ref = v_val_tip_ref + v_val_ref.

   chWSheet:Range("A" + STRING(v_num_linha)):Value = STRING(tt-dados.cdn_tip_ref).
   chWSheet:Range("B" + STRING(v_num_linha)):Value = tt-dados.des_tip_ref.
   chWSheet:Range("C" + STRING(v_num_linha)):NumberFormat = "@".
   chWSheet:Range("C" + STRING(v_num_linha)):Value = STRING(tt-dados.dat_batida,"99/99/9999").
   chWSheet:Range("D" + STRING(v_num_linha)):Value = STRING(tt-dados.hra_batida,"HH:MM:SS").

   FIND FIRST disposit_control_aces NO-LOCK
        WHERE disposit_control_aces.cod_ender_disposit = tt-dados.cod_ip_disp NO-ERROR.
   IF AVAIL disposit_control_aces THEN
      chWSheet:Range("E" + STRING(v_num_linha)):Value = STRING(disposit_control_aces.cdn_disposit_control_aces) + " - " + 
                                                        disposit_control_aces.des_disposit_control_aces.

   chWSheet:Range("F" + STRING(v_num_linha)):Value = tt-dados.cdn_empresa.
   chWSheet:Range("G" + STRING(v_num_linha)):Value = tt-dados.cdn_estab.
   chWSheet:Range("H" + STRING(v_num_linha)):Value = STRING(tt-dados.cdn_funcionario).
   chWSheet:Range("I" + STRING(v_num_linha)):Value = tt-dados.nom_funcionario.
   chWSheet:Range("J" + STRING(v_num_linha)):Value = tt-dados.cod_rh_ccusto.
   chWSheet:Range("A" + STRING(v_num_linha) + ":J" + STRING(v_num_linha)):Borders({&xlEdgeRightLeftTopBottom}):LineStyle = 1.

   IF LAST-OF(tt-dados.cdn_tip_ref) THEN DO:
      CREATE tt-resumo.
      ASSIGN tt-resumo.cdn_tip_ref = tt-dados.cdn_tip_ref
             tt-resumo.des_tip_ref = tt-dados.des_tip_ref
             tt-resumo.val_ref     = v_val_ref
             tt-resumo.qti_ref     = v_qti_ref
             tt-resumo.val_tot_ref = v_val_tip_ref.
   END.
END.

IF VALID-OBJECT(chWSheet)      THEN DELETE OBJECT chWSheet.
chWSheet = chWBook:Sheets:Item().
chWSheet = chWBook:Sheets:Add().
chExcel:Workbooks:Item(1):Sheets(1):Name = "Resumo".

chWSheet:Range("A1"):Value = "Tipo Refei‡Æo".
chWSheet:Range("B1"):Value = "Descri‡Æo".
chWSheet:Range("C1"):Value = "Valor Unit rio".
chWSheet:Range("D1"):Value = "Quantidade Ref.".
chWSheet:Range("E1"):Value = "Valor Total".
chWSheet:Range("A1:E1"):Font:Bold = TRUE.
chWSheet:Range("A1:E1"):Borders({&xlEdgeRightLeftTopBottom}):LineStyle = 1.

ASSIGN v_num_linha = 1.

FOR EACH tt-resumo:
   ASSIGN v_num_linha = v_num_linha + 1.

   chWSheet:Range("A" + STRING(v_num_linha)):Value = STRING(tt-resumo.cdn_tip_ref).
   chWSheet:Range("B" + STRING(v_num_linha)):Value = tt-resumo.des_tip_ref.
   chWSheet:Range("C" + STRING(v_num_linha)):Value = STRING(tt-resumo.val_ref,"zz9.99").
   chWSheet:Range("D" + STRING(v_num_linha)):Value = STRING(tt-resumo.qti_ref,">>>>9").
   chWSheet:Range("E" + STRING(v_num_linha)):Value = STRING(tt-resumo.val_tot_ref,"zzz,zz9.99").
END.

chExcel:Visible = TRUE.

IF VALID-OBJECT(chWSheet)      THEN DELETE OBJECT chWSheet.
IF VALID-OBJECT(chWSheetRange) THEN DELETE OBJECT chWSheetRange.
IF VALID-OBJECT(chWBook)       THEN DELETE OBJECT chWBook.
IF VALID-OBJECT(chExcel)       THEN DELETE OBJECT chExcel.

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.

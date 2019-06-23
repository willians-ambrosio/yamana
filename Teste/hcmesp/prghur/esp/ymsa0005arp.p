/*------------------------------------------------------------------------------
  Purpose...: Sorteio Aleatorio para Revista
  Parameters:  <none>
  Notes.....: HCM Consulting / Outubro-2017
------------------------------------------------------------------------------*/
{include/i-prgvrs.i YMSA0005ARP 1.00.00.000}

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-func NO-UNDO
   FIELD cdn_empresa        LIKE funcionario.cdn_empresa
   FIELD cdn_estab          LIKE funcionario.cdn_estab
   FIELD cdn_funcionario    LIKE funcionario.cdn_funcionario
   FIELD cod_pis            AS CHARACTER FORMAT "999999999999"
   FIELD id_tsa             AS CHARACTER
   INDEX idprim AS UNIQUE PRIMARY cdn_empresa cdn_estab cdn_funcionario.

DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEFINE VARIABLE h-acomp             AS HANDLE                           NO-UNDO.
DEFINE VARIABLE Cnxn0               AS COM-HANDLE                       NO-UNDO.
DEFINE VARIABLE rs0                 AS COM-HANDLE                       NO-UNDO.
DEFINE VARIABLE ObjCommand0         AS COM-HANDLE                       NO-UNDO.
DEFINE VARIABLE odbc-query          AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE ado-null            AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE ado-recordcont      AS INTEGER                          NO-UNDO.
DEFINE VARIABLE ado-registro        AS INTEGER                          NO-UNDO.

DEFINE VARIABLE v_num_aux           AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_cdn_aux           AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_num_random        AS INTEGER                          NO-UNDO.
DEFINE VARIABLE v_cod_date          AS CHARACTER                        NO-UNDO.

FIND FIRST func_revista EXCLUSIVE-LOCK
     WHERE func_revista.dat_referencia = TODAY NO-ERROR.
IF AVAIL func_revista THEN DO:

   /* Busca dados TSA */
   CREATE "ADODB.Connection" Cnxn0.
   CREATE "ADODB.RecordSet"  rs0.
   CREATE "ADODB.Command"    ObjCommand0.

   /*
   ASSIGN Cnxn0:ConnectionString = "Server=YDMSQLTESTE\TSA;Provider=SQLNCLI10;Server=YDMSQLTESTE;User Id=SVC.TSA;Password=1325YOK@;".
   */
   ASSIGN Cnxn0:ConnectionString = "Server=YDMPRIMA02\TSA;Provider=SQLNCLI10;Server=YDMPRIMA02;User Id=SVC.TSA;Password=9872HLK%;".

   Cnxn0:OPEN(,,,) NO-ERROR.
   IF (ERROR-STATUS:NUM-MESSAGES > 0) THEN
       RETURN "NOK":U.

   DO v_num_aux = 1 TO func_revista.qti_func_cadastro:
      CREATE tt-func.
      ASSIGN tt-func.cdn_empresa     = func_revista.cdn_empr_func[v_num_aux]
             tt-func.cdn_estab       = func_revista.cdn_estab_func[v_num_aux]
             tt-func.cdn_funcionario = func_revista.cdn_matr_func[v_num_aux].

      FIND FIRST funcionario OF tt-func NO-LOCK NO-ERROR.
      IF AVAIL funcionario THEN
         ASSIGN tt-func.cod_pis = STRING(DECIMAL(funcionario.cod_pis),"999999999999").
   END.

   IF func_revista.qti_func_cadastro < func_revista.qti_funcionarios THEN DO:
      /* Pega maior matricula ativa no sistema */
      ASSIGN v_cdn_aux = 0.
      FOR EACH funcionario USE-INDEX fncnr_matrbase NO-LOCK
         WHERE funcionario.dat_desligto_func <> ?:
         IF funcionario.cdn_funcionario > v_cdn_aux THEN
            ASSIGN v_cdn_aux = funcionario.cdn_funcionario.
      END.

      trans_01:
      DO v_num_aux = 1 TO (func_revista.qti_funcionarios - func_revista.qti_func_cadastro):
         /* Atribui numero randomico */
         ASSIGN v_num_random = RANDOM(1,v_cdn_aux).

         trans_02:
         REPEAT:

            IF v_num_random > v_cdn_aux THEN
               ASSIGN v_num_random = RANDOM(1,v_cdn_aux).

            IF CAN-FIND(FIRST funcionario
                        WHERE funcionario.cdn_funcionario   = v_num_random
                          AND funcionario.dat_desligto_func = ?) THEN DO:
               FIND FIRST funcionario NO-LOCK
                    WHERE funcionario.cdn_funcionario   = v_num_random
                      AND funcionario.dat_desligto_func = ? NO-ERROR.
               IF AVAIL funcionario THEN DO:
                  IF NOT CAN-FIND(FIRST tt-func
                                  WHERE tt-func.cdn_empresa     = funcionario.cdn_empresa
                                    AND tt-func.cdn_estab       = funcionario.cdn_estab
                                    AND tt-func.cdn_funcionario = funcionario.cdn_funcionario) THEN DO:
                     CREATE tt-func.
                     ASSIGN tt-func.cdn_empresa     = funcionario.cdn_empresa
                            tt-func.cdn_estab       = funcionario.cdn_estab
                            tt-func.cdn_funcionario = funcionario.cdn_funcionario
                            tt-func.cod_pis         = STRING(DECIMAL(funcionario.cod_pis),"999999999999").

                     ASSIGN func_revista.qti_func_cadastro                              = func_revista.qti_func_cadastro + 1
                            func_revista.cdn_empr_func[func_revista.qti_func_cadastro]  = funcionario.cdn_empresa
                            func_revista.cdn_estab_func[func_revista.qti_func_cadastro] = funcionario.cdn_estab
                            func_revista.cdn_matr_func[func_revista.qti_func_cadastro]  = funcionario.cdn_funcionario.

                     LEAVE trans_02.
                  END.
                  ELSE DO:
                     ASSIGN v_num_random = v_num_random + 1.
                     NEXT trans_02.
                  END.
               END.
            END.
            ELSE DO:
               ASSIGN v_num_random = v_num_random + 1.
               NEXT trans_02.
            END.

         END. /* REPEAT */
      END. /* DO = 1 */
   END.

   ASSIGN v_cod_date = STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" +
                       STRING(DAY(TODAY),"99") + " 00:00:00".

   FOR EACH tt-func:

      ASSIGN odbc-query = "select ID" + 
                          "  from USER_SA" +
                          " where USER_SA.PIS = '" + tt-func.cod_pis + "';".

      ObjCommand0:ActiveConnection = Cnxn0.
      ObjCommand0:CommandText      = odbc-query.
      objCommand0:CommandType      = 1.
      Cnxn0:CursorLocation         = 3.
      rs0:CursorType               = 1.
      rs0:LockType                 = 3.
      rs0                          = ObjCommand0:EXECUTE(OUTPUT ado-null,"",32).
      ado-recordcont               = rs0:RecordCount.

      rs0:MoveFirst NO-ERROR.
      IF rs0:FIELDS("ID"):VALUE <> "" AND
         rs0:FIELDS("ID"):VALUE <> ?  THEN
         ASSIGN tt-func.id_tsa = rs0:FIELDS("ID"):VALUE.

      IF tt-func.id_tsa <> "" THEN DO:
         ASSIGN odbc-query = "insert into EXCEPTION (ID_USER_SA,DIRECTION,INITIAL_DATE,END_DATE,EXCEPTION_TYPE,OBSERVATION,ORIGIN,CREATED_USER)" +
                             "     values ('" + tt-func.id_tsa + "','0'," + v_cod_date + "','" + v_cod_date + "','1','Bloq. Revista RPW','0','adm');".

         ObjCommand0:ActiveConnection = Cnxn0.
         ObjCommand0:CommandText      = odbc-query.
         objCommand0:CommandType      = 1.
         Cnxn0:CursorLocation         = 3.
         rs0:CursorType               = 1.
         rs0:LockType                 = 3.
         rs0                          = ObjCommand0:EXECUTE(OUTPUT ado-null,"",32).
         ado-recordcont               = rs0:RecordCount.
      END.
   END. /* EACH tt-func */

END.

ObjCommand0:CLOSE           NO-ERROR.
RELEASE OBJECT Cnxn0        NO-ERROR.
RELEASE OBJECT ObjCommand0  NO-ERROR.
RELEASE OBJECT rs0          NO-ERROR.

ASSIGN Cnxn0       = ?
       ObjCommand0 = ?
       rs0         = ?.

RETURN "OK":U.

/************************************************************************
* Programa..: ESAU002RP.P 
* Descri‡Æo.: 
* AUTOR.....: Bruno Bertulli (DSC)
* DATA......: 06/07/2013
************************************************************************/

/* include de controle de vers’o */
{include/i-prgvrs.i ESOF004RP 2.06.00.001}

/* prýprocessador para ativar ou n’o a saðda para RTF */
&GLOBAL-DEFINE RTF NO

/* prýprocessador para setar o tamanho da p˜gina */
&SCOPED-DEFINE pagesize 0   
/* defini¯Êo das temp-tables para recebimento de parümetros */

define temp-table tt-param no-undo
    field destino          as integer
    field c-destino        as char 
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD cgc-ini          LIKE emitente.cgc
    FIELD cgc-fim          LIKE emitente.cgc
    FIELD cod-emitente-ini LIKE emitente.cod-emitente
    FIELD cod-emitente-fim LIKE emitente.cod-emitente
    FIELD nome-abrev-ini   LIKE emitente.nome-abrev        
    FIELD nome-abrev-fim   LIKE emitente.nome-abrev        
    FIELD cod-gr-forn-ini  LIKE emitente.cod-gr-forn
    FIELD cod-gr-forn-fim  LIKE emitente.cod-gr-forn
    FIELD dt-ini           AS DATE
    FIELD dt-fim           AS DATE
    FIELD gera-excel       AS LOGICAL
    .

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de parümetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

DEFINE VARIABLE i-cont              AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-des-val-ant       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-des-val-nov       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chExcelApplication  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkItem          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cCampos             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp             AS HANDLE     NO-UNDO.

DEFINE TEMP-TABLE tt-relatorio
    FIELD cod_usuario       LIKE tabela_vrf_monitor.cod_usuario FORMAT "X(10)"
    FIELD dat_atualiz       LIKE tabela_vrf_monitor.dat_atualiz
    FIELD hra_atualiz       LIKE tabela_vrf_monitor.hra_atualiz
    FIELD cod_chave         LIKE tabela_vrf_monitor.cod_chave
    FIELD cod-banco-ant     AS CHAR FORMAT "X(08)"
    FIELD cod-banco-nov     AS CHAR FORMAT "X(08)"
    FIELD agencia-ant       AS CHAR FORMAT "X(10)"
    FIELD agencia-nov       AS CHAR FORMAT "X(10)"
    FIELD conta-corren-ant  AS CHAR FORMAT "X(15)"
    FIELD conta-corren-nov  AS CHAR FORMAT "X(15)"
    FIELD ins-municipal-ant AS CHAR FORMAT "X(15)"
    FIELD ins-municipal-nov AS CHAR FORMAT "X(15)"
    FIELD ins-estadual-ant  AS CHAR FORMAT "X(15)"
    FIELD ins-estadual-nov  AS CHAR FORMAT "X(15)"
    FIELD cgc               LIKE emitente.cgc               FORMAT "X(15)"
    FIELD cod-emitente      LIKE emitente.cod-emitente
    FIELD nome-abrev        LIKE emitente.nome-abrev        
    FIELD nome-emit         LIKE emitente.nome-emit         FORMAT "X(40)"
    FIELD cod-gr-forn       LIKE emitente.cod-gr-forn
    FIELD nom_usuario       LIKE usuar_mestre.nom_usuario   FORMAT "X(40)"
    FIELD num-pedido        LIKE pedido-compr.num-pedido    INITIAL 0
    FIELD data-pedido       LIKE pedido-compr.data-pedido   INITIAL ?
    .

/* ===> Main Block <=== */

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp (INPUT "Processando").

ASSIGN cCampos = "cod-banco,agencia,conta-corren,ins-municipal,ins-estadual".

FIND FIRST tt-param NO-ERROR.
    
FOR EACH tabela_vrf_monitor NO-LOCK
    WHERE tabela_vrf_monitor.cod_base_dados = "ems2cadme"
    AND   tabela_vrf_monitor.cod_tabela     = "emitente"
    AND   tabela_vrf_monitor.dat_atualiz   >= tt-param.dt-ini
    AND   tabela_vrf_monitor.dat_atualiz   <= tt-param.dt-fim
    AND   tabela_vrf_monitor.cod_evento     = "W"
    ,EACH atrib_vrf_monitor OF tabela_vrf_monitor
        BREAK BY tabela_vrf_monitor.cod_base_dados
              BY tabela_vrf_monitor.cod_tabela
              BY atrib_vrf_monitor.cod_atributo
              BY tabela_vrf_monitor.dat_atualiz
              BY tabela_vrf_monitor.hra_atualiz:

    IF INDEX (ccampos,atrib_vrf_monitor.cod_atributo) = 0 THEN NEXT.

    RUN pi-acompanhar IN h-acomp (INPUT "Processando : " + STRING (tabela_vrf_monitor.dat_atualiz)).

    FIND FIRST tt-relatorio
        WHERE tt-relatorio.cod_usuario  = tabela_vrf_monitor.cod_usuario
        AND   tt-relatorio.dat_atualiz  = tabela_vrf_monitor.dat_atualiz
        AND   tt-relatorio.hra_atualiz  = tabela_vrf_monitor.hra_atualiz
        AND   tt-relatorio.cod_chave    = tabela_vrf_monitor.cod_chave NO-ERROR.
    IF NOT AVAILABLE tt-relatorio THEN DO:
        CREATE tt-relatorio.
        ASSIGN tt-relatorio.cod_usuario  = tabela_vrf_monitor.cod_usuario
               tt-relatorio.dat_atualiz  = tabela_vrf_monitor.dat_atualiz
               tt-relatorio.hra_atualiz  = tabela_vrf_monitor.hra_atualiz
               tt-relatorio.cod_chave    = tabela_vrf_monitor.cod_chave
               .

        FIND FIRST usuar_mestre NO-LOCK 
            WHERE usuar_mestre.cod_usuario = tt-relatorio.cod_usuario NO-ERROR.
        IF AVAILABLE usuar_mestre THEN
            ASSIGN tt-relatorio.nom_usuario = usuar_mestre.nom_usuario.

    END.

    FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = INTEGER (tt-relatorio.cod_chave) NO-ERROR.
    IF AVAILABLE emitente THEN DO:
        ASSIGN tt-relatorio.cgc        = emitente.cgc
               tt-relatorio.cod-emitente = emitente.cod-emitente
               tt-relatorio.nome-abrev = emitente.nome-abrev
               tt-relatorio.nome-emit  = emitente.nome-emit
               tt-relatorio.nome-emit  = replace(tt-relatorio.nome-emit,CHR(10),"")
               tt-relatorio.nome-emit  = replace(tt-relatorio.nome-emit,CHR(11),"")
               tt-relatorio.nome-emit  = replace(tt-relatorio.nome-emit,CHR(12),"")
               tt-relatorio.nome-emit  = replace(tt-relatorio.nome-emit,CHR(13),"")
               tt-relatorio.nome-emit  = replace(tt-relatorio.nome-emit,";","")
               tt-relatorio.nome-emit  = substring(tt-relatorio.nome-emit,1,100).
               tt-relatorio.cod-gr-for = emitente.cod-gr-for
               .

           FOR LAST pedido-compr NO-LOCK
               WHERE pedido-compr.cod-emitente = emitente.cod-emitente:
               ASSIGN tt-relatorio.num-pedido  = pedido-compr.num-pedido
                      tt-relatorio.data-pedido = pedido-compr.data-pedido.
           END.
    END.

    IF atrib_vrf_monitor.cod_atributo = "cod-banco" THEN DO:
        ASSIGN c-des-val-ant = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_ant,chr(254)):
            ASSIGN c-des-val-ant = c-des-val-ant + ENTRY(i-cont,atrib_vrf_monitor.des_val_ant,chr(254)).
            ASSIGN tt-relatorio.cod-banco-ant = c-des-val-ant.
        END.
    
        ASSIGN c-des-val-nov = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_nov,chr(254)):
            ASSIGN c-des-val-nov = c-des-val-nov + ENTRY(i-cont,atrib_vrf_monitor.des_val_nov,chr(254)).
            ASSIGN tt-relatorio.cod-banco-nov = c-des-val-nov.
        END.
    END.

    IF atrib_vrf_monitor.cod_atributo = "agencia" THEN DO:
        ASSIGN c-des-val-ant = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_ant,chr(254)):
            ASSIGN c-des-val-ant = c-des-val-ant + ENTRY(i-cont,atrib_vrf_monitor.des_val_ant,chr(254)).
            ASSIGN tt-relatorio.agencia-ant = c-des-val-ant.
        END.
    
        ASSIGN c-des-val-nov = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_nov,chr(254)):
            ASSIGN c-des-val-nov = c-des-val-nov + ENTRY(i-cont,atrib_vrf_monitor.des_val_nov,chr(254)).
            ASSIGN tt-relatorio.agencia-nov = c-des-val-nov.
        END.
    END.

    IF atrib_vrf_monitor.cod_atributo = "conta-corren" THEN DO:
        ASSIGN c-des-val-ant = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_ant,chr(254)):
            ASSIGN c-des-val-ant = c-des-val-ant + ENTRY(i-cont,atrib_vrf_monitor.des_val_ant,chr(254)).
            ASSIGN tt-relatorio.conta-corren-ant = c-des-val-ant.
        END.
    
        ASSIGN c-des-val-nov = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_nov,chr(254)):
            ASSIGN c-des-val-nov = c-des-val-nov + ENTRY(i-cont,atrib_vrf_monitor.des_val_nov,chr(254)).
            ASSIGN tt-relatorio.conta-corren-nov = c-des-val-nov.
        END.
    END.

    IF atrib_vrf_monitor.cod_atributo = "ins-municipal" THEN DO:
        ASSIGN c-des-val-ant = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_ant,chr(254)):
            ASSIGN c-des-val-ant = c-des-val-ant + ENTRY(i-cont,atrib_vrf_monitor.des_val_ant,chr(254)).
            ASSIGN tt-relatorio.ins-municipal-ant = c-des-val-ant.
        END.
    
        ASSIGN c-des-val-nov = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_nov,chr(254)):
            ASSIGN c-des-val-nov = c-des-val-nov + ENTRY(i-cont,atrib_vrf_monitor.des_val_nov,chr(254)).
            ASSIGN tt-relatorio.ins-municipal-nov = c-des-val-nov.
        END.
    END.

    IF atrib_vrf_monitor.cod_atributo = "ins-estadual" THEN DO:
        ASSIGN c-des-val-ant = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_ant,chr(254)):
            ASSIGN c-des-val-ant = c-des-val-ant + ENTRY(i-cont,atrib_vrf_monitor.des_val_ant,chr(254)).
            ASSIGN tt-relatorio.ins-estadual-ant = c-des-val-ant.
        END.
    
        ASSIGN c-des-val-nov = "".
        DO i-cont = 1 TO NUM-ENTRIES(atrib_vrf_monitor.des_val_nov,chr(254)):
            ASSIGN c-des-val-nov = c-des-val-nov + ENTRY(i-cont,atrib_vrf_monitor.des_val_nov,chr(254)).
            ASSIGN tt-relatorio.ins-estadual-nov = c-des-val-nov.
        END.
    END.

END.

/*
OUTPUT TO VALUE(tt-param.arquivo) NO-CONVERT.
FOR EACH tt-relatorio NO-LOCK
    WHERE tt-relatorio.cgc          >= tt-param.cgc-ini
    AND   tt-relatorio.cgc          <= tt-param.cgc-fim
    AND   tt-relatorio.cod-emitente >= tt-param.cod-emitente-ini
    AND   tt-relatorio.cod-emitente <= tt-param.cod-emitente-fim
    AND   tt-relatorio.nome-abrev   >= tt-param.nome-abrev-ini
    AND   tt-relatorio.nome-abrev   <= tt-param.nome-abrev-fim
    AND   tt-relatorio.cod-gr-forn  >= tt-param.cod-gr-forn-ini
    AND   tt-relatorio.cod-gr-forn  <= tt-param.cod-gr-forn-fim
    BREAK BY tt-relatorio.dat_atualiz
          BY tt-relatorio.hra_atualiz:

    DISP tt-relatorio.cod_usuario       COLUMN-LABEL "Usuario" 
         tt-relatorio.nom_usuario       COLUMN-LABEL "Nome Usuario" 
         tt-relatorio.dat_atualiz       COLUMN-LABEL "Data Alt." 
         tt-relatorio.hra_atualiz       COLUMN-LABEL "Hora Alt." 
         tt-relatorio.cgc               COLUMN-LABEL "CGC / CPF" 
         tt-relatorio.nome-abrev        COLUMN-LABEL "Nome Abrev" 
         tt-relatorio.nome-emit         COLUMN-LABEL "Nome Emitente" FORMAT "X(40)"
         tt-relatorio.cod-banco-ant     COLUMN-LABEL "Banco Ant" 
         tt-relatorio.cod-banco-nov     COLUMN-LABEL "Banco Nov" 
         tt-relatorio.agencia-ant       COLUMN-LABEL "Ag. Ant" 
         tt-relatorio.agencia-nov       COLUMN-LABEL "Ag. Nov" 
         tt-relatorio.conta-corren-ant  COLUMN-LABEL "Cta Corren Ant" 
         tt-relatorio.conta-corren-nov  COLUMN-LABEL "Cta Corren Nov" 
         tt-relatorio.ins-municipal-ant COLUMN-LABEL "I.M. Ant" 
         tt-relatorio.ins-municipal-nov COLUMN-LABEL "I.M. Nov" 
         tt-relatorio.ins-estadual-ant  COLUMN-LABEL "I.E. Ant" 
         tt-relatorio.ins-estadual-nov  COLUMN-LABEL "I.E. Nov" 
         WITH WIDTH 320.
END.
OUTPUT CLOSE.
*/

IF tt-param.gera-excel = YES THEN DO:

    OUTPUT TO VALUE(tt-param.arquivo) NO-CONVERT.

    PUT UNFORMAT
        "Usuario"
        ";Nome Usuario"
        ";Data Alt."
        ";Hora Alt."
        ";CGC / CPF"
        ";Cod Emitente"
        ";Nome Abrev"
        ";Grupo Forn"
        ";Nome Emitente"
        ";Banco Ant"
        ";Banco Nov"
        ";Ag. Ant"
        ";Ag. Nov"
        ";Cta Corren Ant"
        ";Cta Corren Nov"
        ";I.M. Ant"
        ";I.M. Nov"
        ";I.E. Ant"
        ";I.E. Nov"
        ";Dt. Pedido"
        ";Num Pedido".
    PUT SKIP.

    FOR EACH tt-relatorio NO-LOCK
        WHERE tt-relatorio.cgc          >= tt-param.cgc-ini
        AND   tt-relatorio.cgc          <= tt-param.cgc-fim
        AND   tt-relatorio.cod-emitente >= tt-param.cod-emitente-ini
        AND   tt-relatorio.cod-emitente <= tt-param.cod-emitente-fim
        AND   tt-relatorio.nome-abrev   >= tt-param.nome-abrev-ini
        AND   tt-relatorio.nome-abrev   <= tt-param.nome-abrev-fim
        AND   tt-relatorio.cod-gr-forn  >= tt-param.cod-gr-forn-ini
        AND   tt-relatorio.cod-gr-forn  <= tt-param.cod-gr-forn-fim
        BREAK BY tt-relatorio.dat_atualiz
              BY tt-relatorio.hra_atualiz:

        RUN pi-acompanhar IN h-acomp (INPUT "Imprimindo : " + STRING (tt-relatorio.cod-emitente)).

        PUT UNFORMAT
             tt-relatorio.cod_usuario 
             ";" tt-relatorio.nom_usuario
             ";" tt-relatorio.dat_atualiz 
             ";" tt-relatorio.hra_atualiz FORMAT "99:99:99".

        IF LENGTH (TRIM (tt-relatorio.cgc)) = 11 THEN
            PUT UNFORMAT ";" tt-relatorio.cgc FORMAT "999.999.999-22".
        ELSE
            PUT UNFORMAT ";" tt-relatorio.cgc FORMAT "99.999.999/9999-99".

        PUT UNFORMAT
             ";" tt-relatorio.cod-emitente
             ";" tt-relatorio.nome-abrev 
             ";" tt-relatorio.cod-gr-forn
             ";" tt-relatorio.nome-emit 
             ";" tt-relatorio.cod-banco-ant
             ";" tt-relatorio.cod-banco-nov
             ";" tt-relatorio.agencia-ant
             ";" tt-relatorio.agencia-nov
             ";" tt-relatorio.conta-corren-ant
             ";" tt-relatorio.conta-corren-nov
             ";" tt-relatorio.ins-municipal-ant
             ";" tt-relatorio.ins-municipal-nov
             ";" tt-relatorio.ins-estadual-ant
             ";" tt-relatorio.ins-estadual-nov
             ";" (IF tt-relatorio.data-pedido = ? THEN "" ELSE STRING (tt-relatorio.data-pedido,"99/99/9999"))
             ";" tt-relatorio.num-pedido
            .
        PUT SKIP.
    END.
    OUTPUT CLOSE.

    RUN pi-geraexcel (INPUT tt-param.arquivo).

END.


RUN pi-finalizar IN h-acomp.


/* ===> Procedures <=== */

PROCEDURE Pi-GeraExcel:
    DEFINE INPUT PARAM p-arquivo AS CHARACTER   NO-UNDO.

    CREATE "Excel.Application" chExcelApplication.
    chExcelApplication:Visible = true.
    chExcelApplication:Workbooks:OpenText(p-arquivo, , , , , , TRUE ).
    chWorkSheet = chExcelApplication:Sheets:Item(1).
/*     chWorkSheet:rows("1:1"):Select().        */
/*     chExcelApplication:selection:INSERT(1).  */
                                                           
    /* Altera a largura da coluna de acordo com o tamanho do seu conteudo */
    chExcelApplication:Cells:Select.
    chExcelApplication:Cells:EntireColumn:AutoFit.

    chWorkSheet:Range("A2"):Select(). 

/* *******************************/                                                     

    RELEASE OBJECT chExcelApplication.
/*     RELEASE OBJECT chWorkbook. */
    RELEASE OBJECT chWorksheet.

END PROCEDURE.


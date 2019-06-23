/****************************************************************************************** 
** 	   Programa: rep004-rp.p
**   	      Autor: Vando Ribeiro
**   	 Fornecedor: Grupo DKP
**    	 Data: 29/10/2018
** Change/Chamado: 
**    Objetivo: Enviar e-mail com prazo de pagamento inferior para grupo financeiro
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: tt-param
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
/* include de controle de vers∆o */
{include/i-prgvrs.i rep004-rp 0.12.00.000}
{utp/utapi019.i}
/* preprocessador para ativar ou n∆o a saida para rtf */
&GLOBAL-DEFINE XLS YES

/* preprocessador para setar o tamanho da pagina */
&SCOPED-DEFINE pagesize 45

/* definiá∆o das temp-tables para recebimento de parametros */
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

/* tabela aux */
DEFINE TEMP-TABLE tt-mes_estab NO-UNDO
    FIELD mensagem        AS CHAR FORMAT "x(50000)"
    FIELD cod-estab       AS CHAR.


DEFINE TEMP-TABLE tt-envio-relat NO-UNDO
     field cod-estabel    like dupli-apagar.cod-estabel  
     field serie-docto    like dupli-apagar.serie-docto  
     field nro-docto      like dupli-apagar.nro-docto    
     field cod-emitente   like dupli-apagar.cod-emitente 
     field nat-operacao   like dupli-apagar.nat-operacao 
     field nome-cli-for   AS CHARACTER                                                    
     field vl-a-pagar     AS DECIMAL FORMAT ">>>>>,>>>,>>9.99"
     field dt-emissao     like dupli-apagar.dt-emissao                                         
     field dt-vencim      like dupli-apagar.dt-vencim                                          
     field data-geracao   like esp_hist_aprov.data-geracao 
     field Dias           like esp_hist_aprov.Dias                                           
     field nom_usuario    like usuar_mestre.nom_usuario.       

DEFINE BUFFER b-tt-mes_estab FOR tt-mes_estab.

DEFINE VARIABLE c-mess-top  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess      AS CHARACTER FORMAT "x(50000)"  NO-UNDO.
DEFINE VARIABLE c-mess-base AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-ini      AS DATE        NO-UNDO.
DEFINE VARIABLE dt-fim      AS DATE        NO-UNDO.
DEFINE VARIABLE i-mes       AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-ano       AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod_usuario AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita  AS RAW.

/*recebimento de parametros*/
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/*para executar via RPW n∆o pode aceitar os valores recebidos nos canpos da tt-param*/

/*include padr∆o para variaveis de relatorio*/
{include/i-rpvar.i}

/* definiá∆o de variaveis */
DEF VAR h-acomp AS HANDLE NO-UNDO.

/* include padrao para output de realtorios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a definicao da frame de cabeáalho e rodape */
{include/i-rpcab.i &STREAM="str-rp"}

/*bloco principal do programa */
ASSIGN c-programa  = "rep004-rp"
       c-versao    = "1.00"
       c-revisao   = ".00.000"
       c-empresa   = "Yamana"
       c-sistema   = "REP"
       c-titulo-relat = "Envio de Email".

/* para visualizar cabeáalho/rodape em saida */
VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

/* executando de forma persistente o utilitario de acompanhamento */                                                               
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.                                                                                         
/* {utp/ut-liter.i Imprimindo *} */
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).                                                                                

/* corpo do relatorio */  
FIND FIRST tt-param NO-LOCK NO-ERROR.                                                                                                                                
FIND FIRST param_email NO-LOCK NO-ERROR.
EMPTY TEMP-TABLE tt-envio2.
EMPTY TEMP-TABLE tt-mensagem.

RUN utp/utapi019.p PERSISTENT SET h-utapi019.

c-mess-top = "<html>"                                                                          
             + "<head>" 
             + "<style>"
             + "table, th, td"
             + "~{"
             + "border: 1px solid black;"
             + "border-collapse: collapse;"
             + "text-align: CENTER;"
             + "}"
             + "</style>"
             + "</head>"                                                                      
             + "<body>"
             + "<p>Prezado(a)(s),<p>"
             + "<p> Segue abaixo a lista das notas atualizadas no recebimento com prazo para pagamento inferior aos valores parametrizados no Cadastro de parÉmetros de bloqueio.</p>"
             + "<TABLE style=~"width:100%~">"                                                                      
             + "<tr>"                                                                         
             + "<th>" + "Estab" + "</th>"                                                   
             + "<th>" + "Fornecedor" + "</th>"                                              
             + "<th>" + "Nota" + "</th>"                                                    
             + "<th>" + "Serie" + "</th>"
             + "<th>" + "Natureza" + "</th>"
             + "<th>" + "Valor" +  "</th>"                                                   
             + "<th>" + "Data Emiss∆o" + "</th>"                                            
             + "<th>" + "Data Vencimento" + "</th>"                                         
             + "<th>" + "Data da Atualizaá∆o" + "</th>"                                               
             + "<th>" + "Nr. Dias" + "</th>"                                                
             + "<th>" + "Usu†rio Digitaá∆o NF" + "</th>"                                    
             + "</tr>".

 c-mess-base = "</table>"                                                        
             + "<p>&nbsp;</p>"                                                   
             + "<p>" + "At," + "</p>"                                            
             + "<p>" + "<strong>" + "Sustená∆o Yamana." + "</strong>" + "</p>"   
             + "</body>"                                                         
             + "</html>" .                                                       


EMPTY TEMP-TABLE tt-envio-relat.

FOR EACH esp_param_bloq NO-LOCK BY esp_param_bloq.cod-estabel:

    IF NOT esp_param_bloq.env-relat THEN NEXT.


    CASE esp_param_bloq.freq-envio:
        WHEN 1 THEN DO: /*frequencia diaria*/

            ASSIGN dt-ini = IF WEEKDAY(TODAY) = 2 THEN TODAY - 3 ELSE TODAY - 1 /*se for segunda a data inicia na sexta senao inicia no dia anterior*/
                   dt-fim = TODAY - 1.

        END.
        WHEN 2 THEN DO: /*frequencia semanal*/

            RUN pi-semana (INPUT  TODAY,
                           OUTPUT dt-ini,
                           OUTPUT dt-fim).
        END.
        OTHERWISE /*frequencia mensal*/
            DO:
                i-mes = MONTH(DATE(STRING(TODAY + 32 - DAY(TODAY + 32), "99/99/9999"))) - 1.
                i-ano = IF MONTH(TODAY) = 1 THEN YEAR(TODAY) - 1 ELSE YEAR(TODAY).

                dt-ini = DATE("01/" + STRING(i-mes, "99") + "/" + STRING(i-ano)).
                dt-fim = DATE(STRING(dt-ini + 32 - DAY(dt-ini + 32), "99/99/9999")).
            END.
    END CASE.
  
    FOR EACH esp_hist_aprov WHERE
        DATE(esp_hist_aprov.data-geracao) >=  dt-ini AND 
        DATE(esp_hist_aprov.data-geracao) <=  dt-fim EXCLUSIVE-LOCK BREAK BY esp_hist_aprov.cod-estabel:

        FIND FIRST dupli-apagar NO-LOCK 
             WHERE dupli-apagar.cod-estabel   =  esp_hist_aprov.cod-estabel 
               AND dupli-apagar.serie-docto   =  esp_hist_aprov.serie-docto                
               AND dupli-apagar.nro-docto     =  esp_hist_aprov.nro-docto                  
               AND dupli-apagar.cod-emitente  =  esp_hist_aprov.cod-emitente               
               AND dupli-apagar.nat-operacao  =  esp_hist_aprov.nat-operacao NO-ERROR.
        IF NOT AVAIL dupli-apagar THEN NEXT.

        FIND usuar_mestre WHERE
             usuar_mestre.cod_usuario = esp_hist_aprov.cod_usuario_geracao NO-LOCK NO-ERROR.
        IF NOT AVAIL usuar_mestre THEN NEXT.

        FIND FIRST emitente WHERE emitente.cod-emitente = esp_hist_aprov.cod-emitente NO-LOCK NO-ERROR.
        IF NOT AVAIL emitente THEN NEXT.

        /* Begins: 21/01/2019 - Willians Ambrosio - DKP - Regra revisada e ajustada, para evitar duplicidades na geraá∆o */
        FIND FIRST tt-envio-relat WHERE
                   tt-envio-relat.cod-estabel  = dupli-apagar.cod-estabel    AND
                   tt-envio-relat.serie-docto  = dupli-apagar.serie-docto    AND
                   tt-envio-relat.nro-docto    = dupli-apagar.nro-docto      AND
                   tt-envio-relat.cod-emitente = dupli-apagar.cod-emitente   AND
                   tt-envio-relat.nat-operacao = dupli-apagar.nat-operacao   EXCLUSIVE-LOCK NO-ERROR.                 
        IF NOT AVAIL tt-envio-relat THEN
        DO:
           CREATE tt-envio-relat.
           ASSIGN tt-envio-relat.cod-estabel  = dupli-apagar.cod-estabel    
                  tt-envio-relat.serie-docto  = dupli-apagar.serie-docto    
                  tt-envio-relat.nro-docto    = dupli-apagar.nro-docto      
                  tt-envio-relat.cod-emitente = dupli-apagar.cod-emitente   
                  tt-envio-relat.nat-operacao = dupli-apagar.nat-operacao   
                  tt-envio-relat.nome-cli-for = string(esp_hist_aprov.cod-emitente) + " - " + emitente.nome-abrev 
                  tt-envio-relat.nro-docto    = esp_hist_aprov.nro-docto  
                  tt-envio-relat.vl-a-pagar   = dupli-apagar.vl-a-pagar 
                  tt-envio-relat.dt-emissao   = dupli-apagar.dt-emissao 
                  tt-envio-relat.dt-vencim    = dupli-apagar.dt-vencim 
                  tt-envio-relat.data-geracao = esp_hist_aprov.data-geracao 
                  tt-envio-relat.Dias         = esp_hist_aprov.Dias 
                  tt-envio-relat.nom_usuario  = usuar_mestre.nom_usuario.   

           ASSIGN c-mess = "<tr>"                                                                                                     
                           + "<td>" + tt-envio-relat.cod-estabel                             + "</td>"                                              
                           + "<td>" + tt-envio-relat.nome-cli-for                            + "</td>"    
                           + "<td>" + tt-envio-relat.nro-docto                               + "</td>" 
                           + "<td>" + tt-envio-relat.serie-docto                             + "</td>"
                           + "<td>" + tt-envio-relat.nat-operacao                            + "</td>" 
                           + "<td>" + string(tt-envio-relat.vl-a-pagar, ">>>>>,>>>,>>9.99")  + "</td>"   
                           + "<td>" + string(tt-envio-relat.dt-emissao)                      + "</td>"                                         
                           + "<td>" + string(tt-envio-relat.dt-vencim)                       + "</td>"                                              
                           + "<td>" + string(tt-envio-relat.data-geracao,"99/99/9999 HH:MM") + "</td>"   
                           + "<td>" + string(tt-envio-relat.Dias)                            + "</td>"                                               
                           + "<td>" + tt-envio-relat.nom_usuario                             + "</td>"                                                              
                           + "</tr>". 
                  
           CREATE tt-mes_estab.
           ASSIGN tt-mes_estab.mensagem    = c-mess  
                  tt-mes_estab.cod-estab   = tt-envio-relat.cod-estabel.              
        END.
        /* End 21/01/2019 */

        ASSIGN esp_hist_aprov.email-preparado = YES.
    END.
END.

ASSIGN c-mess = "".
FOR EACH tt-mes_estab BY tt-mes_estab.cod-estab:
    c-mess = c-mess +  tt-mes_estab.mensagem.
END.

FOR EACH esp_hist_email NO-LOCK:

    ASSIGN c-cod_usuario = "".
    FIND usuar_mestre WHERE 
         usuar_mestre.cod_usuario = esp_hist_email.cod_usuario NO-LOCK NO-ERROR.
    IF AVAIL usuar_mestre THEN
        FIND FIRST usuar_mestre_ext NO-LOCK
             WHERE usuar_mestre_ext.cod_usuario = usuar_mestre.cod_usuario
               AND usuar_mestre_ext.cod_domin_so <> "" NO-ERROR.
        IF AVAIL usuar_mestre_ext THEN
            ASSIGN c-cod_usuario = usuar_mestre_ext.cod_usuar_so.

    IF c-mess <> "" THEN DO:

         EMPTY TEMP-TABLE tt-envio2.
         EMPTY TEMP-TABLE tt-mensagem.
         EMPTY TEMP-TABLE tt-erros.
             
        RUN pi-acompanhar IN h-acomp (INPUT "Enviando: " + TRIM(esp_hist_email.cod_usuario)).

        CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
               tt-envio2.exchange          = param_email.log_servid_exchange
               tt-envio2.servidor          = param_email.cod_servid_e_mail
               tt-envio2.porta             = param_email.num_porta
               tt-envio2.destino           = usuar_mestre.cod_e_mail_local
               tt-envio2.assunto           = "Notas atualizadas no recebimento - Prazo pagamento inferior"
               tt-envio2.remetente         = "SustencaoYamana@yamana.com"
               tt-envio2.copia             = ""
               tt-envio2.importancia       = 2
               tt-envio2.log-enviada       = YES
               tt-envio2.log-lida          = YES
               tt-envio2.acomp             = YES
               tt-envio2.formato           = "HTML".
        
        CREATE tt-mensagem.                                       
        ASSIGN tt-mensagem.seq-mensagem = 1                         
               tt-mensagem.mensagem     = c-mess-top + c-mess + c-mess-base.         
        
        RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                       INPUT  TABLE tt-mensagem,
                                       OUTPUT TABLE tt-erros).


        IF TEMP-TABLE tt-erros:HAS-RECORDS THEN
        DO:
            OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erro-email-rec-" + STRING(TODAY, "99999999") + "-" + STRING(ETIME, "99999999") + ".txt").
            FOR EACH tt-erros:
                DISP tt-erros WITH SCROLLABLE.
            END.
            OUTPUT CLOSE.
        END.
    END.
END.

DELETE PROCEDURE h-utapi019.


/* fechamento do output do relatorio */
{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


PROCEDURE pi-semana:

    DEFINE INPUT  PARAMETER p-dt-semana      AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER p-dt-ini-semana  AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER p-dt-fim-semana  AS DATE NO-UNDO.

    DEFINE VARIABLE l-semana      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE i-dia         AS INTEGER NO-UNDO.
    DEFINE VARIABLE dt-ini-semana AS DATE    NO-UNDO.
    DEFINE VARIABLE dt-fim-semana AS DATE    NO-UNDO.
    DEFINE VARIABLE dt-sem-ant    AS DATE    NO-UNDO.

    ASSIGN
        l-semana      = YES
        dt-sem-ant    = p-dt-semana
        i-dia         = WEEKDAY(p-dt-semana).

    DO WHILE l-semana:
        IF i-dia = 2 THEN
            ASSIGN 
                dt-sem-ant = dt-sem-ant - 4
                i-dia      = WEEKDAY(dt-sem-ant)
                l-semana   = NO.
        ELSE
        DO:
            ASSIGN
                dt-sem-ant = dt-sem-ant - 1
                i-dia      = WEEKDAY(dt-sem-ant).

            IF i-dia = 2 THEN
                ASSIGN 
                    dt-sem-ant = dt-sem-ant - 4
                    i-dia      = WEEKDAY(dt-sem-ant)
                    l-semana   = NO.
        END.
    END.

    ASSIGN
        l-semana      = YES
        dt-ini-semana = dt-sem-ant
        dt-fim-semana = dt-sem-ant
        i-dia         = WEEKDAY(dt-sem-ant).

    IF i-dia > 1 AND i-dia < 7 THEN
    DO WHILE l-semana:
        ASSIGN
            dt-ini-semana = dt-ini-semana - 1
            i-dia = i-dia - 1.

        IF i-dia = 1 THEN 
            ASSIGN
                l-semana = NO
                p-dt-ini-semana = dt-ini-semana.
    END.

    ASSIGN l-semana = YES.
           i-dia    = WEEKDAY(dt-sem-ant).

    IF i-dia > 1 AND i-dia < 7 THEN
    DO WHILE l-semana:
        ASSIGN
            dt-fim-semana = dt-fim-semana + 1
            i-dia = i-dia + 1.
    
        IF i-dia = 7 THEN 
            ASSIGN
                l-semana = NO
                p-dt-fim-semana = dt-fim-semana.
    END.
END.


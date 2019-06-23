/****************************************************************************************** 
** 	   Programa: cc002-rp.p
**   	      Autor: Vando Ribeiro
**   	 Fornecedor: Grupo DKP
**    	 Data: 15/11/2018
** Change/Chamado: 
**    Objetivo: Enviar e-mail com condiá∆o pagamento inferior a 21 dias
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
{include/i-prgvrs.i cc002-rp 12.00.00.001}
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
    FIELD mensagem        AS CHAR
    FIELD cod-estab       AS CHAR.
DEFINE BUFFER b-tt-mes_estab FOR tt-mes_estab.

DEFINE VARIABLE c-mess-top  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mess      AS CHARACTER   NO-UNDO.
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
       c-titulo-relat = "Envio de Email Condiá∆o Pagto Inferior a 21 Dias".

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
             + "<p> Segue abaixo a lista dos pedidos com condiá∆o de pagamento inferior a 21 dias.</p>"
             + "<TABLE style=~"width:100%~">"                                                                      
             + "<tr>"                                                                         
             + "<th>" + "Estab"          + "</th>"
             + "<th>" + "Pedido"         + "</th>"
             + "<th>" + "Data Emiss∆o"   + "</th>"
             + "<th>" + "Ordem Compra"   + "</th>"
             + "<th>" + "Contrato"       + "</th>"
             + "<th>" + "Fornecedor"     + "</th>"
             + "<th>" + "Condiá∆o Pagto" + "</th>"
             + "<th>" + "Respons†vel Pd"  + "</th>"
             + "</tr>".

c-mess-base = "</table>"                                                        
             + "<p>&nbsp;</p>"                                                   
             + "<p>" + "At," + "</p>"                                            
             + "<p>" + "<strong>" + "Sustená∆o Yamana." + "</strong>" + "</p>"   
             + "</body>"                                                         
             + "</html>" .                                                       

c-mess = "".

FOR EACH esp_hist_email_pd EXCLUSIVE-LOCK USE-INDEX idx-dt-envio
   WHERE esp_hist_email_pd.dt-envio-email = ?:

    FIND pedido-compr OF esp_hist_email_pd NO-LOCK NO-ERROR.
    FIND emitente OF esp_hist_email_pd NO-LOCK NO-ERROR.
    FIND cond-pagto OF esp_hist_email_pd NO-LOCK NO-ERROR.

    FIND usuar_mestre WHERE
         usuar_mestre.cod_usuario = esp_hist_email_pd.cod-usuario NO-LOCK NO-ERROR.
    IF NOT AVAIL usuar_mestre THEN NEXT.
    
    IF NOT AVAIL emitente THEN NEXT.
    IF NOT AVAIL cond-pagto THEN NEXT.

    c-mess = c-mess + "<tr>"
        + "<td>" + esp_hist_email_pd.cod-estabel +                                          "</td>"
        + "<td>" + string(esp_hist_email_pd.num-pedido  , ">>>>>,>>9") +                    "</td>"
        + "<td>" + string(pedido-compr.data-pedido, "99/99/9999") +                         "</td>"
        + "<td>" + string(esp_hist_email_pd.numero-orde, "zzzzz9,99") +                     "</td>"
        + "<td>" + string(esp_hist_email_pd.nr-contrato, ">>>>>>>>9") +                     "</td>"
        + "<td>" + string(esp_hist_email_pd.cod-emitente) + " - " + emitente.nome-abrev +   "</td>"
        + "<td>" + string(esp_hist_email_pd.cod-cond-pag) + " - " + cond-pagto.descricao +  "</td>"
        + "<td>" + esp_hist_email_pd.cod-usuario + " - " + usuar_mestre.nom_usuario +       "</td>"
        + "</tr>".

    ASSIGN esp_hist_email_pd.dt-envio-email = NOW.
END.


FOR EACH esp_usu_email_pd NO-LOCK:

    IF c-mess = "" THEN LEAVE.

    ASSIGN c-cod_usuario = "".
    FIND FIRST usuar_mestre
         WHERE usuar_mestre.cod_usuario = esp_usu_email_pd.cod-usuario NO-LOCK NO-ERROR.

    IF AVAIL usuar_mestre THEN
        FIND FIRST usuar_mestre_ext NO-LOCK
             WHERE usuar_mestre_ext.cod_usuario = usuar_mestre.cod_usuario
               AND usuar_mestre_ext.cod_domin_so <> "" NO-ERROR.

        IF AVAIL usuar_mestre_ext THEN
            ASSIGN c-cod_usuario = usuar_mestre_ext.cod_usuar_so.

    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.
    EMPTY TEMP-TABLE tt-erros.
     
    RUN pi-acompanhar IN h-acomp (INPUT "Enviando: " + TRIM(c-cod_usuario) + "@yamana.com").

    CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
               tt-envio2.exchange          = param_email.log_servid_exchange
               tt-envio2.servidor          = param_email.cod_servid_e_mail
               tt-envio2.porta             = param_email.num_porta
               tt-envio2.destino           = TRIM(c-cod_usuario) + "@yamana.com"
               tt-envio2.assunto           = "Pedidos com condiá∆o de pagamento inferior a 21 dias"
               tt-envio2.remetente         = "SustencaoYamana@yamana.com"
               tt-envio2.copia             = ""
               tt-envio2.importancia       = 1
               tt-envio2.log-enviada       = NO
               tt-envio2.log-lida          = NO
               tt-envio2.acomp             = NO
               tt-envio2.formato           = "html".
    
        CREATE tt-mensagem.                                       
        ASSIGN tt-mensagem.seq-mensagem = 1                         
               tt-mensagem.mensagem     = c-mess-top + c-mess + c-mess-base. 
    
        RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                       INPUT  TABLE tt-mensagem,
                                       OUTPUT TABLE tt-erros).
    IF TEMP-TABLE tt-erros:HAS-RECORDS THEN
    DO:
        OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erro-email-pd-" + STRING(TODAY, "99999999") + "-" + STRING(ETIME, "99999999") + ".txt").
        FOR EACH tt-erros:
            DISP tt-erros WITH SCROLLABLE.
        END.
        OUTPUT CLOSE.
    END.
END.

DELETE PROCEDURE h-utapi019.

{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

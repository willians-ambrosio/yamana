{utp/utapi019.i}

DEF VAR v_msg AS CHAR FORMAT "X(3000)".        
DEFINE VARIABLE v_arquivo  AS CHARACTER  FORMAT "x(10000)" NO-UNDO.
DEF VAR dat_hoje AS DATE FORMAT 99/99/9999 NO-UNDO.
DEF VAR v_status AS CHAR.
DEF VAR v_hora AS CHAR FORMAT "x(8)".
DEF VAR v_des AS CHAR NO-UNDO.
DEF VAR v_log AS LOG.
DEF VAR v_destino AS CHAR NO-UNDO.
DEF VAR v_prog AS CHAR NO-UNDO.
DEF VAR v_nom_log AS CHAR NO-UNDO.
DEF VAR v_diretorio AS CHAR NO-UNDO.
DEF VAR v_diretorio2 AS CHAR NO-UNDO.
DEF VAR v_diret_img AS CHAR NO-UNDO.

DEF STREAM str-rp.

DEF TEMP-TABLE tt-dados 
    FIELD num_pessoa_fisic AS INT
    FIELD nom_pessoa_fisic AS CHAR 
    FIELD nom_e_mail AS CHAR.

FORM
    funcionario.cdn_empresa      
    funcionario.cdn_estab        
    funcionario.cdn_funcionario  
    funcionario.nom_pessoa_fisic COLUMN-LABEL "Nome"  FORMAT "x(35)"
    rh_pessoa_fisic.nom_e_mail   COLUMN-LABEL "Email" FORMAT "x(35)"
    dat_hoje                     COLUMN-LABEL "Data Envio"
    v_hora                       COLUMN-LABEL "Hora" 
    v_status                     COLUMN-LABEL "Status" FORMAT "x(30)" SKIP
    with STREAM-IO no-attr-space NO-BOX width 300 53 DOWN FRAME f-dados.  

FORM
    funcionario.cdn_empresa      
    funcionario.cdn_estab
    funcionario.cdn_funcionario
    funcionario.nom_pessoa_fisic  FORMAT "x(30)"   
    rh_pessoa_fisic.nom_e_mail    FORMAT "x(40)"    
    dat_hoje                         
    v_hora                           
    v_status  FORMAT "x(40)" SKIP                   
    with NO-LABELS STREAM-IO no-attr-space NO-BOX width 300 53 DOWN FRAME f-registros.  

ASSIGN v_diretorio  = search("prghur\esp\ymfp0004.p") 
       v_diretorio2 = REPLACE (v_diretorio, "\ymfp0004.p", "" )
       v_diret_img  = SEARCH("image\ymfp0004.jpg")
       v_destino    = REPLACE(v_diretorio, "\ymfp0004.p", "\log0001.txt").

IF (search(v_destino) <> ?) THEN v_log = YES. ELSE v_log = NO.

OUTPUT STREAM str-rp TO VALUE (v_destino)APPEND convert target "iso8859-1".

ASSIGN dat_hoje = TODAY.

/*Corpo do E-mail*/
ASSIGN v_msg =         
    '<html>' + CHR(10) +  
    '<body>' + CHR(10) +  
    '<div id="flashContent" align=center>' + CHR(10) +
    '<a href="https://portal.spo.yamana.com/Flash/animacao_PT.swf">' + CHR(10) +
    "<img src='cid:ymfp0004.jpg' width=383 height=385  /></a>" + CHR(10) +
    '<p class=MsoNormal align=center style="text-align:center">' + '<font face=Calibri>' +  "Para visualizar o cart∆o" + '</font>' +
    '<font face=Calibri>' + '<a href="https://portal.spo.yamana.com/Flash/animacao_PT.swf">'  + " clique aqui" + '</a></p>' + '</font>' + CHR(10) +
    '<p class=MsoNormal align=center style="text-align:center">' + '<font face=Calibri>' +  "To view the card"  + '</font>' + 
    '<font face=Calibri>' + '<a href="https://portal.spo.yamana.com/Flash/animacao_eng.swf">' + " click here" + '</a></p>' + '</font>' + CHR(10) +
    '<p class=MsoNormal align=center style="text-align:center">' + '<font face=Calibri>' + "Para ver la tarjeta, haga" + '</font>' + 
    '<font face=Calibri>' + '<a href="https://portal.spo.yamana.com/Flash/animacao_esp.swf">' + " clic aqu°" + '</a></p>' +  '</font>' + CHR(10) +
    '</div>'  + CHR(10) +
    '</body>' + CHR(10) +
    '</html>' .


/*Busca funcionarios aniversariantes*/
FOR EACH funcionario NO-LOCK WHERE
         funcionario.dat_desligto = ?,
    FIRST Rh_pessoa_fisic NO-LOCK WHERE
          rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic  AND
    month(rh_pessoa_fisic.dat_nascimento) = MONTH(dat_hoje) AND
      DAY(rh_pessoa_fisic.dat_nascimento) = DAY(dat_hoje)
    BREAK BY funcionario.num_pessoa_fisic:
    
    IF NOT CAN-FIND(FIRST tt-dados WHERE 
                          tt-dados.num_pessoa_fisic = rh_pessoa_fisic.num_pessoa_fisic) THEN DO:

        CREATE tt-dados.
        ASSIGN tt-dados.num_pessoa_fisic   = rh_pessoa_fisic.num_pessoa_fisic
               tt-dados.nom_pessoa_fisic   = funcionario.nom_pessoa_fisic
               tt-dados.nom_e_mail         = rh_pessoa_fisic.nom_e_mail.

        ASSIGN v_hora = STRING(TIME,"HH:MM").

        RUN pi-envia-email.
        RUN pi-gera-log.

    END.
END.

PROCEDURE pi-envia-email:

    EMPTY TEMP-TABLE tt-erros.
    
    /*Envia E-mail */
    FOR EACH tt-envio2:
        DELETE tt-envio2.
    END.
    
    FOR EACH tt-mensagem:
        DELETE tt-mensagem.
    END.
    
    FIND FIRST param_glob_rh NO-LOCK NO-ERROR.
    IF AVAIL param_glob_rh THEN DO:
        RUN utp/utapi019.p PERSISTENT SET h-utapi019.   

        CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
               tt-envio2.servidor          = trim(substr(param_glob_rh.cod_livre_2,01,40))
               tt-envio2.porta             = param_glob_rh.num_livre_1
               tt-envio2.exchange          = param_glob_rh.log_livre_1
               tt-envio2.remetente         = "no-reply@yamana.com"  
               tt-envio2.destino           = rh_pessoa_fisic.nom_e_mail
               tt-envio2.importancia       = 2
               tt-envio2.assunto           = "Feliz anivers" + CHR(225) + "rio! / Feliz cumplea" + CHR(241) + "os! / Happy birthday!"    
               tt-envio2.formato           = "HTML"
               tt-envio2.log-enviada       = NO
               tt-envio2.log-lida          = NO
               tt-envio2.acomp             = no
               tt-envio2.arq-anexo         = v_diret_img.

        CREATE tt-mensagem.
        ASSIGN tt-mensagem.seq-mensagem = 1
               tt-mensagem.mensagem     = v_msg .
        OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "envemail.txt").
        RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                       INPUT  TABLE tt-mensagem,   
                                       OUTPUT TABLE tt-erros).
        FIND FIRST tt-erros NO-ERROR.
        IF AVAIL tt-erros THEN DO:
            ASSIGN v_status = tt-erros.desc-erro. 
        END.
        ELSE ASSIGN v_status = "Enviado com sucesso!".

        OUTPUT CLOSE.
        DELETE PROCEDURE h-utapi019.
    END.

    
END PROCEDURE.

PROCEDURE pi-gera-log.

    IF NOT v_log THEN DO:

        DISP STREAM str-rp
            funcionario.cdn_empresa
            funcionario.cdn_estab
            funcionario.cdn_funcionario
            funcionario.nom_pessoa_fisic
            rh_pessoa_fisic.nom_e_mail
            dat_hoje
            v_hora
            v_status
        with FRAME f-dados.
        DOWN WITH FRAME f-dados.

    END.
    ELSE DO:
        DISP STREAM str-rp
            funcionario.cdn_empresa
            funcionario.cdn_estab
            funcionario.cdn_funcionario
            funcionario.nom_pessoa_fisic
            rh_pessoa_fisic.nom_e_mail
            dat_hoje
            v_hora
            v_status
        with FRAME f-registros.
        DOWN WITH FRAME f-registros.

    END.
END PROCEDURE.

QUIT.

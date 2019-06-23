/*********************************************************************************
**    Programa: td_usuar_grp_usuar.p
**       Autor: Daniela Campos - DKP
**        Data: 11/05/2018
**    Objetivo: Solicita requisiá∆o quando da retirada de grupo de acesso
**********************************************************************************/
DEF PARAM BUFFER bf2_usuar_grp_usuar FOR usuar_grp_usuar.

def new global shared var v_cod_usuar_corren like usuar_mestre.cod_usuario no-undo.

DEFINE VARIABLE vlo-erro AS LOGICAL   NO-UNDO.
DEFINE VARIABLE c-req    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-origem AS CHARACTER NO-UNDO.

IF AVAIL bf2_usuar_grp_usuar THEN DO:

    RUN pi-retorno-origem (OUTPUT c-origem).

    ASSIGN c-req  = ''
           vlo-erro = NO.

    RUN esp/esmen012-g04.w (OUTPUT c-req,
                            INPUT bf2_usuar_grp_usuar.cod_usuario,
                            INPUT bf2_usuar_grp_usuar.cod_grp_usuar,
                            INPUT "Usuario").
    /* dpc */
    IF c-req = '' OR c-req = ? THEN DO:

        MESSAGE "N£mero da requisiá∆o Ç obrigat¢rio!" SKIP(01)
                 "N∆o ser† poss°vel realizar a aá∆o!"
            VIEW-AS ALERT-BOX ERROR.

        ASSIGN vlo-erro = YES.
    END.
    ELSE DO:  /* Cria permiss∆o de acesso */

        IF NOT CAN-FIND(FIRST ctrl-conflito-force NO-LOCK 
                        WHERE ctrl-conflito-force.usuario_add        = bf2_usuar_grp_usuar.cod_usuario
                        AND   ctrl-conflito-force.cod_grp_usuar_add  = bf2_usuar_grp_usuar.cod_grp_usuar
                        AND   ctrl-conflito-force.data_aprov         = TODAY
                        AND   ctrl-conflito-force.hora_aprov         = TIME) THEN DO:

            CREATE ctrl-conflito-force.
            ASSIGN ctrl-conflito-force.cod_grp_usuar_add       = bf2_usuar_grp_usuar.cod_grp_usuar
                   ctrl-conflito-force.usuario_aprov           = v_cod_usuar_corren
                   ctrl-conflito-force.usuario_add             = bf2_usuar_grp_usuar.cod_usuario
                   ctrl-conflito-force.data_aprov              = TODAY
                   ctrl-conflito-force.hora_aprov              = TIME
                   ctrl-conflito-force.motivo_aprov            = "Retirada de acesso por requisiá∆o"
                   ctrl-conflito-force.requisicao              = c-req
                   ctrl-conflito-force.tipo                    = 3 /* exclus∆o de acesso */
                   ctrl-conflito-force.origem                  = c-origem + ";Seguranáa Usu†rios".
        END.    
    END.
END.

IF vlo-erro THEN DO:

    RETURN "NOK".
END.

{upc\upsec000.i} /*definicao da procedura pi-retorna-origem */

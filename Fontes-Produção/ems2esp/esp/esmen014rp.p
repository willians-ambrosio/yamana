/*****************************************************************************************
**    Programa: esmen014rp.p
**       Autor: RDE
**        Data: 18/04/2018
**    Objetivo: Relatorio - justificativa - aprova‡Æo for‡ada conflito de acessos
******************************************************************************************/
{include/i-prgvrs.i esmen014rp 1.00.00.000} 
{utp/ut-glob.i}
{include/i-rpvar.i}

def var h-acomp            as handle  no-undo.
DEFINE VARIABLE c-usuario-aprov AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-usuario-add   AS CHARACTER   NO-UNDO.

{upc\upsec000.i} /*definicao da procedura pi-retorna-origem */
DEFINE VARIABLE c-origem AS CHARACTER   NO-UNDO.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD usuario-ini                  AS CHAR
    FIELD usuario-fim                  AS CHAR
    FIELD grupo-ini                    AS CHAR
    FIELD grupo-fim                    AS CHAR
    FIELD cod_prog_dtsul_add_fim       AS CHAR   
    FIELD cod_prog_dtsul_add_ini       AS CHAR
    field cod_prog_dtsul_base_fim      as char 
    field cod_prog_dtsul_base_ini      as char 
    field cod_prog_dtsul_conflito_fim  as char 
    field cod_prog_dtsul_conflito_ini  as char 
    field data_aprov_fim               as DATE
    field data_aprov_ini               as DATE
    FIELD tipo                         AS INT.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

for each tt-digita:
   delete tt-digita.
end.       

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param no-error. 

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Processando").

{include/i-rpout.i}
CASE tt-param.tipo:
    
    WHEN 1 /*conflito */ THEN
        PUT "Origem;Programa Origem;Data Aprova‡Æo;Hora;Usu rio Aprova‡Æo;Nome;Usu rio Adicionado;Nome;Grupo Adicionado;Programa Base;Programa Conflito;Requisi‡Æo;Justificativa aprova‡Æo for‡ada" SKIP.
    WHEN 2 /*normal */
         THEN PUT "Origem;Programa Origem;Data Aprova‡Æo;Hora;Usu rio Aprova‡Æo;Nome;Usu rio Adicionado;Nome;Grupo Adicionado;Requisi‡Æo;Motivo" SKIP.
    WHEN 3 /*retirada acesso */ THEN
        PUT "Origem;Programa Origem;Data Aprova‡Æo;Hora;Usu rio Aprova‡Æo;Nome;Usu rio Retiradao;Nome;Grupo Retirado;Requisi‡Æo;Motivo" SKIP.
END CASE.

RUN pi-retorno-origem (OUTPUT c-origem).

FOR EACH ctrl-conflito-force WHERE
    ctrl-conflito-force.data_aprov              >= tt-param.data_aprov_ini              AND
    ctrl-conflito-force.data_aprov              <= tt-param.data_aprov_fim              AND
    ctrl-conflito-force.usuario_add             >= tt-param.usuario-ini                 AND
    ctrl-conflito-force.usuario_add             <= tt-param.usuario-fim                 AND 
    ctrl-conflito-force.cod_grp_usuar_add       >= tt-param.grupo-ini                   AND
    ctrl-conflito-force.cod_grp_usuar_add       <= tt-param.grupo-fim                   AND
    ctrl-conflito-force.cod_prog_dtsul_base     >= tt-param.cod_prog_dtsul_base_ini     AND
    ctrl-conflito-force.cod_prog_dtsul_base     <= tt-param.cod_prog_dtsul_base_fim     AND
    ctrl-conflito-force.cod_prog_dtsul_conflito >= tt-param.cod_prog_dtsul_conflito_ini AND
    ctrl-conflito-force.cod_prog_dtsul_conflito <= tt-param.cod_prog_dtsul_conflito_fim AND 
    ctrl-conflito-force.tipo                     = tt-param.tipo                    NO-LOCK
    BREAK BY ctrl-conflito-force.data_aprov
          BY ctrl-conflito-force.hora_aprov
          BY ctrl-conflito-force.usuario_aprov:

     ASSIGN c-usuario-aprov = ""
            c-usuario-add   = "".


     /* Lista somente a origem de acordo com o banco conectado */
     IF TRIM(ENTRY(1,ctrl-conflito-force.origem,";")) = c-origem THEN DO:

         /* Busca nome usu rio aprova‡Æo */
         FIND usuar_mestre NO-LOCK WHERE
              usuar_mestre.cod_usuario = ctrl-conflito-force.usuario_aprov NO-ERROR.
         ASSIGN c-usuario-aprov = usuar_mestre.nom_usuario WHEN AVAIL usuar_mestre.
    
         /* Busca nome usu rio add-del*/
         FIND usuar_mestre NO-LOCK WHERE
              usuar_mestre.cod_usuario = ctrl-conflito-force.usuario_add NO-ERROR.
         ASSIGN c-usuario-add = usuar_mestre.nom_usuario WHEN AVAIL usuar_mestre.
    
         RUN pi-acompanhar IN h-acomp (input "Data/Hora: " + STRING(ctrl-conflito-force.data_aprov) + " / " + STRING(ctrl-conflito-force.hora_aprov,"hh:mm:ss")).
    
         IF tt-param.tipo = 1 /*conflito */  THEN
            PUT UNFORMATTED 
                 ENTRY(1,ctrl-conflito-force.origem,";")             ";"
                 ENTRY(2,ctrl-conflito-force.origem,";")             ";"
                 ctrl-conflito-force.data_aprov                  ";"
                 STRING(ctrl-conflito-force.hora_aprov,"hh:mm:ss")  ";"
                 ctrl-conflito-force.usuario_aprov               ";"                  
                 c-usuario-aprov                                 ";"
                 ctrl-conflito-force.usuario_add                 ";"
                 c-usuario-add                                   ";"
                 ctrl-conflito-force.cod_grp_usuar_add           ";"
                 ctrl-conflito-force.cod_prog_dtsul_base         ";"  
                 ctrl-conflito-force.cod_prog_dtsul_conflito     ";"
                 ctrl-conflito-force.requisicao                  ";"
                 ctrl-conflito-force.motivo_aprov                SKIP.
    
         IF tt-param.tipo = 2 /* libera‡Æo normal */  THEN
            PUT UNFORMATTED 
                 ENTRY(1,ctrl-conflito-force.origem,";")             ";"
                 ENTRY(2,ctrl-conflito-force.origem,";")             ";"
                 ctrl-conflito-force.data_aprov                  ";"
                 STRING(ctrl-conflito-force.hora_aprov,"hh:mm:ss")  ";"
                 ctrl-conflito-force.usuario_aprov               ";"                  
                 c-usuario-aprov                                 ";"
                 ctrl-conflito-force.usuario_add                 ";"
                 c-usuario-add                                   ";"
                 ctrl-conflito-force.cod_grp_usuar_add           ";"
                 ctrl-conflito-force.requisicao                  ";"
                 ctrl-conflito-force.motivo_aprov                SKIP.
    
         IF tt-param.tipo = 3 /* retirada acesso */  THEN
            PUT UNFORMATTED 
                 ENTRY(1,ctrl-conflito-force.origem,";")             ";"
                 ENTRY(2,ctrl-conflito-force.origem,";")             ";"
                 ctrl-conflito-force.data_aprov                  ";"
                 STRING(ctrl-conflito-force.hora_aprov,"hh:mm:ss")  ";"
                 ctrl-conflito-force.usuario_aprov               ";"                  
                 c-usuario-aprov                                 ";"
                 ctrl-conflito-force.usuario_add                 ";"
                 c-usuario-add                                   ";"
                 ctrl-conflito-force.cod_grp_usuar_add           ";"
                 ctrl-conflito-force.requisicao                  ";"
                 ctrl-conflito-force.motivo_aprov                SKIP.
     END.
END.
                
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.

RETURN "OK":U.

/* fim - esmen014rp.p */
                                                         

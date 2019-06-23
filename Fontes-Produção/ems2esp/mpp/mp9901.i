/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*******************************************************************************
**
**   Programa.......: mp9901.i
**
**   Autor..........: Datasul S/A 
**
**   Objetivo.......: Defini‡Æo da temp-table TT-ERRO-CONEXAO 
**
*********************************************************************************/

def temp-table tt-erro-conexao NO-UNDO
    /* erros cadastrados */
    field cod-versao-integracao  as integer format "999"
    field cod-maq-destino        as integer format "9999"
    field cod-erro               as integer format "99999"
    field desc-erro              as char    format "x(60)"
    field conteudo-erro          as char    format "x(20)"
    /* campo classificador */
    field tp-erro-cad            as log
    /* erros do progress */
    field db-erro                as char
    field lb-erro                as char
    field service-erro           as char
    field host-erro              as char
    field param-erro             as char
    field dt-erro                as date
    field hora-erro              as int.

def temp-table tt-mqseries NO-UNDO
    field Hconn       as int
    field Hobj        as int
    field Hretorno    as int
    field QMgrName    as char  /* Nome da Queue Manager local */
    field ReplyToQ    as char  /* Nome da Queue de Retorno    */  
    field ObjectName  as char  /* Queue conectada             */   
    field queue-retorno as char  /* Queue de retorno            */
    field tp-queue    as char
    field handle-prog as handle
    field cd-maquina  like maquina.cd-maquina.
            

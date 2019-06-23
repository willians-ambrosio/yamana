/* Definição da tt-param */
 
{cdp/cdcfgmnt.i}

DEFINE BUFFER bfeqpto FOR mab-eqpto.
DEFINE BUFFER bfmodel FOR mab-model.
DEFINE BUFFER bfmab-trajeto FOR mab-trajeto.

/****************** Defini‡ao de Vari veis de Sele‡Æo de parƒmetros do Relat¢rio *********************/ 
def  var c-yes             as char    no-undo.  
def  var c-no              as char    no-undo.  
 
def  var l-motorizados     as log view-as toggle-box.
def  var l-n-motorizados   as log view-as toggle-box.
def  var l-ativos          as log view-as toggle-box.
def  var l-Inativos        as log view-as toggle-box.
def  var l-proprios        as log view-as toggle-box.
def  var l-terceiros       as log view-as toggle-box.
def  var l-combust         as log view-as toggle-box.
def  var l-caract          as log view-as toggle-box.
def  var l-comptes         as log view-as toggle-box.
def  var l-hist            as log view-as toggle-box.
def  var l-eventos         as log view-as toggle-box.
def  var l-planos          as log view-as toggle-box.
def  var l-config          as log view-as toggle-box.
def  var l-param           as log view-as toggle-box.
def  var l-Insere          as log view-as toggle-box.

def  var h-acomp           as handle  no-undo.

def  var i-ordem-ini            like ord-manut.nr-ord-produ          no-undo.                   
def  var i-ordem-fim            like ord-manut.nr-ord-produ          no-undo.      
def  var i-empresa-ini          like ord-manut.ep-codigo             no-undo.     
def  var i-empresa-fim          like ord-manut.ep-codigo             no-undo.      
def  var c-equipamento-ini      like ord-manut.cd-equipto            no-undo.
def  var c-equipamento-fim      like ord-manut.cd-equipto            no-undo.
def  var dt-manut-ini           like ord-manut.dt-manut              no-undo.
def  var dt-manut-fim           like ord-manut.dt-manut              no-undo.
def  var dt-ini-cedo-ini        like ord-manut.dt-ini-cedo           no-undo.
def  var dt-ini-cedo-fim        like ord-manut.dt-ini-cedo           no-undo.
def  var i-tipo-manut-ini       like ord-manut.cd-tipo               no-undo.
def  var i-tipo-manut-fim       like ord-manut.cd-tipo               no-undo.
DEF  VAR i-planejador-ini       LIKE ord-manut.cd-planejado          NO-UNDO.
DEF  VAR i-planejador-fim       LIKE ord-manut.cd-planejado          NO-UNDO.
def  var dt-ini-tarde-ini        like ord-manut.dt-ini-tarde         no-undo.
def  var dt-ini-tarde-fim        like ord-manut.dt-ini-tarde         no-undo.
def  var dt-prev-termino-ini    like ord-manut.dt-prev               no-undo.
def  var dt-prev-termino-fim    like ord-manut.dt-prev               no-undo.
def  var dt-termino-ini         like ord-manut.dt-fecham             no-undo.
def  var dt-termino-fim         like ord-manut.dt-fecham             no-undo.
                                
/** OM **/                      
DEF VAR i-ordem                 LIKE ord-manut.nr-ord-produ          no-undo.
DEF VAR c-estado                AS CHARACTER FORMAT "X(12)"          NO-UNDO.
DEF VAR c-equipto               AS CHARACTER FORMAT "X(50)"          NO-UNDO.
DEF VAR dt-manut                LIKE ord-manut.dt-manut              no-undo.
DEF VAR dt-ini-cedo             LIKE ord-manut.dt-ini-cedo           no-undo.
DEF VAR c-TipoManutencao        AS CHARACTER FORMAT "X(30)"          no-undo.
DEF VAR c-Planejador            LIKE ord-manut.cd-planejado          no-undo.
DEF VAR dt-ini-tarde            LIKE ord-manut.dt-ini-tarde          no-undo.
DEF VAR dt-PrevTermino          LIKE ord-manut.dt-prev               no-undo.
DEF VAR dt-Termino              LIKE ord-manut.dt-fecham             no-undo.
DEF VAR c-TipoOrdem             AS CHARACTER FORMAT "x(16)"          NO-UNDO.  
DEF VAR c-plano                 LIKE ord-manut.plano-orig            NO-UNDO.

/** Tarefa **/
DEF VAR i-tarefa                LIKE ord-taref.cd-tarefa             NO-UNDO.
DEF VAR c-evento                AS CHARACTER FORMAT "X(45)"          NO-UNDO.
DEF VAR c-estado-om             AS CHARACTER FORMAT "X(15)"          NO-UNDO.
DEF VAR h-reportadas            LIKE ord-taref.tempo-real            NO-UNDO.
DEF VAR psegundo                AS INTEGER                           NO-UNDO.
/*DEF VAR h-tempo                 LIKE ord-taref.tempo                 NO-UNDO.*/

/** Previsto **/
DEF VAR c-tecnico               AS CHARACTER FORMAT "X(25)"          no-undo.
DEF VAR dt-data-previs          AS DATE FORMAT "99/99/9999"          no-undo.
DEF VAR c-equipe-setor          AS CHARACTER FORMAT "X(40)"          NO-UNDO.
                                
/**Reportados**/                
DEF VAR c-funcionario           AS CHAR FORMAT "X(30)"               no-undo. 
DEF VAR dt-realizado            LIKE  ord-mob.dt-trans               no-undo. 
DEF VAR hr-inicio               LIKE  ord-mob.hora-inicio            NO-UNDO. 
DEF VAR hr-final                LIKE  ord-mob.hora-termino           no-undo.
DEF VAR c-equipe                AS CHARACTER FORMAT "X(33)"          NO-UNDO.

DEFINE VARIABLE Ordem            AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE Estado           AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE Equipto          AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE DtManut          AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE DtIniCedo        AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE DtIniTarde       AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE TpOrdem          AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE TipoManut        AS CHARACTER FORMAT "X(15)"         NO-UNDO.
DEFINE VARIABLE Planejador       AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE dtprev           AS CHARACTER FORMAT "X(12)"         NO-UNDO.                            
DEFINE VARIABLE dtPrevManut      AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE plano            AS CHARACTER FORMAT "X(12)"         NO-UNDO.
                                                                     
DEFINE VARIABLE tarefa           AS CHARACTER FORMAT "X(10)"         NO-UNDO.
DEFINE VARIABLE evento           AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE ch-estado        AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE planoManut       AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE HReporte         AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE SubSist          AS CHARACTER FORMAT "X(12)"         NO-UNDO.



DEFINE VARIABLE previsto         AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE tecnico          AS CHARACTER FORMAT "X(15)"         NO-UNDO.
DEFINE VARIABLE dtPrevisto       AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE equipe-setor     AS CHARACTER FORMAT "X(12)"         NO-UNDO.
                                                                     
DEFINE VARIABLE reportado        AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE funcionario      AS CHARACTER FORMAT "X(15)"         NO-UNDO.
DEFINE VARIABLE dtRealizado      AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE hrInicio         AS CHARACTER FORMAT "X(12)"         NO-UNDO.
DEFINE VARIABLE hrFinal          AS CHARACTER FORMAT "X(10)"         NO-UNDO.
DEFINE VARIABLE equipe_setor     AS CHARACTER FORMAT "X(15)"         NO-UNDO.



/**ordem**/
{utp/ut-liter.i "Ordem" *}
ASSIGN Ordem = RETURN-VALUE.
{utp/ut-liter.i "Estado" *}
ASSIGN Estado = RETURN-VALUE.
{utp/ut-liter.i "Equipto" *}
ASSIGN Equipto = RETURN-VALUE.
{utp/ut-liter.i "DtManut" *}
ASSIGN DtManut = RETURN-VALUE.
{utp/ut-liter.i "DtIniCedo" *}
ASSIGN DtIniCedo = RETURN-VALUE.
{utp/ut-liter.i "Tp_Ordem" *}
ASSIGN TpOrdem = RETURN-VALUE.
{utp/ut-liter.i "Tipo_Manuten‡Æo" *}
ASSIGN TipoManut = RETURN-VALUE.
{utp/ut-liter.i "Planejador" *}
ASSIGN planejador = RETURN-VALUE.
{utp/ut-liter.i "DtIniTarde" *}
ASSIGN dtIniTarde = RETURN-VALUE.
{utp/ut-liter.i "DtPrev" *}
ASSIGN dtprev = RETURN-VALUE.
{utp/ut-liter.i "DtPrevManut" *}
ASSIGN dtPrevManut = RETURN-VALUE.
{utp/ut-liter.i "Plano Orig" *}
ASSIGN plano = RETURN-VALUE.


/**tarefa**/
{utp/ut-liter.i "Tarefa" *}
ASSIGN Tarefa = RETURN-VALUE.
{utp/ut-liter.i "Evento" *}
ASSIGN evento = RETURN-VALUE.
{utp/ut-liter.i "Estado" *}
ASSIGN ch-estado = RETURN-VALUE.
{utp/ut-liter.i "Plano_Manut" *}
ASSIGN planoManut = RETURN-VALUE.
{utp/ut-liter.i "H._Reporte" *}
ASSIGN HReporte = RETURN-VALUE.
{utp/ut-liter.i "Sub-Sistema" *}
ASSIGN subSist = RETURN-VALUE.

/**previsto**/

{utp/ut-liter.i "Previsto:" *}
ASSIGN previsto = RETURN-VALUE.
{utp/ut-liter.i "T‚cnico" *}
ASSIGN tecnico = RETURN-VALUE.
{utp/ut-liter.i "Dt._Previs" *}
ASSIGN dtPrevisto = RETURN-VALUE.
{utp/ut-liter.i "Equipe/Setor" *}
ASSIGN equipe-setor = RETURN-VALUE.

/**reportado**/

{utp/ut-liter.i "Reportado:" *}
ASSIGN reportado = RETURN-VALUE.
{utp/ut-liter.i "Funcion rio" *}
ASSIGN funcionario = RETURN-VALUE.
{utp/ut-liter.i "Dt._Realizado" *}
ASSIGN dtRealizado = RETURN-VALUE.
{utp/ut-liter.i "hr._Inici" *}
ASSIGN hrInicio = RETURN-VALUE.
{utp/ut-liter.i "Hr._Final" *}
ASSIGN hrFinal = RETURN-VALUE.
{utp/ut-liter.i "Equipe/Setor" *}
ASSIGN equipe_setor = RETURN-VALUE.



FORM SKIP (2) Ordem        NO-LABEL                                 AT 1
     Estado       NO-LABEL                                 AT 13
     Equipto      NO-LABEL                                 AT 26
     DtManut      NO-LABEL                                 AT 85
     DtIniCedo    NO-LABEL                                 AT 98 
     DtIniTarde    NO-LABEL                                AT 110 SKIP
                 
     TpOrdem      NO-LABEL                                 AT 1
     TipoManut    NO-LABEL                                 AT 17 
     Planejador   NO-LABEL                                 AT 54
     plano        NO-LABEL                                 AT 70
     dtprev       NO-LABEL                                 AT 96
     dtPrevManut  NO-LABEL                                 AT 108 SKIP


    ----------------------------------------------------------------------------------------------------------------------------------- AT 1 SKIP


     i-Ordem                         AT 1   NO-LABEL
     c-Estado                        AT 13  NO-LABEL    
     c-Equipto                       AT 26  NO-LABEL
     dt-manut                        AT 85  NO-LABEL
     dt-ini-cedo                     AT 98 NO-LABEL 
     dt-ini-tarde                    AT 110 NO-LABEL SKIP                                     

     c-tipoOrdem                     AT 1   NO-LABEL
     c-TipoManutencao                AT 17  NO-LABEL
     c-Planejador                    AT 54  NO-LABEL
     c-plano                         AT 70  NO-LABEL
     dt-PrevTermino                  AT 96  NO-LABEL
     dt-Termino                      AT 110 NO-LABEL SKIP (1)

    with  no-box  width 132 side-labels frame f-ordem stream-io.

    

FORM    tarefa        NO-LABEL                             AT 3
        evento        NO-LABEL                             AT 13
        ch-estado     NO-LABEL                             AT 62
        HReporte      NO-LABEL                             AT 82 SKIP
        
        ------                                             AT 3
        --------------------------------------------       AT 13
        ------------                                       AT 62
        ----------------                                   AT 82 SKIP

        
        i-tarefa                                           AT 3   NO-LABEL
        c-evento                                           AT 13  NO-LABEL
        c-estado-om                                        AT 62  NO-LABEL
        h-reportadas                                       AT 82 NO-LABEL SKIP

    with  no-box  width 132 side-labels frame f-tarefa stream-io.
        
        
 FORM   previsto          NO-LABEL                         AT 10
        tecnico           NO-LABEL                         AT 23
        dtPrevisto        NO-LABEL                         AT 54
        equipe-setor      NO-LABEL                         AT 66 SKIP
                        
        "-----------------------------"                    AT 23
        "----------"                                       AT 54
        "--------------------------------"                 AT 66 SKIP
                        
        c-tecnico                                          AT 23 NO-LABEL
        dt-data-previs                                     AT 54 NO-LABEL 
        c-equipe-setor                                     AT 66 NO-LABEL SKIP
        
        
   with  no-box  width 132 side-labels frame f-previsto stream-io.     
        
                                                           
 FORM   reportado     NO-LABEL                             AT 10
        funcionario   NO-LABEL                             AT 23
        dtRealizado   NO-LABEL                             AT 54
        hrInicio      NO-LABEL                             AT 68
        hrFinal       NO-LABEL                             AT 81
        Equipe_Setor  NO-LABEL                             AT 92 SKIP
        
        "-----------------------------"                    AT 23
        "-------------"                                    AT 54
        "-----------"                                      AT 68
        "---------"                                        AT 81
        "--------------------------------"                 AT 92 SKIP
        
        
        c-funcionario                                      AT 23 NO-LABEL
        dt-realizado                                       AT 54 NO-LABEL
        hr-inicio                                          AT 68 NO-LABEL
        hr-final                                           AT 81 NO-LABEL
        c-equipe                                           AT 92 NO-LABEL
     with  no-box  width 132 side-labels frame f-reportado stream-io.


 /* {utp/ut-liter.i "Chassi/S‚rie" * R }                                                 */
/* assign  mab-eqpto.cod-chassi:label in frame f-eqpto = trim(return-value).            */
/* {utp/ut-liter.i Tipo * R }                                                           */
/* assign  cTipoEqpto:label in frame f-eqpto = trim(return-value).                      */
/* {utp/ut-liter.i Tipo_Propriet rio }                                                  */
/* assign  cTipoProp:label in frame f-eqpto  = trim(return-value).                      */
/* {utp/ut-liter.i Propriet rio }                                                       */
/* assign  cProp:label in frame f-eqpto  = trim(return-value).                          */
/* {utp/ut-liter.i Ano_Fabric./Modelo }                                                 */
/* assign  cAnoModel:label in frame f-eqpto  = substring(trim(return-value),1,18).      */
/* {utp/ut-liter.i Utiliza_Contador_Secund rio }                                        */
/* assign   mab-eqpto.log-cont-sec:label in frame f-eqpto  = trim(return-value).        */
/* {utp/ut-liter.i Contador_Secund rio_Inicial }                                        */
/* assign   mab-eqpto.val-hodom-horim-sec:label in frame f-eqpto  = trim(return-value). */
/* {utp/ut-liter.i Uso_Secund rio_Inicial }                                             */
/* assign   mab-eqpto.val-km-sec:label in frame f-eqpto  = trim(return-value).          */





/* Parameters Definitions --- */
      DEF temp-table tt-param
        field destino              as integer
        field arquivo              as char
        field usuario              as char
        field data-exec            as date
        field hora-exec            as integer
        field parametro            as logical
        FIELD formato              as integer
        field v_num_tip_aces_usuar as integer
        field i-ep-codigo          as integer
        field classifica           as integer
        field estado               as integer
        field tipo                 as integer
        field desc-classifica      as char format "x(40)"
        field rs-considera         as integer
        field desc-considera       as char format "x(40)"
        field l-motorizados        as logical  
        field l-n-motorizados      as logical
        field l-ativos             as logical
        field l-Inativos           as logical 
        field l-proprios           as logical  
        field l-terceiros          as logical
        field l-combust            as logical
        field l-caract             as logical
        field l-comptes            as logical
        field l-hist               as logical  
        field l-eventos            as logical
        field l-planos             as logical
        field l-config             as logical 
        field l-param              as logical
        field l-Insere             as logical
        
        field i-ordem-ini          like ord-manut.nr-ord-produ 
        field i-ordem-fim          like ord-manut.nr-ord-produ 
        field i-empresa-ini        LIKE ord-manut.ep-codigo    
        field i-empresa-fim        like ord-manut.ep-codigo    
        field c-equipamento-ini    like ord-manut.cd-equipto   
        field c-equipamento-fim    like ord-manut.cd-equipto   
        field dt-manut-ini         like ord-manut.dt-manut     
        field dt-manut-fim         like ord-manut.dt-manut     
        field dt-ini-cedo-ini      like ord-manut.dt-ini-cedo  
        field dt-ini-cedo-fim      like ord-manut.dt-ini-cedo  
        field i-tipo-manut-ini     like ord-manut.cd-tipo      
        field i-tipo-manut-fim     like ord-manut.cd-tipo      
        FIELD i-planejador-ini     LIKE ord-manut.cd-planejado 
        FIELD i-planejador-fim     LIKE ord-manut.cd-planejado 
        field dt-ini-tarde-ini     like ord-manut.dt-ini-tarde 
        field dt-ini-tarde-fim     like ord-manut.dt-ini-tarde 
        field dt-prev-termino-ini  like ord-manut.dt-prev      
        field dt-prev-termino-fim  like ord-manut.dt-prev      
        field dt-termino-ini       like ord-manut.dt-fecham    
        field dt-termino-fim       like ord-manut.dt-fecham.    
        
def temp-table tt-digita no-undo
    field linha      as int
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.




/****Fim da Include********/

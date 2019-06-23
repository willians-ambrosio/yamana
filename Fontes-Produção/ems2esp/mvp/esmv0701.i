/* Definição da tt-param */
 
{cdp/cdcfgmnt.i}

.
DEFINE BUFFER bfeqpto FOR mab-eqpto.
DEFINE BUFFER bfmodel FOR mab-model.
DEFINE BUFFER bfmab-trajeto FOR mab-trajeto.

/****************** Defini‡ao de Vari veis de Sele‡Æo de parƒmetros do Relat¢rio *********************/ 

def  var h-acomp           as handle  no-undo.

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
        field naoIniciado          as LOGICAL
        FIELD Iniciado             AS LOGICAL
        FIELD Terminado            AS LOGICAL
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
        field l-Insere             as LOGICAL
        field dt-programacao-ini   like mmv-eqpto-calend-plano.dat-movto
        field dt-programacao-fim   like mmv-eqpto-calend-plano.dat-movto
        field i-empresa-ini        like mmv-eqpto-calend-plano.ep-codigo
        field i-empresa-fim        like mmv-eqpto-calend-plano.ep-codigo
        field c-estabelec-ini      like mmv-ord-manut.cod-estabel       
        field c-estabelec-fim      like mmv-ord-manut.cod-estabel       
        field c-equipamento-ini    like mmv-eqpto-calend-plano.cod-eqpto
        field c-equipamento-fim    like mmv-eqpto-calend-plano.cod-eqpto
        field c-oficina-ini        like mmv-eqpto-calend-plano.cod-ofici
        field c-oficina-fim        like mmv-eqpto-calend-plano.cod-ofici.
        
 
        
def temp-table tt-digita no-undo
    field linha      as int
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

DEFINE TEMP-TABLE TT-ano
    FIELD inicial AS DATE
    FIELD final   AS DATE.

DEFINE TEMP-TABLE TT-eqpto
    FIELD cod-eqpto   AS CHAR
    FIELD data        AS DATE
    FIELD previsto    AS CHAR
    FIELD model       AS CHAR.

/****Fim da Include********/

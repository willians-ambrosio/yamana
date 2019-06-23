
/******************************************************************************************************************************************
** Programa: esp/ymof0115rp.p
** Data    : 03-09-2015
** Autor   : Mauricio Cerqueira Miranda
** Objetivo: Extra‡Æo de itens sem CFA
********************************************************************************************************************************************/

/* include de controle de versÊo */
{include/i-prgvrs.i YMOF0115 "TOTVS"}


define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    FIELD c-item-i         AS CHAR FORMAT "x(16)"
    FIELD c-item-f         AS CHAR FORMAT "x(16)"
    FIELD d-implanta-ini          AS DATE 
    FIELD d-implanta-fim          AS DATE
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

DEFINE VARIABLE c-status AS CHARACTER   NO-UNDO.

/* recebimento de parümetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*Carregando a tt-digita*/
create tt-param.
raw-transfer raw-param to tt-param.


/* carregando tt-digita */
For Each tt-raw-digita:
    Create tt-digita.
    Raw-transfer tt-raw-digita.raw-digita To tt-digita.
End.     


DEFINE VARIABLE  i-ep-codigo-usuario1 AS CHARACTER   NO-UNDO.
def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.

FIND FIRST trad_org_ext WHERE trad_org_ext.cod_tip_unid_organ = "998"                  
                        AND   trad_org_ext.cod_matriz_trad_org_ext = "ems2"            
                        AND   trad_org_ext.cod_unid_organ = i-ep-codigo-usuario NO-LOCK NO-ERROR.    
IF AVAIL trad_org_ext THEN DO:                                                         
    ASSIGN  i-ep-codigo-usuario1 =  trad_org_ext.cod_unid_organ_ext.
END.
ELSE ASSIGN i-ep-codigo-usuario1 = i-ep-codigo-usuario.

define variable h-acomp as handle no-undo.                             

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Extra‡Æo de Dados").

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "extracao_item_sCfa.csv") .

PUT
    "Empresa            ;"
    "Item               ;"
    "Descricao          ;"
    "Grupo Estoque      ;"
    "Familia            ;"
    "Familia Comercial  ;"
    "Unidade Medida     ;"
    "Data Implantacao   ;"
    "Situacao           ;" SKIP.

FOR EACH ITEM NO-LOCK
   WHERE ITEM.it-codigo    >= tt-param.c-item-i
     AND ITEM.it-codigo    <= tt-param.c-item-f
     AND ITEM.data-implant >= tt-param.d-implanta-ini
     AND ITEM.data-implant <= tt-param.d-implanta-fim:

    FIND FIRST ext-item-cfa NO-LOCK
         WHERE ext-item-cfa.it-codigo = ITEM.it-codigo
           AND ext-item-cfa.ep-codigo = i-ep-codigo-usuario1 NO-ERROR.
    IF AVAIL(ext-item-cfa) THEN NEXT.

    run pi-acompanhar in h-acomp(input "Item: " + string(ITEM.it-codigo)).


    CASE ITEM.cod-obsoleto:
        WHEN 1 THEN
            ASSIGN c-status = "Ativo".
        WHEN 2 THEN
            ASSIGN c-status = "Obs. Ordens Automaticas".
        WHEN 3 THEN
            ASSIGN c-status = "Obs. todas as Ordens".
        WHEN 4 THEN
            ASSIGN c-status = "Totalmente Obsoleto".
    END CASE.

   
    
    EXPORT DELIMITER ';'
            i-ep-codigo-usuario1
            ITEM.it-codigo
            ITEM.desc-item
            ITEM.ge-codigo
            ITEM.fm-codigo
            ITEM.fm-cod-com
            ITEM.un
            ITEM.data-implant
            c-status.



END.
RUN pi-finalizar IN h-acomp.

OUTPUT CLOSE.

MESSAGE "Foi criado um relatorio em: " + string(SESSION:TEMP-DIRECTORY) + "extracao_item_sCfa.csv"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

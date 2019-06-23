/**============================================================**
** Programa....: 
** Empresa.....: Cleilton / DSC
** Data........: 16/02/2015
** Objetivo....: Replicar os itens para as demais empresas
** Alteraá∆o...: 
** ............:  
**=============================================================**/
{include/i_dbvers.i}
{include/i-prgvrs.i YMCD0203A 11.5.11.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMCD0203A MCD}
&ENDIF

/*** Definiá∆o de Vari†veis Globais ***/
{utp/ut-glob.i}


{cdp/cdcfgman.i}
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
{cdp/cdcfgcex.i}

/*** Definiá∆o de ParÉmetros ***/
DEFINE INPUT  PARAMETER p-emp AS CHARACTER   NO-UNDO.

/*** Definiá∆o de Vari†veis Locais ***/
DEFINE VARIABLE c-log AS CHARACTER   NO-UNDO.

/* Validaá‰es B†sicas */
Gravar:
FOR EACH es-fila-rep-item
    WHERE es-fila-rep-item.ep-codigo = STRING(p-emp) EXCLUSIVE-LOCK
    TRANSACTION ON ERROR UNDO Gravar, NEXT Gravar:

    ASSIGN c-log = "".

    /* Unidade de Medida */
    IF NOT CAN-FIND(FIRST {1}tab-unidade
                    WHERE {1}tab-unidade.un = es-fila-rep-item.un NO-LOCK) THEN DO:
        ASSIGN c-log = c-log + "| " + 
                       "Unidade de medida " + QUOTER(es-fila-rep-item.un) + " n∆o encontrada no ERP.".
    END.

    /* Grupo de estoque */
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-log = c-log + "| " + 
                                    "Grupo de Estoque " + QUOTER(es-fila-rep-item.ge-codigo) + " deve ser n£mero inteiro.".
    END.
    IF NOT CAN-FIND(FIRST {1}grup-estoque
                    WHERE {1}grup-estoque.ge-codigo = INTEGER(es-fila-rep-item.ge-codigo) NO-LOCK) THEN DO:
        ASSIGN c-log = c-log + "| " + 
                                    "Grupo de Estoque " + QUOTER(es-fila-rep-item.ge-codigo) + " n∆o encontrada no ERP.".
    END.

    /* Fam°lia de materiais */
    FOR FIRST {1}familia FIELDS (fm-codigo deposito-pad)
        WHERE {1}familia.fm-codigo = es-fila-rep-item.fm-codigo NO-LOCK: END. 
    IF NOT AVAIL {1}familia THEN DO:
        ASSIGN c-log = c-log + "| " + 
                                    "Sub-Grupo(Familia Material) " + QUOTER(es-fila-rep-item.fm-codigo) + " n∆o encontrada no ERP.".
    END.
    FOR FIRST {1}fam-uni-estab FIELDS(cod-estabel deposito-pad)
        WHERE {1}fam-uni-estab.fm-codigo = es-fila-rep-item.fm-codigo NO-LOCK: 
        ASSIGN es-fila-rep-item.cod-estabel  = {1}fam-uni-estab.cod-estabel.
    END.

    /* Fam°lia comercial */
    IF  TRIM(es-fila-rep-item.fm-cod-com) <> "" AND
        NOT CAN-FIND(FIRST {1}fam-comerc
                    WHERE {1}fam-comerc.fm-cod-com = es-fila-rep-item.fm-cod-com NO-LOCK) THEN DO:
        ASSIGN c-log = c-log + "| " + 
                                    "Fam°lia(Familia Comercial) " + QUOTER(es-fila-rep-item.fm-cod-com) + " n∆o encontrada no ERP.".
    END.

    /* Verificar se item origem existe e esta ativo no ERP */
    FOR FIRST {1}item FIELDS(it-codigo cod-obsoleto cod-estabel ge-codigo un)
        WHERE {1}item.it-codigo = es-fila-rep-item.it-codigo NO-LOCK: END.
    IF AVAIL {1}item THEN DO:
        DELETE es-fila-rep-item.
        NEXT Gravar.
    END.

    IF TRIM(c-log,"| ") <> "" THEN DO:
        ASSIGN es-fila-rep-item.dt-ult-tentativa = NOW
               es-fila-rep-item.mensagem-erro    = TRIM(c-log,"| ")
               es-fila-rep-item.nr-tentativas    = es-fila-rep-item.nr-tentativas + 1.
        NEXT Gravar.
    END.

    DISABLE TRIGGERS FOR LOAD OF {1}item.
    CREATE {1}item.
    BUFFER-COPY es-fila-rep-item TO {1}item
        ASSIGN {1}item.cod-obsoleto   = 4.                                                                   
    IF AVAIL {1}fam-uni-estab THEN DO:
        ASSIGN {1}item.deposito-pad = {1}fam-uni-estab.deposito-pad
               {1}item.cod-estabel  = {1}fam-uni-estab.cod-estabel.
    END.

    FIND FIRST {2}es-it-depto
        WHERE {2}es-it-depto.it-codigo = es-fila-rep-item.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL {2}es-it-depto THEN DO:
        CREATE {2}es-it-depto.
        ASSIGN {2}es-it-depto.cod-depto      = INTEGER(es-fila-rep-item.cod-depto)
               {2}es-it-depto.data           = TODAY
               {2}es-it-depto.data-altera    = TODAY
               {2}es-it-depto.hora           = STRING(TIME,"hh:mm")
               {2}es-it-depto.hr-altera      = STRING(TIME,"hh:mm")
               {2}es-it-depto.it-codigo      = es-fila-rep-item.it-codigo
               {2}es-it-depto.origem         = "Carga via replicaá∆o"
               {2}es-it-depto.usuar-altera   = "Sistema"
               {2}es-it-depto.usuario        = "Sistema".
    END.
    DELETE es-fila-rep-item.

END. /* es-fila-rep-item */



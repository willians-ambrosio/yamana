DEFINE TEMP-TABLE tt-arquivo-xml NO-UNDO
    FIELD nome             AS CHAR
    FIELD caminho-completo AS CHAR .


/*Mapeamento do XML*/
DEFINE TEMP-TABLE tt-nfe NO-UNDO
    FIELD chave-acesso AS CHAR
    FIELD ide-nnf      AS CHAR
    FIELD ide-serie    AS CHAR
    FIELD ide-natOp    AS CHAR
    FIELD emit-cnpj    AS CHAR
    FIELD emit-xNome   AS CHAR
    FIELD dest-cnpj    AS CHAR
    FIELD tot-vProd    AS DEC
    FIELD tot-vNF      AS DEC
    FIELD arquivo      AS CHAR
    FIELD sem-doc      AS LOG.


/*Itens do xml*/  
DEFINE TEMP-TABLE tt-nfe-item NO-UNDO
    FIELD chave-acesso AS CHAR
    FIELD ide-nnf     AS CHAR
    FIELD ide-serie   AS CHAR
    FIELD emit-cnpj   AS CHAR
    FIELD dest-cnpj   AS CHAR
    FIELD det-nitem   AS INT
    FIELD det-cprod   AS CHAR
    FIELD det-xprod   AS CHAR
    FIELD det-cfop    AS CHAR
    FIELD det-ucom    AS CHAR
    FIELD det-qcom    AS DEC
    FIELD det-vuncom  AS DEC
    FIELD det-vprod   AS DEC
    FIELD det-utrib   AS CHAR
    FIELD det-qtrib   AS DEC 
    FIELD det-vuntrib AS DEC. 

DEFINE TEMP-TABLE tt-erros-leitura-xml NO-UNDO
    FIELD nome             AS CHAR
    FIELD caminho-completo AS CHAR
    FIELD erro             AS CHAR.
    

/*Planilha principal*/
DEFINE TEMP-TABLE tt-planilha NO-UNDO
    /*XML*/
    FIELD chave-acesso AS CHAR
    FIELD ide-serie    AS CHAR
    FIELD ide-nnf      AS CHAR
    FIELD cnpj         AS CHAR
    FIELD xnome        AS CHAR
    FIELD det-nitem    AS INT
    FIELD det-cfop     AS CHAR
    FIELD det-cprod    AS CHAR
    FIELD det-xprod    AS CHAR
    FIELD det-vprod    AS DEC
    /*Dataul*/
    FIELD cod-estabel  AS CHAR
    FIELD serie        AS CHAR
    FIELD nr-docto     AS CHAR
    FIELD cod-emitente as INT
    FIELD nome-abrev   AS CHAR
    FIELD seq          AS INT
    FIELD it-codigo    AS CHAR
    FIELD nat-operacao AS CHAR
    FIELD preco-tot    AS DEC
    /*Status*/
    FIELD sit-seq      AS CHAR
    FIELD sit-preco    AS CHAR
    FIELD var-preco    AS DEC.
    


/*Documentos no datasul controle se ja foram usados*/
DEFINE TEMP-TABLE tt-item-doc-est-used LIKE item-doc-est
    FIELD chave-acesso AS CHAR
    FIELD usado        AS LOG
    FIELD det-nitem    AS INT
    FIELD preco-max    AS DEC
    FIELD preco-min    AS DEC.


/*Itens sem relaciinamento*/
DEFINE TEMP-TABLE tt-sem-relac NO-UNDO
    /*XML*/
    FIELD chave-acesso AS CHAR
    FIELD ide-serie    AS CHAR
    FIELD ide-nnf      AS CHAR
    FIELD cnpj         AS CHAR
    FIELD xnome        AS CHAR
    FIELD det-nitem    AS INT
    FIELD det-cfop     AS CHAR
    FIELD det-cprod    AS CHAR
    FIELD det-xprod    AS CHAR
    FIELD det-vprod    AS DEC .

DEFINE TEMP-TABLE tt-xml-sem-doc NO-UNDO
    FIELD chave-acesso AS CHAR
    FIELD emit-cnpj    as char
    FIELD emit-xnome   as char
    FIELD ide-serie    as char
    FIELD ide-nnf      as char
    FIELD ide-natOp    as char
    FIELD arquivo      as char
    FIELD com-principal AS LOG.



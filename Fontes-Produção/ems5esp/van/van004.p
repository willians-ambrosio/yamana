/****************************************************************************************** 
** 	   Programa: van004.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 27/09/2018
** Change/Chamado: 
**      Objetivo: Gera arquivo de Remessa de transf. entre contas para banco via van Accesstage
**                Segmentos A e B layout 240 posi��es - Bradesco
**
******************************** CONTROLE DE ALTERA��ES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri��o da Altera��o
**
**
****************************** INFORMA��ES ADICIONAIS ************************************
** PAR�METROS DE ENTRADA: Rowid da tabela de movimenta��o da conta corrente
** PAR�METROS DE SA�DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT  PARAMETER ipr-row-table AS ROWID NO-UNDO.
DEFINE INPUT  PARAMETER ipc-dir-rem   AS CHARACTER   NO-UNDO.

{van\van003.i}


/* Monta Header do Arquivo */
ASSIGN c-header =   STRING(INT(c-banco),"999")           /* Cod. Banco */
                  + "0000"                               /* Lote de Servi�o */
                  + '0'                                  /* Tipo de Servi�o */
                  + FILL(' ',9)                          /* Brancos */
                  + '2'                                  /* Tipo de Inscri��o - 1-CPF 2-CNPJ */
                  + STRING(DEC(c-cnpj),'99999999999999')              /* CNPJ/CPF */
                  + STRING(c-convenio,"x(20)")          /* Convenio */
                  + STRING(int(c-agencia),"99999")      /* Agencia */
                  + STRING(c-digAg,"X(01)")             /* Dig Ag */
                  + STRING(int(c-conta),"999999999999") /* Conta Corren */
                  + STRING(c-digCta,"X(01)")            /* Dig Conta */
                  + STRING(c-dig-agcB,'X(01)')          /* Dig Verificador Ag/Conta */
                  + STRING(c-nomEmp,'X(30)')            /* Nome Empresa - x(30) */
                  + STRING(c-nomBanco,'X(30)')          /* nome do Banco - x(30) */
                  + FILL(" ",10)                        /* Exclusivo Febraban */
                  + "1"                                 /* Cod. Remessa = 1 */
                  + STRING(TODAY,"99999999")            /* Data gera��o do arquivo */
                  + REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                  + STRING(i_num_rem,"999999") /* Prox Remessa */
                  + "089"                      /* Versao layout */
                  + "01600"                    /* Densidade */
                  + FILL(" ",20)               /* Reservado Banco*/
                  + FILL(" ",20)               /* Branco Reservado empresa */
                  + FILL(" ",29)               /* Reservado Febraban */.
                
/* Header do Lote */
ASSIGN c-headerLote =  STRING(INT(c-banco),"999")           /* Cod. Banco */
                      + "0001"                              /* Lote de servi�o */
                      + "1"                                 /* Tipo de Registro */
                      + "C"                                 /* Tipo de Opera��o C-Credito */
                      + "20"                                /* Pagamento a Fornecedor */
                      + STRING(INT(c-formalancto),"99")     /* Forma de Lan�amento */
                      + "045"                               /* Vers�o do layout */
                      + " "                                 /* Febraban */
                      + "2"                                 /* Tipo Inscri��o */
                      + STRING(DEC(c-cnpj),'99999999999999')    /* CNPJ/CPF */
                      + STRING(c-convenio,"x(20)")          /* Convenio */
                      + STRING(int(c-agencia),"99999")      /* Agencia */
                      + STRING(c-digAg,"X(01)")             /* Dig Ag */
                      + STRING(int(c-conta),"999999999999") /* Conta Corren */
                      + STRING(c-digCta,"X(01)")            /* Dig Conta */
                      + STRING(c-dig-agcB,'X(01)')          /* Dig Verificador Ag/Conta */
                      + STRING(c-nomEmp,'X(30)')            /* Nome Empresa - x(30) */
                      + FILL(" ",40)                        /* Mensagem */
                      + STRING(c-end,"X(30)")               /* Endere�o */
                      + "00000"                             /* N�mero */
                      + STRING(c-bairro,"x(15)")            /* Complemento */
                      + STRING(c-cidade,"X(20)")            /* Cidade */
                      + STRING(int(c-cep),"99999999")       /* CEP */
                      + STRING(c-uf,"X(02)")                /* UF */
                      + "01"                                /* Forma Pagto */
                      + FILL(" ",16).                       /* Febraban - 6 + Ocorrencia - 10*/

IF c-cta-fav = "8081-0"
     THEN ASSIGN c-cta-fav = "8081".                        .

/* Segmento A */
ASSIGN c-SegA =  STRING(INT(c-banco),"999")           /* Cod. Banco */       
                + "0001"                              /* Lote de servi�o */  
                + "3"                                 /* Tipo de Registro */ 
                + "00001"                             /* Sequencial do Lote */
                + "A"                                 /* Cod. do Segmento */
                + "0"                                 /* Tipo de movimento 0-Inclus�o 9-Exclus�o */
                + "00"                                /* Instru��o 00-Inclus�o 99-Exclus�o */
                + STRING(INT(c-camCen),"999")         /* Camara Centralizadora */                
                + STRING(INT(c-bco-favor),"999")      /* Banco Favorecido */
                + STRING(INT(c-ag-bcofav),"99999")    /* Ag Banco Favorecido */
                + STRING(c-div-agfav,"X(01)")         /* Digito Ag Favorecido */
                + STRING(DEC(REPLACE(c-cta-fav,"-","")),"999999999999") /* Conta Corrente */
                + STRING(c-dig-cta-fav,"X(01)")         /* Dig conta */
                + STRING(c-dig-agcta,"X(01)")           /* Dig AG Conta */
                + STRING(c-nomFav,"X(30)")              /* Nome Favorecido */
                + STRING(c-num_id_movto,"X(20)")        /* Numero do documento - id da movto_cta_corren */
                + STRING(INT(c-dt-pagto),"99999999")    /* data de pagamento */        
                + "BRL"                                 /* Tipo da moeda */
                + FILL("0",15)                              /* Qtd na moeda */
                + STRING(DEC(c-vl-pagto),"999999999999999") /* Valor do Pagamento */
                + FILL(" ",20)                              /* Docto do Banco */
                + "00000000"                                /* Dt Efetiva��o do pagto */
                + FILL("0",15)                              /* Valor Real Pago */
                + FILL(" ",40)                              /* Outras Informa��es - 40 */
                + "  " /*STRING(c-compl-tp-servico,"X(02)") */       /* 218 a 219 - Brancos - + Compl Tipo de Servi�o - 2*/
                + STRING(c-cod-finalid-ted,"X(05)")          /* Finalidade TED */
                + STRING(c-compl-finalid-pagto,"X(02)")      /* Finalidade Pagto */
                + FILL(" ",3)                                /* CNAB */
                + "0"                                        /* Aviso Fornec */
                + FILL(" ",10).                              /* Ocorrencias */

ASSIGN c-SegB = STRING(INT(c-banco),"999")           /* Cod. Banco */                             
               + "0001"                              /* Lote de servi�o */                        
               + "3"                                 /* Tipo de Registro */                       
               + "00002"                             /* Sequencial do Lote */                     
               + "B"                                 /* Cod. do Segmento */                       
               + FILL(" ",3)                         /* Febraban */
               + "2"                                 /* Tipo Inscri��o */
               + STRING(DEC(c-cnpj-Fav),"99999999999999") /* Inscri��o */
               + STRING(c-endFav,'x(30)')                 /* End favorecido */
               + "00000"                                  /* nr do end */
               + FILL(" ",15)                             /* Complemento */
               + STRING(c-bairroFav,"X(15)")              /* Bairro Fav */
               + STRING(c-cidadeFav,"X(20)")              /* Cidade fav */
               + STRING(INT(c-cepFav),"99999999")         /* CEP */
               + STRING(c-ufFav,"X(02)")                  /* UF */
               + STRING(INT(c-dt-pagto),"99999999")       /* Data Vencto */
               + STRING(DEC(c-vl-pagto),"999999999999999") /* Valor do Pagamento */
               + FILL("0",60)                             /* Vr Abatimento, Vr Desconto, Vr Mora, Multa */
               + FILL(" ",15)                             /* Documento Favorecido */
               + "0"                                      /* Aviso ao favorecido */
               + FILL(" ",6)                              /* SIAPE */
               + FILL(" ",8)                              /* Febraban */.

/* Trailler do Lote */
ASSIGN c-traLote = STRING(INT(c-banco),"999")            /* Cod. Banco */         
                   + "0001"                              /* Lote de servi�o */    
                   + "5"                                 /* Tipo de Registro */   
                   + FILL(" ",9)                         /* Febraban */           
                   + "000004"                            /* Qtd Reg Lote - O Zero ao final � para complementar as posi��es do campo de valor 16,2*/ 
                   + STRING(DEC(c-vl-pagto),"999999999999999999") /* Total Valores */
                   + FILL("0",18)                                 /* Qtd Moedas */
                   + FILL("0",6)                                  /* Nr Aviso de D�bito */
                   + FILL(" ",175).                               /* Febraban - 165 + Ocor retorno - 10 */

/* Trailler do Arquivo */
ASSIGN c-trailler = STRING(INT(c-banco),"999")  /* Cod. Banco */            
                    + "9999"                    /* Lote de servi�o */      
                    + "9"                       /* Tipo de Registro */     
                    + FILL(" ",9)               /* Febraban */             
                    + "000001"                  /* Qtd Lote */ 
                    + "000006"                  /* Qtd Registro */
                    + "000000"                  /* Qtd Contas */
                    + FILL(" ",205)             /* Febraban */  .
            
RUN pi-exporta.

RETURN RETURN-VALUE.


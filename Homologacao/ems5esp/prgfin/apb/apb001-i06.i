/****************************************************************************************** 
**         Programa: apb001-i06.i
**            Autor: Vando Ribeiro
**       Fornecedor: DKP
**       Data: 05/11/2018
** Change/Chamado: REQ04
**    Objetivo: Cabe�alhos das colunas dos movtoa aprovados na prepara��o de e-mail.
**
******************************** CONTROLE DE ALTERA��ES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descri��o da Altera��o
**
**

****************************** INFORMA��ES ADICIONAIS ************************************
** PAR�METROS DE ENTRADA: N/A
** PAR�METROS DE SA�DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

c-mess-top = "<html>"                                                                          
             + "<head>" 
             + "<style>"
             + "table, th, td"
             + "~{"
             + "border: 1px solid black;"
             + "border-collapse: collapse;"
             + "text-align: CENTER;"
             + "}"
             + "</style>"
             + "</head>"                                                                      
             + "<body>"
             + "<p>Prezado(a)(s),<p>"
             + "<p> Segue abaixo a lista dos movimentos aprovados:</p>"
             + "<TABLE style=~"width:100%~">"                                                                      
             + "<tr>"                                                                         
             + "<th>" + "Conta Corrente" + "</th>"                                                   
             + "<th>" + "Data Movto" + "</th>"                                                   
             + "<th>" + "Sequ�ncia" + "</th>"                                              
             + "<th>" + "Fluxo Movto" + "</th>"                                                    
             + "<th>" + "Valor Movto" + "</th>"                                                    
             + "<th>" + "Data Transa��o" + "</th>"                                                    
             + "<th>" + "Justificativa" + "</th>"                                               
             + "<th>" + "Usu Digita��o" + "</th>"                                                
             + "<th>" + "Data Digita��o" + "</th>"                                    
             + "</tr>".

c-mess-base = "</table>"                                                        
         + "<p>&nbsp;</p>"                                                   
         + "<p>" + "At," + "</p>"                                            
         + "<p>" + "<strong>" + "Susten��o Yamana." + "</strong>" + "</p>"   
         + "</body>"                                                         
         + "</html>" .                                                       


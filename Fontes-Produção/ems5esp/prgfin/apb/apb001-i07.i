/****************************************************************************************** 
**         Programa: apb001-i07.i
**            Autor: Vando Ribeiro
**       Fornecedor: DKP
**       Data: 05/11/2018
** Change/Chamado: REQ04
**    Objetivo: Cabeáalhos das colunas dos titulos reprovados na preparaá∆o de e-mail.
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**

****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
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
             + "<p> Segue abaixo a lista dos t°tulos reprovados:</p>"
             + "<TABLE style=~"width:100%~">"                                                                      
             + "<tr>"                                                                         
             + "<th>" + "Esp" + "</th>"                                                   
             + "<th>" + "Estab" + "</th>"                                                   
             + "<th>" + "Fornecedor" + "</th>"                                              
             + "<th>" + "T°tulo" + "</th>"                                                    
             + "<th>" + "SÇr" + "</th>"                                                    
             + "<th>" + "Esp" + "</th>"                                                    
             + "<th>" + "Par" + "</th>"                                                    
             + "<th>" + "Valor" +  "</th>"                                                   
             + "<th>" + "Data Emiss∆o" + "</th>"                                            
             + "<th>" + "Data Vencimento" + "</th>"                                         
             + "<th>" + "Justificativa" + "</th>"                                               
             + "<th>" + "Usu Digitaá∆o" + "</th>"                                                
             + "<th>" + "Dt Digitaá∆o" + "</th>"                                    
             + "</tr>".

c-mess-base = "</table>"
             + "<p>&nbsp;</p>"
             + "<p>" + "<strong>" + "VOC“ DEVE EXCLUIR O(S) T÷TULO(S) OU ALTERAR E SUBMETER A NOVA APROVAÄ«O." + "</strong>" + "</p>"
             + "<p>" + "</p>"
             + "<p>" + "At," + "</p>"
             + "<p>" + "<strong>" + "Sustená∆o Yamana." + "</strong>" + "</p>"
             + "</body>"
             + "</html>".

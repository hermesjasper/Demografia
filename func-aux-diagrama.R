
inicio_ano <- function(ano){ return(as.Date(str_c(ano,"-01-2"))) }
meio_ano <- function(ano){ return(as.Date(str_c(ano,"-06-01"))) }
final_ano <- function(ano){ return(as.Date(str_c(ano,"-12-30"))) }
inicio_idade <- function(idade){ return(idade + 0.01) }
final_idade <- function(idade){ return(idade - 0.01) }

#________________________________________________________ Função que apaga uma quadrado no ano e idade desejado

apaga_quadrado <- function(diagram, ano, idade){
  
  losangulos <- data.frame(group = c(1, 1, 1, 2, 2, 2),
                           x = c(inicio_ano(as.character(ano)), final_ano(as.character(ano)), final_ano(as.character(ano)),
                                 inicio_ano(as.character(ano)), final_ano(as.character(ano)), inicio_ano(as.character(ano))),
                           y = c(inicio_idade(idade), inicio_idade(idade), final_idade(idade + 1),
                                 final_idade(idade + 1), final_idade(idade + 1), inicio_idade(idade)))
  
  linhas <- data.frame(inicio = c(inicio_ano(as.character(ano))), 
                       fim = c(final_ano(as.character(ano))))
  
  templot <- lexis_polygon(diagram, x = losangulos$x,
                           y = losangulos$y, group = losangulos$group,
                           fill = "white", alpha = 1) %>%
    lexis_lifeline(birth = linhas$inicio, exit = linhas$fim, lwd = 0.3, colour = "black")
  
  return(templot)
}

#_______________________________________________________ Função que cria uma quadrado no ano e idade desejado da cor desejada

cria_quadrado <- function(diagram, ano, idade, cor){
  
  
  losangulos <- data.frame(group = c(1, 1, 1, 2, 2, 2),
                           x = c(inicio_ano(as.character(ano)), final_ano(as.character(ano)), final_ano(as.character(ano)),
                                 inicio_ano(as.character(ano)), final_ano(as.character(ano)), inicio_ano(as.character(ano))),
                           y = c(inicio_idade(idade), inicio_idade(idade), final_idade(idade + 1),
                                 final_idade(idade + 1), final_idade(idade + 1), inicio_idade(idade)))
  
  linhas <- data.frame(inicio = c(inicio_ano(as.character(ano))), 
                       fim = c(final_ano(as.character(ano))))
  
  templot <- lexis_polygon(diagram, x = losangulos$x,
                           y = losangulos$y, group = losangulos$group,
                           fill = cor, alpha = 1) %>%
    lexis_lifeline(birth = linhas$inicio, exit = linhas$fim, lwd = 0.3, colour = "black")
  
  return(templot)
}

#_______________________ Função que cria um poligono na parte 'inferior' ou 'superior' do quadrado no ano e idade desejado da cor desejada

cria_poligono <- function(diagram, tipo, ano, idade, cor, delta, alpha){
  final_ano <- final_ano(as.character(ano)) + 364*(delta-1)
  final_idade <- final_idade(idade + delta)
  
  losangulos <- data.frame(group = c(1, 1, 1,2,2,2),
                           x = c(inicio_ano(as.character(ano)), final_ano,  final_ano,
                                 inicio_ano(as.character(ano)), final_ano, inicio_ano(as.character(ano))),
                           y = c(inicio_idade(idade), inicio_idade(idade), final_idade,
                                 final_idade, final_idade, inicio_idade(idade)))
  
  linhas <- data.frame(inicio = c(inicio_ano(as.character(ano)) - 364*idade), 
                       fim = c(final_ano))
  
  if (tipo == "inferior"){
    
    templot <- lexis_polygon(diagram, x = losangulos$x[1:3],
                             y = losangulos$y[1:3], group = losangulos$group[1:3],
                             fill = cor, alpha = alpha) %>%
      lexis_lifeline(birth = linhas$inicio, exit = linhas$fim, lwd = 0.3, colour = "black", alpha= alpha)
  }
  else{
    
    templot <- lexis_polygon(diagram, x = losangulos$x[4:6],
                             y = losangulos$y[4:6], group = losangulos$group[4:6],
                             fill = cor, alpha = alpha) %>%
      lexis_lifeline(birth = linhas$inicio, exit = linhas$fim, lwd = 0.3, colour = "black", alpha = alpha)
    
  }
  
  return(templot)
}

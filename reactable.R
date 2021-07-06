# Coloca o local atual como padrão (depende do RStudio como IDE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
a = read.csv("atrasados-dados.csv", sep=",", encoding="UTF-8", header = TRUE, strip.white=TRUE)

a$Previsao <- paste0("01/",a$Previsao)
a$Previsao <- as.Date(a$Previsao,"%d/%m/%Y")
# a$Previsao.Tabela <- strftime(a$Previsao, "%b/%Y")
a$Previsao.Tabela <- a$Previsao

library(lubridate)

a$Previsao <- a$Previsao %m+% months(1)
a$Previsao <- a$Previsao - days(1)
a$Comunicacao <- as.Date(a$Comunicacao,"%d/%m/%Y")
a$Atraso <- as.numeric(difftime(Sys.Date(),a$Previsao))
# a$Atraso <- as.numeric(a$Atraso)
# b <- ymd(a$Comunicacao)
a$Dif.Comunicacao <- ifelse(difftime(Sys.Date(),a$Comunicacao)>120,"Não","Sim")
# a$Dif.Comunicacao <- as.numeric(difftime(Sys.Date(),a$Comunicacao))
a <- a[,c(1,2,3,4,8,9,5,10,6,7)]



library(reactable)
library(htmlwidgets)

reactable(a, compact=FALSE, filterable=FALSE, searchable = TRUE, defaultPageSize = 30,highlight = FALSE,defaultSortOrder="desc",defaultSorted="Atraso",
          language = reactableLang(
            noData = "Nenhum registro encontrado",
            searchPlaceholder = "Pesquisar",
            pageInfo = "{rowStart}\u2013{rowEnd} de {rows} registros",
            pageNext = "Próximo",
            pagePrevious = "Anterior"
          ),
          columns = list(
            Nome = colDef(cell = function(value, index){
              url <- a$Link[index]
              htmltools::tags$a(href = url, target = "_blank", as.character(value))
            },
              style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1,display = "flex", flexDirection = "column", justifyContent = "center"),
              headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, display = "flex", flexDirection = "column", alignSelf = "flex-end")),
            Link = colDef(show=FALSE),
            Autor = colDef(align = "center", style = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                           headerStyle = list(display = "flex", flexDirection = "column", alignSelf = "flex-end")),
            Previsao.Tabela = colDef(
              name = "Previsão de Entrega",
              align = "center",
              style = list(display = "flex", flexDirection = "column", justifyContent = "center"),
              headerStyle = list(display = "flex", flexDirection = "column", alignSelf = "flex-end"),
              cell = function(value) {
                strftime(value, "%b/%Y")}),
            Atraso = colDef(name = "Atraso<br>(dias)",
                            align = "center",
                            style = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                            headerStyle = list(display = "flex", flexDirection = "column", alignSelf = "flex-end"),
                            minWidth = 80,
                            cell = JS("function(cellInfo) {
                                      var today = new Date();
                                      var dd = String(today.getDate()).padStart(2, '0');
                                      var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
                                      var yyyy = today.getFullYear();
                                      today = yyyy + '-' + mm + '-' + dd;
      
                                      var date1 = new Date(today);
                                      var date2 = new Date(cellInfo.row['Previsao']);
      
                                      var diff = date1 - date2;
                                      var dias = Math.floor(diff/86400000);
      
                                      return '<b>' + dias + '</b>'
                                      }"
                            ),html = TRUE),
            Comunicacao = colDef(
              name = "Última Comunicação",
              align = "center",
              style = list(display = "flex", flexDirection = "column", justifyContent = "center"),
              headerStyle = list(display = "flex", flexDirection = "column", alignSelf = "flex-end"),
              minWidth = 110,
              cell = function(value) {
                strftime(value, "%d/%m/%Y")}),
            Previsao = colDef(
              show = FALSE
            ),
            Dif.Comunicacao = colDef(
              name = "Comunicação<br>(em 120 dias)",
              align = "center",
              resizable = FALSE,
              style = list(display = "flex", flexDirection = "column", justifyContent = "center"),
              headerStyle = list(display = "flex", flexDirection = "column", alignSelf = "flex-end"),
              cell = JS("function(cellInfo) {
                        var today = new Date();
                        var dd = String(today.getDate()).padStart(2, '0');
                        var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
                        var yyyy = today.getFullYear();
                        today = yyyy + '-' + mm + '-' + dd;
      
                        var date1 = new Date(today);
                        var date2 = new Date(cellInfo.row['Comunicacao']);
      
                        var diff = date1 - date2;
                        var dias = Math.floor(diff/86400000);
                        
                        if (dias > 120) {
                           return 'Não'
                        } else {
                           return 'Sim'
                        }
      
                 }"),
              html = TRUE),
            Apoiadores = colDef(
              align = "center",
              style = list(display = "flex", flexDirection = "column", justifyContent = "center"),
              headerStyle = list(display = "flex", flexDirection = "column", alignSelf = "flex-end")
            ),
            Arrecadado = colDef(
              name = "Valor Arrecadado",
              align = "center",
              style = list(display = "flex", flexDirection = "column", justifyContent = "center"),
              headerStyle = list(display = "flex", flexDirection = "column", alignSelf = "flex-end"),
              format = colFormat(prefix = "R$ ", separators = TRUE, digits = 0))))


spatial1 <- function(x){
  archivos <- list.files(x, pattern = "\\.xlsx$", full.names = TRUE)
  
  extraccion <- function(y){
    db <- read_xlsx(y, skip = 10, col_names = F)
    names(db) <- paste0("X", seq(1:16))
    
    db %>% 
      mutate(
        cuenca = str_extract(y, "(?<=/)(.*?)(?=.xlsx)"),
        categoria = X16)%>% 
      select(X1, X7, X8, cuenca, X12, X13, X14, categoria) %>% 
      rename_with(~ c("aaa","codigo", "descripcion", "cuenca", "zona", "este", "norte", "categoria"),
                  everything())
  }
  
  lista <- map(archivos, extraccion)
  bind_rows(lista)}
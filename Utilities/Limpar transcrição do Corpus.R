library(tcltk)

pasta_entrada <- tk_choose.dir(caption = "Selecione a pasta de entrada")
pasta_saida <- tk_choose.dir(caption = "Selecione a pasta de saída")

if (is.na(pasta_entrada) || is.na(pasta_saida)) {
  stop("Seleção de pasta cancelada. O script será encerrado.")
}

limpar_linha <- function(linha) {
  linha_processada <- linha # Começa com a linha original
  

  linha_processada <- gsub("(?<=\\S)(//)", " \\1", linha_processada, perl = TRUE)  # Ex: palavra// -> palavra //
  linha_processada <- gsub("(//)(?=\\S)", "\\1 ", linha_processada, perl = TRUE)  # Ex: //palavra -> // palavra
  
 
  linha_processada <- gsub("(?<=[^\\s/])(/)(?![/])", " \\1", linha_processada, perl = TRUE) # Ex: palavra/ -> palavra / (mas não em //)
  linha_processada <- gsub("(?<![/])(/)(?=[^\\s/])", "\\1 ", linha_processada, perl = TRUE) # Ex: /palavra -> / palavra (mas não em //)

  termos_remover_marcadores <- c(
    "\\*ent8:", "\\*ent9:",   # Marcadores com números e dois pontos (do pedido anterior)
    "\\*inf1:", "\\*inf2:",   # Marcadores novos/reforçados
    "\\*inf:",                # Marcador *inf com dois pontos
    "inf:",                   # Marcador inf com dois pontos (sem asterisco)
    "\\*ent",                 # Marcador *ent genérico (sem dois pontos)
    "\\*inf"                  # Marcador *inf genérico (sem dois pontos)
  )
  padrao_marcadores <- paste(termos_remover_marcadores, collapse = "|")
  linha_processada <- gsub(padrao_marcadores, "", linha_processada, ignore.case = TRUE, perl = TRUE)
  
  linha_processada <- gsub("[<>\\[\\]\"\\(\\)_“”]", "", linha_processada, perl = TRUE)
  
  mapa_substituicao_numeros <- c(
    "\\b0\\b"  = "zero",
    "\\b2\\b"  = "dois",
    "\\b5\\b"  = "cinco",
    "\\b12\\b" = "doze",
    "\\b15\\b" = "quinze",
    "\\b25\\b" = "vinte e cinco"
  )
  for (padrao_num_regex in names(mapa_substituicao_numeros)) {
    substituicao_texto <- mapa_substituicao_numeros[[padrao_num_regex]]
    linha_processada <- gsub(padrao_num_regex, substituicao_texto, linha_processada, perl = TRUE)
  }
  
  linha_processada <- gsub("\\s+", " ", linha_processada) 
  linha_processada <- trimws(linha_processada)
  
  return(linha_processada)
}

# --- Processamento dos Arquivos ---
arquivos_txt <- list.files(pasta_entrada, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)

if (length(arquivos_txt) == 0) {
  cat("Nenhum arquivo .txt encontrado na pasta de entrada:", pasta_entrada, "\n")
} else {
  cat("Processando", length(arquivos_txt), "arquivo(s) .txt...\n")
  
  for (arquivo in arquivos_txt) {
    nome_base <- basename(arquivo)
    caminho_saida_arquivo <- file.path(pasta_saida, nome_base)
    
    linhas <- tryCatch({
      readLines(arquivo, warn = FALSE, encoding = "UTF-8")
    }, error = function(e) {
      message(paste("Erro ao ler o arquivo", nome_base, "com UTF-8. Tentando com codificação padrão do sistema. Erro:", e$message))
      tryCatch({
        readLines(arquivo, warn = FALSE)
      }, error = function(e2) {
        message(paste("Erro ao ler o arquivo", nome_base, "com codificação padrão também. Pulando arquivo. Erro:", e2$message))
        return(NULL) 
      })
    })
    
    if (is.null(linhas)) {
      next 
    }
    
    linhas_limpas <- sapply(linhas, limpar_linha)
    
    linhas_limpas <- linhas_limpas[linhas_limpas != ""] # Remove linhas que ficaram completamente vazias
    
    if (length(linhas_limpas) > 0) {
      out_con <- file(caminho_saida_arquivo, "w", encoding = "UTF-8")
      writeLines(linhas_limpas, out_con, sep = "\n")
      close(out_con)
      cat("Arquivo limpo salvo em:", caminho_saida_arquivo, "\n")
    } else {
      cat("Arquivo", nome_base, "resultou em conteúdo vazio após a limpeza. Nenhum arquivo de saída gerado para ele.\n")
    }
  }
  
  cat("\nProcessamento concluído para", length(arquivos_txt), "arquivo(s).\n")
}
selecionar_pasta <- function(titulo_prompt) {
  caminho_pasta <- NULL
  
  if (Sys.getenv("RSTUDIO") == "1" && nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      tryCatch({
        message(paste("Abrindo diálogo para:", titulo_prompt))
        caminho_pasta_temp <- rstudioapi::selectDirectory(caption = titulo_prompt)
        if (!is.null(caminho_pasta_temp)) {
          caminho_pasta <- normalizePath(caminho_pasta_temp)
        }
      }, error = function(e) {
        message("Falha ao usar rstudioapi::selectDirectory. Usando entrada manual no console.")
      })
    } else {
      message("Pacote rstudioapi não encontrado ou RStudio não está respondendo. Usando entrada manual no console.")
    }
  }
  
  if (is.null(caminho_pasta)) {
    cat(paste0(titulo_prompt, " (ex: /caminho/para/pasta ou C:\\caminho\\pasta).\n"))
    cat("Deixe em branco e pressione Enter para cancelar.\n")
    caminho_pasta_input <- readline(prompt = "Digite o caminho da pasta: ")
    
    if (caminho_pasta_input != "") {
      caminho_pasta_normalizado <- normalizePath(caminho_pasta_input, mustWork = FALSE)
      if (dir.exists(caminho_pasta_normalizado)) {
        caminho_pasta <- caminho_pasta_normalizado
      } else {
        message(paste("ERRO: A pasta fornecida não existe:", caminho_pasta_input))
        return(NULL)  
      }
    } else {
      message("Seleção de pasta cancelada pelo usuário.")
      return(NULL) 
    }
  }
  
  if (!is.null(caminho_pasta) && !dir.exists(caminho_pasta)) {
    message(paste("ERRO: A pasta selecionada não é válida ou não existe:", caminho_pasta))
    return(NULL)
  }
  
  return(caminho_pasta)
}

processar_arquivo <- function(caminho_arquivo) {
  texto_completo <- tryCatch({
    linhas <- readLines(caminho_arquivo, warn = FALSE, encoding = "UTF-8")
    paste(linhas, collapse = " ") # Junta as linhas com um espaço entre elas
  }, error = function(e) {
    message(paste("Erro ao ler o arquivo:", basename(caminho_arquivo), "-", e$message))
    return(NULL) # Retornar NULL em caso de erro para ser tratado depois
  })
  
  if (is.null(texto_completo) || nchar(trimws(texto_completo)) == 0) {
    return(character(0)) # Retornar vetor de caracteres vazio
  }
  
  texto_minusculo <- tolower(texto_completo)
  
  palavras <- unlist(strsplit(texto_minusculo, "\\s+")) # Usar \\s+ para lidar com múltiplos espaços
  
  palavras <- gsub("[^[:alnum:]]", "", palavras)
  
  palavras <- palavras[palavras != ""] 
  
  return(palavras)
}

gerar_lista_palavras <- function(caminhos_arquivos) {
  todas_as_palavras <- c()
  for (caminho in caminhos_arquivos) {
    message(paste("Processando arquivo:", caminho)) 
    palavras_do_arquivo <- processar_arquivo(caminho)
    todas_as_palavras <- c(todas_as_palavras, palavras_do_arquivo)
  }
  
  if (length(todas_as_palavras) == 0) {
    message("Nenhuma palavra foi extraída dos arquivos fornecidos.")
    return(character(0))
  }
  
  # A remoção de duplicatas acontece aqui, APÓS a limpeza correta
  palavras_unicas <- unique(todas_as_palavras)
  palavras_ordenadas <- sort(palavras_unicas)
  return(palavras_ordenadas)
}


cat("--- Seleção da Pasta de Entrada ---\n")
pasta_entrada <- selecionar_pasta("Selecione a PASTA contendo os arquivos de transcrição (.txt)")

if (is.null(pasta_entrada)) {
  stop("Nenhuma pasta de entrada selecionada ou pasta inválida. O script será encerrado.", call. = FALSE)
}
message(paste("\nPasta de entrada selecionada:", pasta_entrada))
message("Procurando arquivos .txt, incluindo subpastas...")

arquivos_txt <- list.files(
  path = pasta_entrada,
  pattern = "\\.txt$",   
  full.names = TRUE,     
  ignore.case = TRUE,    
  recursive = TRUE,      
  all.files = TRUE       
)

if (length(arquivos_txt) == 0) {
  stop(paste("Nenhum arquivo .txt encontrado na pasta:", pasta_entrada, "(incluindo subpastas). O script será encerrado."), call. = FALSE)
}

message(paste("\nTotal de arquivos .txt encontrados (incluindo subpastas):", length(arquivos_txt)))
message("Arquivos que serão processados:")
for(arquivo in arquivos_txt) {
  message(paste(" - ", arquivo)) 
}

message("\n--- Processando Arquivos ---")
lista_final_palavras <- gerar_lista_palavras(arquivos_txt)

if (length(lista_final_palavras) == 0) {
  message("\nNenhuma palavra foi extraída dos arquivos. Nenhum arquivo de saída será gerado.")
} else {
  message(paste("\nTotal de palavras únicas e limpas encontradas:", length(lista_final_palavras)))
  
  cat("\n--- Seleção da Pasta de Saída ---\n")
  pasta_saida <- selecionar_pasta("Selecione a PASTA para SALVAR o arquivo com a lista de palavras")
  
  if (is.null(pasta_saida)) {
    message("\nNenhuma pasta de saída selecionada ou pasta inválida.")
    message("A lista de palavras será impressa no console:")
    cat("\n--- Lista de Palavras Únicas e Limpas (em ordem alfabética) ---\n")
    print(lista_final_palavras)
  } else {
    message(paste("\nPasta de saída selecionada:", pasta_saida))
    nome_arquivo_saida <- "lista_palavras_unicas_limpas.txt" 
    caminho_arquivo_saida <- file.path(pasta_saida, nome_arquivo_saida)
    
     tryCatch({
      # Usar file() com encoding explícito é a forma mais robusta de salvar
      arquivo_conexao <- file(caminho_arquivo_saida, "w", encoding = "UTF-8")
      writeLines(lista_final_palavras, con = arquivo_conexao)
      close(arquivo_conexao)
      
      message(paste("\n--- Sucesso ---"))
      message(paste("Lista de palavras salva com sucesso em:", caminho_arquivo_saida))
    }, error = function(e) {
      message(paste("\n--- Erro ao Salvar Arquivo ---"))
      message(paste("Não foi possível salvar o arquivo em:", caminho_arquivo_saida))
      message(paste("Erro:", e$message))
      message("A lista de palavras será impressa no console como alternativa:")
      cat("\n--- Lista de Palavras Únicas e Limpas (em ordem alfabética) ---\n")
      print(lista_final_palavras)
    })
  }
}

message("\n--- Script Concluído ---")
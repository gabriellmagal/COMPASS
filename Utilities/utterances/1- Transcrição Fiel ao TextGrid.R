if (!requireNamespace("tcltk", quietly = TRUE)) install.packages("tcltk")
library(tcltk)
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)

# --- 1. FUNÇÕES AUXILIARES ---
selecionar_pasta <- function(titulo = "Selecione uma pasta") {
  caminho <- NA; tryCatch({ if (Sys.getenv("RSTUDIO") == "1" && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) { caminho <- rstudioapi::selectDirectory(caption = titulo) } else { if (requireNamespace("tcltk", quietly = TRUE)) { caminho <- tcltk::tclvalue(tcltk::tkchooseDirectory(title = titulo)) } else { stop("Nenhum GUI disponível.") } } }, error = function(e) { message("Erro: ", e$message); return(NULL) }); if (is.null(caminho) || is.na(caminho) || nchar(caminho) == 0) return(NULL); return(normalizePath(caminho))
}

parse_textgrid_words <- function(filepath) {
  tryCatch({
    lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
    
    word_tier_header_index <- which(grepl('name = "words"', tolower(lines), fixed = TRUE))
    if (length(word_tier_header_index) == 0) return(NULL)
    start_block_indices <- grep("item \\[[0-9]+\\]:", lines)
    current_tier_block_index <- findInterval(word_tier_header_index, start_block_indices)
    tier_start_line <- start_block_indices[current_tier_block_index]
    tier_end_line <- ifelse(current_tier_block_index < length(start_block_indices), start_block_indices[current_tier_block_index + 1] - 1, length(lines))
    tier_lines <- lines[tier_start_line:tier_end_line]
    interval_starts <- grep("intervals \\[[0-9]+\\]:", tier_lines)
    if(length(interval_starts) == 0) return(data.frame())
    labels <- gsub('^\\s*text = "(.*)"\\s*$', "\\1", tier_lines[interval_starts + 3])
    return(data.frame(label = labels, stringsAsFactors = FALSE))
  }, error = function(e) {
    warning(paste("Erro ao parsear o TextGrid:", basename(filepath), e$message))
    return(NULL)
  })
}

sanitizar_palavra <- function(palavra) {
  return(tolower(gsub("[^[:alnum:]]", "", palavra)))
}


# --- 2. SCRIPT PRINCIPAL ---
cat("Este script gera novas transcrições (.txt) que são fiéis aos TextGrids alinhados.\n\n")

pasta_textgrids <- selecionar_pasta(titulo = "SELECIONE A PASTA COM OS TEXTGRIDS ALINHADOS")
if (is.null(pasta_textgrids)) stop("Operação cancelada.")

pasta_transcricoes_originais <- selecionar_pasta(titulo = "SELECIONE A PASTA COM AS TRANSCRIÇÕES ORIGINAIS")
if (is.null(pasta_transcricoes_originais)) stop("Operação cancelada.")

pasta_saida <- selecionar_pasta(titulo = "SELECIONE UMA PASTA DE SAÍDA para as novas transcrições")
if (is.null(pasta_saida)) stop("Operação cancelada.")
if (pasta_transcricoes_originais == pasta_saida) stop("ERRO: A pasta de entrada e a de saída não podem ser a mesma.")

# --- PROCESSAMENTO ---
dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)
arquivos_tg_nomes <- list.files(path = pasta_textgrids, pattern = "\\.TextGrid$")
if (length(arquivos_tg_nomes) == 0) { stop("ERRO: Nenhum arquivo .TextGrid encontrado.") }

cat(paste("\nEncontrados", length(arquivos_tg_nomes), "arquivos para processar...\n\n"))

for (nome_arquivo_tg in arquivos_tg_nomes) {
  cat("Processando:", nome_arquivo_tg, "\n")
  
  tryCatch({
    nome_base_sem_ext <- tools::file_path_sans_ext(nome_arquivo_tg)
    
    # Encontra a transcrição original correspondente
    caminho_transcricao <- NULL
    extensao_transcricao <- ""
    caminho_txt <- file.path(pasta_transcricoes_originais, paste0(nome_base_sem_ext, ".txt"))
    caminho_lab <- file.path(pasta_transcricoes_originais, paste0(nome_base_sem_ext, ".lab"))
    if(file.exists(caminho_txt)){ 
      caminho_transcricao <- caminho_txt
      extensao_transcricao <- ".txt"
    } else if(file.exists(caminho_lab)){ 
      caminho_transcricao <- caminho_lab
      extensao_transcricao <- ".lab"
    }
    
    if (is.null(caminho_transcricao)) {
      cat(paste("  -> AVISO: Transcrição original não encontrada. Pulando.\n"))
      next
    }
    
    caminho_tg <- file.path(pasta_textgrids, nome_arquivo_tg)
    tabela_palavras_tg <- parse_textgrid_words(caminho_tg)
    if(is.null(tabela_palavras_tg) || nrow(tabela_palavras_tg) == 0){
      cat(paste("  -> AVISO: TextGrid vazio ou inválido. Pulando.\n"))
      next
    }
    palavras_reais_no_tg <- tabela_palavras_tg$label[tabela_palavras_tg$label != ""]
    
    texto_original_completo <- paste(readLines(caminho_transcricao, warn = FALSE, encoding = "UTF-8"), collapse = " ")
    tokens_originais <- unlist(strsplit(texto_original_completo, "(?<=\\s)|(?=\\s)|(?<=[/\\?&\\+])|(?=[/\\?&\\+])", perl = TRUE))
    tokens_originais <- tokens_originais[nchar(tokens_originais) > 0]
    
    nova_transcricao_tokens <- c()
    idx_palavra_tg <- 1 
    
    for (token in tokens_originais) {
      token_sanitizado <- sanitizar_palavra(token)
      
      if (nchar(token_sanitizado) == 0) {
        nova_transcricao_tokens <- c(nova_transcricao_tokens, token)
        next
      }
      
      if (idx_palavra_tg <= length(palavras_reais_no_tg) && 
          token_sanitizado == sanitizar_palavra(palavras_reais_no_tg[idx_palavra_tg])) {
        
        nova_transcricao_tokens <- c(nova_transcricao_tokens, token)
        idx_palavra_tg <- idx_palavra_tg + 1
      }
    }
    
    texto_final_fiel <- paste(nova_transcricao_tokens, collapse = "")
    texto_final_fiel <- trimws(gsub("\\s+", " ", texto_final_fiel))
    
    nome_arquivo_saida <- paste0(nome_base_sem_ext, extensao_transcricao)
    caminho_saida_txt <- file.path(pasta_saida, nome_arquivo_saida)
    # Escreve o novo arquivo no formato universal UTF-8
    writeLines(texto_final_fiel, con = file(caminho_saida_txt, encoding="UTF-8"), sep = "")
    
  }, error = function(e) {
    cat(paste("  ERRO FATAL NO PROCESSAMENTO do arquivo", nome_arquivo_tg, ":", e$message, "\n"))
  })
}

cat("\n--- Processo Concluído! ---\n")
cat("As novas transcrições fiéis foram salvas em:", pasta_saida, "\n")
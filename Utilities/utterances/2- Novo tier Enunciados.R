if (!requireNamespace("tcltk", quietly = TRUE)) install.packages("tcltk")
library(tcltk)
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)

# --- Funções Auxiliares ---
sanitizar_palavra <- function(palavra) {
  return(tolower(gsub("[^[:alnum:]]", "", palavra)))
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
    
    starts <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_starts + 1]))
    ends <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_starts + 2]))
    labels <- gsub('^\\s*text = "(.*)"\\s*$', "\\1", tier_lines[interval_starts + 3])
    
    return(data.frame(start = starts, end = ends, label = labels, stringsAsFactors = FALSE))
  }, error = function(e) {
    warning(paste("Erro ao parsear o TextGrid:", basename(filepath), e$message))
    return(NULL)
  })
}
selecionar_pasta <- function(titulo = "Selecione uma pasta") {
  caminho <- NA; tryCatch({ if (Sys.getenv("RSTUDIO") == "1" && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) { caminho <- rstudioapi::selectDirectory(caption = titulo) } else { if (requireNamespace("tcltk", quietly = TRUE)) { caminho <- tcltk::tclvalue(tcltk::tkchooseDirectory(title = titulo)) } else { stop("Nenhum GUI disponível.") } } }, error = function(e) { message("Erro: ", e$message); return(NULL) }); if (is.null(caminho) || is.na(caminho) || nchar(caminho) == 0) return(NULL); return(normalizePath(caminho))
}

# --- SELEÇÃO DE PASTAS ---
cat("1. Selecione a pasta de ENTRADA com os TextGrids gerados pelo MFA.\n")
pasta_textgrids <- tk_choose.dir(caption = "Selecione a Pasta com os TextGrids")
if (is.na(pasta_textgrids)) { stop("Seleção cancelada.") }

cat("2. Selecione a pasta com as TRANSCRIÇÕES (.txt ou .lab) originais.\n")
pasta_transcricoes <- tk_choose.dir(caption = "Selecione a Pasta com as Transcrições")
if (is.na(pasta_transcricoes)) { stop("Seleção cancelada.") }

cat("3. Selecione a pasta de SAÍDA para os novos TextGrids com o tier 'utterances'.\n")
pasta_saida <- tk_choose.dir(caption = "Selecione a Pasta de SAÍDA")
if (is.na(pasta_saida)) { stop("Seleção cancelada.") }
if (pasta_textgrids == pasta_saida) { stop("ERRO: A pasta de entrada e a de saída não podem ser a mesma.") }


# --- PROCESSAMENTO ---
arquivos_tg_nomes <- list.files(path = pasta_textgrids, pattern = "\\.TextGrid$")
if (length(arquivos_tg_nomes) == 0) { stop("ERRO: Nenhum arquivo .TextGrid encontrado.") }

cat(paste("\nEncontrados", length(arquivos_tg_nomes), "arquivos para processar.\n\n"))

for (nome_arquivo_base in arquivos_tg_nomes) {
  cat(paste("Processando:", nome_arquivo_base, "\n"))
  
  tryCatch({
    nome_base_sem_ext <- tools::file_path_sans_ext(nome_arquivo_base)
    
    caminho_transcricao <- NULL
    caminho_txt <- file.path(pasta_transcricoes, paste0(nome_base_sem_ext, ".txt"))
    caminho_lab <- file.path(pasta_transcricoes, paste0(nome_base_sem_ext, ".lab"))
    if(file.exists(caminho_txt)){ caminho_transcricao <- caminho_txt } else if(file.exists(caminho_lab)){ caminho_transcricao <- caminho_lab }
    
    if (is.null(caminho_transcricao)) {
      cat(paste("  AVISO: Transcrição não encontrada para", nome_arquivo_base, ". Pulando.\n"))
      next
    }
    
    caminho_tg <- file.path(pasta_textgrids, nome_arquivo_base)
    tier_palavras_df <- parse_textgrid_words(caminho_tg)
    if (is.null(tier_palavras_df) || nrow(tier_palavras_df) == 0) {
      cat(paste("  ERRO: Não foi possível ler o tier 'words' de", nome_arquivo_base, ". Pulando.\n"))
      next
    }
    
    intervalos_de_palavras_tg <- tier_palavras_df[tier_palavras_df$label != "", ]
    intervalos_de_silencio <- tier_palavras_df[tier_palavras_df$label == "", ]
    
    if (nrow(intervalos_de_palavras_tg) == 0) {
      cat(paste("  AVISO: Nenhuma palavra encontrada em", nome_arquivo_base, ". Pulando.\n"))
      next
    }
    
    palavras_do_tg_sanitizadas <- sapply(intervalos_de_palavras_tg$label, sanitizar_palavra)
    
    texto_completo_original <- paste(readLines(caminho_transcricao, warn = FALSE, encoding = "UTF-8"), collapse = " ")
    texto_temp <- str_replace_all(texto_completo_original, "(//|/|\\?)", " \\1 _DELIMITADOR_ ")
    unidades_brutas <- unlist(str_split(trimws(texto_temp), pattern = "_DELIMITADOR_"))
    
    # --- NOVA ETAPA DE PRÉ-FILTRAGEM ---
    unidades_reais <- c()
    for(unidade in unidades_brutas){
      palavras_teste <- unlist(str_extract_all(unidade, "[[:alnum:]'-’]+"))
      if(length(palavras_teste) > 0){
        unidades_reais <- c(unidades_reais, unidade)
      }
    }
    # ------------------------------------
    
    intervalos_de_frase_encontrados <- list()
    idx_tg_global <- 1
    
    for (unidade_bruta in unidades_reais) {
      label_original <- trimws(unidade_bruta)
      if (label_original == "") next
      
      palavras_da_unidade <- unlist(str_extract_all(label_original, "[[:alnum:]'-’]+"))
      palavras_da_unidade_sanitizadas <- sanitizar_palavra(palavras_da_unidade)
      palavras_da_unidade_sanitizadas <- palavras_da_unidade_sanitizadas[palavras_da_unidade_sanitizadas != ""]
      if (length(palavras_da_unidade_sanitizadas) == 0) next
      
      encontrado <- FALSE
      i_inicial_tg <- -1
      comprimento_encontrado <- 0
      
      for (offset in 0:min(1, length(palavras_da_unidade_sanitizadas) - 1)) {
        sequencia_busca <- palavras_da_unidade_sanitizadas[(1 + offset):length(palavras_da_unidade_sanitizadas)]
        if (length(sequencia_busca) == 0) break
        
        if (idx_tg_global <= length(palavras_do_tg_sanitizadas)) {
          for (i in idx_tg_global:length(palavras_do_tg_sanitizadas)) {
            fim_teste <- i + length(sequencia_busca) - 1
            if (fim_teste > length(palavras_do_tg_sanitizadas)) break
            
            sequencia_tg <- palavras_do_tg_sanitizadas[i:fim_teste]
            if (isTRUE(all(sequencia_tg == sequencia_busca))) {
              i_inicial_tg <- i
              comprimento_encontrado <- length(sequencia_busca)
              encontrado <- TRUE
              if(offset > 0) cat(sprintf("  AVISO: Omissão de '%s' detectada. Sincronizando com '%s'.\n", palavras_da_unidade[1], paste(palavras_da_unidade[-1], collapse=" ")))
              break
            }
          }
        }
        if (encontrado) break
      }
      
      if (encontrado) {
        i_final_tg <- i_inicial_tg + comprimento_encontrado - 1
        tempo_inicio <- intervalos_de_palavras_tg$start[i_inicial_tg]
        tempo_fim <- intervalos_de_palavras_tg$end[i_final_tg]
        intervalos_de_frase_encontrados[[length(intervalos_de_frase_encontrados) + 1]] <- data.frame(start = tempo_inicio, end = tempo_fim, label = label_original)
        idx_tg_global <- i_final_tg + 1
      } else {
        cat(sprintf("  AVISO: A sequência '%s' não foi encontrada. Será omitida.\n", label_original))
      }
    }
    
    df_frases <- if(length(intervalos_de_frase_encontrados) > 0) do.call(rbind, intervalos_de_frase_encontrados) else data.frame()
    
    # Lógica de Silêncio e Escrita...
    silencios_para_manter <- data.frame()
    if (nrow(intervalos_de_silencio) > 0) {
      if (!is.null(df_frases) && nrow(df_frases) > 0) {
        for (i in 1:nrow(intervalos_de_silencio)) {
          silencio_atual <- intervalos_de_silencio[i, ]; esta_dentro <- any(sapply(1:nrow(df_frases), function(j) (silencio_atual$start >= df_frases[j, 'start']) && (silencio_atual$end <= df_frases[j, 'end']))); if (!esta_dentro) silencios_para_manter <- rbind(silencios_para_manter, silencio_atual)
        }
      } else { silencios_para_manter <- intervalos_de_silencio }
    }
    df_final <- rbind(df_frases, silencios_para_manter)
    if (!is.null(df_final) && nrow(df_final) > 0) {
      df_final_ordenado <- df_final[order(df_final$start), ]
      linhas_tg_originais <- readLines(caminho_tg, warn = FALSE, encoding="UTF-8")
      size_line_index <- grep("size =", linhas_tg_originais)[1]
      original_size <- as.numeric(gsub("[^0-9]","", linhas_tg_originais[size_line_index]))
      linhas_tg_originais[size_line_index] <- paste0("    size = ", original_size + 1, " ")
      xmax_final <- tail(tier_palavras_df$end, 1)
      novo_tier_texto <- c(paste0("item [", original_size + 1, "]:"), '    class = "IntervalTier"', '    name = "utterances"', paste0("    xmin = ", tier_palavras_df$start[1]), paste0("    xmax = ", xmax_final), paste0("    intervals: size = ", nrow(df_final_ordenado)))
      for (j in 1:nrow(df_final_ordenado)) {
        intervalo <- df_final_ordenado[j, ]; label_escapado <- gsub('"', '""', intervalo$label, fixed = TRUE)
        novo_tier_texto <- c(novo_tier_texto, paste0("        intervals [", j, "]:"), paste0("            xmin = ", intervalo$start), paste0("            xmax = ", intervalo$end), paste0('            text = "', label_escapado, '"'))
      }
      linhas_finais <- c(linhas_tg_originais, novo_tier_texto)
      caminho_saida <- file.path(pasta_saida, paste0("utterances_", nome_arquivo_base))
      writeLines(linhas_finais, con = caminho_saida, sep = "\n")
    } else {
      cat(paste("  AVISO FINAL: Nenhum intervalo pôde ser criado para", nome_arquivo_base, ".\n"))
    }
  }, error = function(e) {
    cat(paste("  ERRO FATAL NO PROCESSAMENTO do arquivo", nome_arquivo_base, ":", e$message, "\n"))
  })
}

cat("\nProcesso concluído!\n")
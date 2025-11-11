# COMPASS: Corpus Mapper for Phonetic And Syllabic Structures
# Copyright (C) 2025  Gabriel Magalhães da Silveira
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# --- PRÉ-PROCESSAMENTO ---

# --- BIBLIOTECAS E MODELO ---
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library(shiny)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(tidyr)
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
library(purrr)
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)
if (!requireNamespace("tcltk", quietly = TRUE)) install.packages("tcltk")
library(tcltk)
if (!requireNamespace("udpipe", quietly = TRUE)) install.packages("udpipe")
library(udpipe)
if (!requireNamespace("tools", quietly = TRUE)) install.packages("tools")
library(tools)

modelo_arquivo <- "portuguese-gsd-ud-2.5-191206.udpipe"
if (!file.exists(modelo_arquivo)) {
  cat("Downloading the Portuguese language model...\n")
  udpipe_download_model(language = "portuguese-gsd")
}
cat("Loading the Portuguese language model...\n")
ud_model <- udpipe_load_model(file = modelo_arquivo)
cat("Model loaded.\n")

# --- DEFINIÇÕES GLOBAIS ---
NUCLEUS_IPA <- c("i","e","ɛ","a","u","o","ɔ","ɨ","ɐ","ĩ","ẽ","ã","õ","ũ","ɐ̃","ĩ","ẽ","ã","õ","ũ")
DIGRAFOS <- c("tʃ", "dʒ")
REGEX_FONES <- paste0("(", paste(DIGRAFOS, collapse = "|"), "|.)")

# --- 3. FUNÇÕES AUXILIARES ---
selecionar_pasta <- function(titulo = "Select a folder") {
  caminho <- tcltk::tclvalue(tcltk::tkchooseDirectory(title = titulo))
  if (nchar(caminho) == 0) return(NULL)
  return(normalizePath(caminho))
}

parse_textgrid_completo <- function(filepath) {
  tryCatch({
    lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
    extract_tier <- function(tier_name, text_lines) {
      header_index <- which(grepl(paste0('name = "', tier_name, '"'), text_lines, fixed = TRUE)); if (length(header_index) == 0) return(NULL)
      item_headers <- grep("item \\[[0-9]+\\]:", text_lines); tier_block_start <- item_headers[findInterval(header_index, item_headers)]; tier_block_end <- length(text_lines)
      next_item_header_index <- which(item_headers > tier_block_start); if (length(next_item_header_index) > 0) tier_block_end <- item_headers[next_item_header_index[1]] - 1
      tier_lines <- text_lines[tier_block_start:tier_block_end]; interval_indices <- grep("intervals \\[[0-9]+\\]:", tier_lines); if(length(interval_indices) == 0) return(data.frame())
      starts <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_indices + 1])); ends <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_indices + 2])); labels <- gsub('^\\s*text = "(.*)"\\s*$', "\\1", tier_lines[interval_indices + 3])
      return(data.frame(t1 = starts, t2 = ends, label = labels, stringsAsFactors = FALSE))
    }
    words_df <- extract_tier("words", lines)
    phones_df <- extract_tier("phones", lines)
    syllables_df <- extract_tier("syllables", lines)
    if (is.null(words_df) || is.null(phones_df) || is.null(syllables_df)) { return(NULL) }
    return(list(words = words_df, phones = phones_df, syllables = syllables_df))
  }, error = function(e) { message(e); return(NULL) })
}


# --- PRÉ-PROCESSAMENTO ---
cat("\n--- STARTING PRE-PROCESSING ---\n")
pasta_corpus <- selecionar_pasta(titulo = "SELECT THE FOLDER with your .TextGrid and .wav files")
if (is.null(pasta_corpus)) stop("Operation canceled.")
caminho_saida_rds <- tcltk::tclvalue(tcltk::tkgetSaveFile(title = "Save FINAL index file as...", initialfile = "indexed_corpus.rds", filetypes = "{{RDS files} {.rds}}"))
if (nchar(caminho_saida_rds) == 0) stop("Operation canceled.")

arquivos_tg <- list.files(pasta_corpus, pattern = "\\.TextGrid$", full.names = TRUE, ignore.case = TRUE)
lista_de_dataframes_fones <- list()
cat(sprintf("\nFound %d files .TextGrid. Starting indexing...\n\n", length(arquivos_tg)))

for (caminho_tg in arquivos_tg) {
  nome_base_com_prefixo <- basename(caminho_tg)
  cat(sprintf("Processing: %s\n", nome_base_com_prefixo))
  
  dados_tg <- parse_textgrid_completo(caminho_tg)
  if (is.null(dados_tg) || nrow(dados_tg$words) == 0) {
    cat("  -> WARNING: Skipped file (does not contain all required tiers).\n")
    next
  }
  
  tabela_palavras <- dados_tg$words; tabela_fones <- dados_tg$phones; tabela_silabas <- dados_tg$syllables
  palavras_reais <- tabela_palavras %>% filter(label != "" & label != "sp")
  if(nrow(palavras_reais) == 0) { next }
  
  palavras_reais$doc_id <- 1:nrow(palavras_reais)
  analise_nlp <- as.data.frame(udpipe_annotate(ud_model, x = palavras_reais$label, doc_id = palavras_reais$doc_id))
  analise_agregada <- analise_nlp %>% group_by(doc_id) %>% summarise(lemma_agregado = paste(lemma, collapse = "-"), pos_agregado = paste(upos, collapse = "-")) %>% ungroup() %>% mutate(doc_id = as.integer(doc_id))
  palavras_analisadas <- palavras_reais %>% left_join(analise_agregada, by = "doc_id") %>% mutate(lemma = lemma_agregado, classe_gramatical = pos_agregado) %>% select(-doc_id, -lemma_agregado, -pos_agregado)
  
  tabela_final_arquivo <- purrr::map_df(1:nrow(palavras_analisadas), function(i) {
    palavra_atual <- palavras_analisadas[i, ]
    
    fones_na_palavra_df <- tabela_fones %>% filter(t1 >= palavra_atual$t1 & t2 <= palavra_atual$t2)
    silabas_na_palavra_df <- tabela_silabas %>% filter(t1 >= palavra_atual$t1 & t2 <= palavra_atual$t2)
    
    if (nrow(fones_na_palavra_df) == 0 || nrow(silabas_na_palavra_df) == 0) return(NULL)
    
    silabas_str <- silabas_na_palavra_df$label[1]
    
    silabas_vetor <- stringr::str_split(silabas_str, "\\.")[[1]]
    n_silabas <- length(silabas_vetor)
    pos_tonica <- which(stringr::str_detect(silabas_vetor, "ˈ"))
    if (length(pos_tonica) == 0) pos_tonica <- if (n_silabas == 1) 1 else NA
    
    df_analise <- tibble()
    for (s_idx in 1:n_silabas) {
      silaba_atual_str <- silabas_vetor[s_idx]
      silaba_limpa <- str_remove_all(silaba_atual_str, "[ˈ.]")
      fones_na_silaba <- str_match_all(silaba_limpa, REGEX_FONES)[[1]][,1]
      n_fones <- length(fones_na_silaba)
      if(n_fones == 0) next
      
      tonicidade_atual <- if (!is.na(pos_tonica)) {
        case_when(n_silabas == 1 ~ "monossílabo", s_idx < pos_tonica ~ "pré-tônica",
                  s_idx == pos_tonica ~ "tônica", s_idx > pos_tonica ~ "pós-tônica", TRUE ~ "átona")
      } else { "átona" }
      
      posicoes_nucleo <- which(fones_na_silaba %in% NUCLEUS_IPA)
      pos_anc_vetor <- rep(NA_character_, n_fones)
      if (length(posicoes_nucleo) > 0) {
        inicio_nucleo <- min(posicoes_nucleo); fim_nucleo <- max(posicoes_nucleo)
        pos_anc_vetor <- case_when(1:n_fones < inicio_nucleo ~ "A", 1:n_fones > fim_nucleo ~ "C", TRUE ~ "N")
      }
      
      df_silaba <- tibble(
        fone = fones_na_silaba,
        silaba = silaba_limpa,
        silaba_id_na_palavra = s_idx,
        tonicidade = tonicidade_atual,
        posicao_anc = pos_anc_vetor
      )
      df_analise <- bind_rows(df_analise, df_silaba)
    }
    
    if(nrow(df_analise) != nrow(fones_na_palavra_df)){
    
      if(nrow(df_analise) > nrow(fones_na_palavra_df)){
        df_analise <- df_analise[1:nrow(fones_na_palavra_df), ]
      } else {
        diff <- nrow(fones_na_palavra_df) - nrow(df_analise)
        df_analise <- bind_rows(df_analise, slice_tail(df_analise, n = diff))
      }
    }
    
    resultado_final <- fones_na_palavra_df %>%
      select(tempo_inicio_fone = t1, tempo_fim_fone = t2) %>%
      bind_cols(df_analise) %>%
      mutate(
        palavra = palavra_atual$label,
        tempo_inicio_palavra = palavra_atual$t1,
        tempo_fim_palavra = palavra_atual$t2,
        lemma = palavra_atual$lemma,
        classe_gramatical = palavra_atual$classe_gramatical,
        silabificacao = silabas_str,
        n_silabas = n_silabas,
        id_tonica_na_palavra = pos_tonica
      )
    
    return(resultado_final)
  })
  
  id_arquivo_sem_prefixo <- stringr::str_remove(tools::file_path_sans_ext(nome_base_com_prefixo), "^syllabified_")
  
  df_arquivo_completo <- tabela_final_arquivo %>%
    mutate(
      id_arquivo = id_arquivo_sem_prefixo,
      caminho_audio = file.path(pasta_corpus, paste0(id_arquivo_sem_prefixo, ".wav"))
    )
  
  lista_de_dataframes_fones[[caminho_tg]] <- df_arquivo_completo
}

cat("\nCombining all data into a single phone table...\n")
base_de_dados_final <- bind_rows(lista_de_dataframes_fones)

cat(sprintf("Saving the final database in: %s\n", caminho_saida_rds))
saveRDS(base_de_dados_final, file = caminho_saida_rds)


cat("\n--- PRE-PROCESSING COMPLETE! ---\n")



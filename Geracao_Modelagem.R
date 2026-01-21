# ETAPA 1: Configuração Inicial e Pacotes --------------------------------------

# Limpar memória
rm(list = ls())
gc()

# Instalar e carregar pacotes
pkgs <- c(
  "dplyr",
  "utiml",
  "mldr",
  "randomForest",
  "e1071",
  "table1",
  "ggplot2",
  "reshape2",
  "gridExtra",
  "tidyr"
)

# Instala somente os que não estão instalados
missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}

# Carrega todos (e dá erro claro se algum falhar)
invisible(lapply(pkgs, function(p) {
  ok <- require(p, character.only = TRUE)
  if (!ok) stop(paste("Falha ao carregar o pacote:", p))
}))


# ETAPA 2: Leitura + Bases (RAW / Validação / Modelagem) ----------------------

setwd("C:/Users/brenn/Documents/TCC - BRUNO/Código/R")

# 2.1 Leitura (RAW: NÃO MEXER)
if (file.exists("dados_tratados2.csv")) {
  dados <- read.csv("dados_tratados2.csv", sep = ",", stringsAsFactors = FALSE)
  names(dados) <- trimws(names(dados))
} else {
  stop("Arquivo 'dados_tratados2.csv' não encontrado no diretório.")
}

# 2.2 Labels (doenças) - devem ser 0/1
labels <- c(
  "Hipertensao", "Diabetes", "Colesterol_Alto",
  "Artrite", "Depressao", "Osteoporose",
  "Problema_cronico_coluna"
)

if (!all(labels %in% names(dados))) {
  stop("Algumas colunas de doenças (labels) não estão no dataset.")
}

# 2.3 Base válida para análises/EDA (somente remove NA nas labels)
dados_valid <- dados[complete.cases(dados[, labels]), ]

# Garantir que labels são numéricas 0/1 (sem recodificar preditores!)
for (lbl in labels) {
  x <- suppressWarnings(as.integer(dados_valid[[lbl]]))
  # se aparecer algo diferente de 0/1 (não deveria), vira NA e cai no filtro abaixo
  x[!(x %in% c(0, 1))] <- NA
  dados_valid[[lbl]] <- x
}
dados_valid <- dados_valid[complete.cases(dados_valid[, labels]), ]

# Matriz de labels para ETAPA 4 (EDA) — alinhada com dados_valid
Ymat <- as.matrix(dados_valid[, labels])

# 2.4 Base de modelagem 
dados_modelagem <- dados_valid

X_cols <- setdiff(names(dados_modelagem), labels)

# Converter character -> factor (pra não quebrar RF/SVM/utiml)
for (col in X_cols) {
  if (is.character(dados_modelagem[[col]])) {
    dados_modelagem[[col]] <- factor(dados_modelagem[[col]])
  }
}

# Imputação SOMENTE nos preditores:
# - numéricos: média (arredondada)
# - fatores: moda
for (col in X_cols) {
  if (any(is.na(dados_modelagem[[col]]))) {
    if (is.numeric(dados_modelagem[[col]]) || is.integer(dados_modelagem[[col]])) {
      media <- round(mean(dados_modelagem[[col]], na.rm = TRUE))
      dados_modelagem[[col]][is.na(dados_modelagem[[col]])] <- media
    } else if (is.factor(dados_modelagem[[col]])) {
      moda <- names(sort(table(dados_modelagem[[col]]), decreasing = TRUE))[1]
      dados_modelagem[[col]][is.na(dados_modelagem[[col]])] <- moda
    }
  }
}

# (Fim da ETAPA 2)

# ETAPA 3: Geração da TABELA 1 (Demográfica) ----------------------------------

if (!exists("dados_valid")) stop("Erro: dados_valid não encontrado. Rode a ETAPA 2 primeiro.")

dados_t1 <- dados_valid  # cópia local só para tabela 1

# Número de doenças e grupos
dados_t1$Num_Doencas <- rowSums(dados_t1[, labels], na.rm = TRUE)
dados_t1$Grupo_Doencas <- cut(
  dados_t1$Num_Doencas,
  breaks = c(-Inf, 0, 1, 2, 3, Inf),
  labels = c("0 Condições", "1 Condição", "2 Condições", "3 Condições", "4+ Condições")
)

# Helper: setar label/units sem Hmisc
set_label <- function(x, txt) { attr(x, "label") <- txt; x }
set_units <- function(x, txt) { attr(x, "units") <- txt; x }

# Idade (se existir)
if ("Idade" %in% names(dados_t1)) {
  dados_t1$Idade <- set_label(dados_t1$Idade, "Idade (anos)")
  dados_t1$Idade <- set_units(dados_t1$Idade, "média (SD)")
}

# Sexo (0/1 já tratado no Tratamento_Dados)
if ("Sexo" %in% names(dados_t1)) {
  dados_t1$Sexo_Fator <- factor(dados_t1$Sexo, levels = c(0, 1), labels = c("Feminino", "Masculino"))
  dados_t1$Sexo_Fator <- set_label(dados_t1$Sexo_Fator, "Sexo")
}

# Escolaridade (0..3 já tratado)
if ("Escolaridade" %in% names(dados_t1)) {
  dados_t1$Escolaridade_Fator <- factor(
    dados_t1$Escolaridade,
    levels = c(0,1,2,3),
    labels = c("Fund. Incompleto/Sem inst.", "Fund. Completo", "Médio Completo", "Superior ou mais"),
    ordered = TRUE
  )
  dados_t1$Escolaridade_Fator <- set_label(dados_t1$Escolaridade_Fator, "Escolaridade")
}

# Escolaridade da mãe (0..3 já tratado)
if ("Escolaridade_Mae" %in% names(dados_t1)) {
  dados_t1$Escolaridade_Mae_Fator <- factor(
    dados_t1$Escolaridade_Mae,
    levels = c(0,1,2,3),
    labels = c("Fund. Incompleto/Sem inst.", "Fund. Completo", "Médio Completo", "Superior ou mais"),
    ordered = TRUE
  )
  dados_t1$Escolaridade_Mae_Fator <- set_label(dados_t1$Escolaridade_Mae_Fator, "Escolaridade da mãe")
}

# Estado civil (dummies já criadas no Tratamento_Dados)
if (all(c("EC_Solteiro","EC_Casado","EC_Divorciado","EC_Viuvo") %in% names(dados_t1))) {
  dados_t1$Estado_Civil_Fator <- dplyr::case_when(
    dados_t1$EC_Solteiro == 1 ~ "Solteiro(a)",
    dados_t1$EC_Casado == 1 ~ "Casado(a)",
    dados_t1$EC_Divorciado == 1 ~ "Divorciado(a)",
    dados_t1$EC_Viuvo == 1 ~ "Viúvo(a)",
    TRUE ~ NA_character_
  )
  dados_t1$Estado_Civil_Fator <- factor(
    dados_t1$Estado_Civil_Fator,
    levels = c("Solteiro(a)", "Casado(a)", "Divorciado(a)", "Viúvo(a)")
  )
  dados_t1$Estado_Civil_Fator <- set_label(dados_t1$Estado_Civil_Fator, "Estado Civil")
}

# Tem filhos (Qtd_Filhos já virou 0/1 no Tratamento_Dados)
if ("Qtd_Filhos" %in% names(dados_t1)) {
  dados_t1$Tem_Filhos <- factor(dados_t1$Qtd_Filhos, levels = c(0,1), labels = c("Não", "Sim"))
  dados_t1$Tem_Filhos <- set_label(dados_t1$Tem_Filhos, "Tem Filhos")
}

# IMC (se existir classificação binária)
if ("IMC_Classificacao" %in% names(dados_t1)) {
  dados_t1$IMC_Fator <- factor(dados_t1$IMC_Classificacao, levels = c(0,1), labels = c("Fora do Ideal", "Ideal"))
  dados_t1$IMC_Fator <- set_label(dados_t1$IMC_Fator, "Classificação IMC")
}

# Risco RCQ (0/1 já tratado)
if ("Risco_RCQ" %in% names(dados_t1)) {
  dados_t1$Risco_RCQ_Fator <- factor(dados_t1$Risco_RCQ, levels = c(0,1), labels = c("Sem Risco", "Com Risco"))
  dados_t1$Risco_RCQ_Fator <- set_label(dados_t1$Risco_RCQ_Fator, "Risco Cintura/Quadril")
}

# Fumo atual (0/1 já tratado)
if ("Fuma_Atualmente" %in% names(dados_t1)) {
  dados_t1$Fumo_Atual_Fator <- factor(dados_t1$Fuma_Atualmente, levels = c(0,1), labels = c("Não", "Sim"))
  dados_t1$Fumo_Atual_Fator <- set_label(dados_t1$Fumo_Atual_Fator, "Fumante Atual")
}

# Fumou no passado (0/1 já tratado)
if ("Fumou_Passado" %in% names(dados_t1)) {
  dados_t1$Fumou_Passado_Fator <- factor(dados_t1$Fumou_Passado, levels = c(0,1), labels = c("Não", "Sim"))
  dados_t1$Fumou_Passado_Fator <- set_label(dados_t1$Fumou_Passado_Fator, "Fumou no passado")
}

# Álcool (0/1 já tratado)
if ("Alcool_Freq" %in% names(dados_t1)) {
  dados_t1$Alcool_Fator <- factor(dados_t1$Alcool_Freq, levels = c(0,1), labels = c("Não", "Sim"))
  dados_t1$Alcool_Fator <- set_label(dados_t1$Alcool_Fator, "Consumo de Álcool")
}

# Atividade vigorosa (0/1 já tratado)
if ("Ativ_Fisica_Vigorosa" %in% names(dados_t1)) {
  dados_t1$Ativ_Vigorosa_Fator <- factor(dados_t1$Ativ_Fisica_Vigorosa, levels = c(0,1), labels = c("Não", "Sim"))
  dados_t1$Ativ_Vigorosa_Fator <- set_label(dados_t1$Ativ_Vigorosa_Fator, "Atividade Física Vigorosa")
}

# Caminhada (0,1,2 já tratado)
if ("Dias_Caminhada" %in% names(dados_t1)) {
  dados_t1$Caminhada_Fator <- factor(
    dados_t1$Dias_Caminhada,
    levels = c(0,1,2),
    labels = c("Não caminha", "Pouco (1–3d)", "Frequente (4–7d)"),
    ordered = TRUE
  )
  dados_t1$Caminhada_Fator <- set_label(dados_t1$Caminhada_Fator, "Frequência de Caminhada")
}

# Qualidade do sono (CORRIGIDO: 0..4 já invertido no Tratamento_Dados)
if ("Qualidade_Sono" %in% names(dados_t1)) {
  dados_t1$Qualidade_Sono_Fator <- factor(
    dados_t1$Qualidade_Sono,
    levels = c(0,1,2,3,4),
    labels = c("Muito Ruim", "Ruim", "Regular", "Boa", "Muito Boa"),
    ordered = TRUE
  )
  dados_t1$Qualidade_Sono_Fator <- set_label(dados_t1$Qualidade_Sono_Fator, "Qualidade do Sono")
}

# Frutas (0,1,2 já tratado)
if ("Frutas_Dias_Semana" %in% names(dados_t1)) {
  dados_t1$Frutas_Fator <- factor(
    dados_t1$Frutas_Dias_Semana,
    levels = c(0,1,2),
    labels = c("Não consome", "Baixo (1–3d)", "Frequente (4–7d)"),
    ordered = TRUE
  )
  dados_t1$Frutas_Fator <- set_label(dados_t1$Frutas_Fator, "Consumo de Frutas")
}

# Raça (dummies -> 1 coluna) se existirem
raca_cols <- c("Raca_Branca","Raca_Preta","Raca_Parda","Raca_Amarela","Raca_Indigena")
if (all(raca_cols %in% names(dados_t1))) {
  dados_t1$Raca_Fator <- dplyr::case_when(
    dados_t1$Raca_Branca == 1 ~ "Branca",
    dados_t1$Raca_Preta == 1 ~ "Preta",
    dados_t1$Raca_Parda == 1 ~ "Parda",
    dados_t1$Raca_Amarela == 1 ~ "Amarela",
    dados_t1$Raca_Indigena == 1 ~ "Indígena",
    TRUE ~ NA_character_
  )
  dados_t1$Raca_Fator <- factor(dados_t1$Raca_Fator,
                                levels = c("Branca","Preta","Parda","Amarela","Indígena"))
  dados_t1$Raca_Fator <- set_label(dados_t1$Raca_Fator, "Raça/Cor")
}

# Renda (se existir): recomendo AGRUPAR pra não virar um monstro na tabela
if ("Renda_Domiciliar" %in% names(dados_t1)) {
  dados_t1$Renda_Domiciliar_Fator <- cut(
    dados_t1$Renda_Domiciliar,
    breaks = c(0, 7, 12, 20),
    labels = c("Faixas 1–7", "Faixas 8–12", "Faixas 13–20"),
    right = TRUE
  )
  dados_t1$Renda_Domiciliar_Fator <- factor(
    dados_t1$Renda_Domiciliar_Fator,
    levels = c("Faixas 1–7", "Faixas 8–12", "Faixas 13–20"),
    ordered = TRUE
  )
  dados_t1$Renda_Domiciliar_Fator <- set_label(dados_t1$Renda_Domiciliar_Fator, "Renda domiciliar (faixas)")
}

# Variáveis da tabela (só entra o que existir)
vars_tabela <- c(
  "Idade", "Sexo_Fator", "Escolaridade_Fator", "Escolaridade_Mae_Fator",
  "Estado_Civil_Fator", "Raca_Fator",
  "Tem_Filhos", "IMC_Fator", "Risco_RCQ_Fator",
  "Fumo_Atual_Fator", "Fumou_Passado_Fator",
  "Alcool_Fator", "Ativ_Vigorosa_Fator",
  "Caminhada_Fator", "Qualidade_Sono_Fator", "Frutas_Fator",
  "Renda_Domiciliar_Fator"
)

vars_existentes <- vars_tabela[vars_tabela %in% names(dados_t1)]
formula_tabela <- as.formula(paste("~", paste(vars_existentes, collapse = " + "), "| Grupo_Doencas"))

tabela1 <- table1::table1(
  formula_tabela,
  data = dados_t1,
  overall = c(left = "Total"),
  caption = "Tabela 1. Características sociodemográficas e de saúde segundo número de condições crônicas."
)

print(tabela1)

# ETAPA 4: Análise Exploratória (Gráficos) -------------------------

if (!exists("Ymat")) stop("Erro: Ymat não encontrado. Rode as Etapas anteriores.")

# Ajuste de nomes
colnames(Ymat) <- gsub("_", " ", colnames(Ymat))
nomes_doencas <- colnames(Ymat)

# GRÁFICO 1: Prevalência
df_prev <- data.frame(
  Doenca = nomes_doencas,
  Prevalencia = colMeans(Ymat) * 100
)

g1 <- ggplot(df_prev, aes(x = reorder(Doenca, Prevalencia), y = Prevalencia)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Prevalencia)), hjust = -0.1, fontface = "bold") +
  scale_y_continuous(limits = c(0, max(df_prev$Prevalencia) + 10)) +
  coord_flip() +
  labs(title = "Gráfico 1: Prevalência das Doenças Crônicas", subtitle = "População ELSI-Brasil", x = "", y = "Prevalência (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), axis.text.y = element_text(size = 11, face = "bold"))

print(g1)
ggsave("Grafico1_Prevalencia.png", plot = g1, width = 8, height = 6)

# GRÁFICO 2: Distribuição
num_doencas <- rowSums(Ymat)
df_dist <- data.frame(Num = factor(num_doencas))

g2 <- ggplot(df_dist, aes(x = Num)) +
  geom_bar(fill = "#2E8B57", color = "black", alpha = 0.8) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, fontface = "bold") +
  labs(title = "Gráfico 2: Distribuição do Número de Doenças", subtitle = "Caracterização da Multimorbidade", x = "Número de Doenças", y = "Quantidade de Indivíduos") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(g2)
ggsave("Grafico2_Distribuicao.png", plot = g2, width = 8, height = 5)

# GRÁFICO 3: Boxplot
df_box <- data.frame(Contagem = num_doencas)

g3 <- ggplot(df_box, aes(y = Contagem, x = "")) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.shape = 1) +
  geom_jitter(width = 0.2, alpha = 0.05, color = "darkblue") +
  labs(title = "Gráfico 3: Dispersão do Número de Doenças", x = "", y = "Número de Doenças") +
  theme_classic()

print(g3)
ggsave("Grafico3_Boxplot.png", plot = g3, width = 4, height = 6)

# GRÁFICO 4: Labelsets
get_labels <- function(row) {
  active <- nomes_doencas[row == 1]
  if (length(active) == 0) return("Nenhuma")
  paste(active, collapse = " + ")
}

labelsets <- apply(Ymat, 1, get_labels)
df_sets <- as.data.frame(table(labelsets))
df_sets <- df_sets[df_sets$labelsets != "Nenhuma", ]
df_sets <- df_sets[order(-df_sets$Freq), ][1:10, ]

g4 <- ggplot(df_sets, aes(x = reorder(labelsets, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "#E69F00", width = 0.7) +
  geom_text(aes(label = Freq), hjust = -0.1, fontface = "bold") +
  coord_flip() +
  labs(title = "Gráfico 4: Top 10 Combinações de Doenças", x = "", y = "Número de Indivíduos") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(g4)
ggsave("Grafico4_Labelsets.png", plot = g4, width = 10, height = 6)

# GRÁFICO 5: Heatmap Correlação
cormat <- round(cor(Ymat), 2)
melted_cormat <- melt(cormat)

g5 <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name = "Correlação\nPearson") +
  geom_text(aes(label = value), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1), axis.title = element_blank(), plot.title = element_text(face = "bold")) +
  coord_fixed() +
  labs(title = "Gráfico 5: Matriz de Correlação entre Doenças")

print(g5)
ggsave("Grafico5_Correlacao.png", plot = g5, width = 8, height = 8)

# GRÁFICO 6: Heatmap Condicional
n_doencas <- ncol(Ymat)
cond_mat <- matrix(0, nrow = n_doencas, ncol = n_doencas)
rownames(cond_mat) <- nomes_doencas
colnames(cond_mat) <- nomes_doencas

for (i in 1:n_doencas) {
  for (j in 1:n_doencas) {
    total_X <- sum(Ymat[, i])
    total_XY <- sum(Ymat[, i] == 1 & Ymat[, j] == 1)
    cond_mat[j, i] <- ifelse(total_X > 0, total_XY / total_X, 0)
  }
}

melted_cond <- melt(cond_mat)
melted_cond$value <- round(melted_cond$value * 100, 1)

g6 <- ggplot(data = melted_cond, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black", name = "Prevalência\nCondicional (%)") +
  geom_text(aes(label = round(value, 0)), color = ifelse(melted_cond$value > 50, "white", "black"), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1), axis.title = element_text(face="bold"), plot.title = element_text(face = "bold")) +
  coord_fixed() +
  labs(title = "Gráfico 6: Heatmap Condicional (Assimetria)", x = "Condição Base (Dado que tem...)", y = "Comorbidade (Qual a chance de ter...)")

print(g6)
ggsave("Grafico6_Condicional.png", plot = g6, width = 9, height = 9)

# Gráfico 7
# GRÁFICO EXTRA: Pizza (0/1 doença vs 2+ doenças)
grupo_mm <- ifelse(num_doencas <= 1, "0–1 doença", "2+ doenças")
df_pie <- as.data.frame(table(grupo_mm))
names(df_pie) <- c("Grupo", "Freq")
df_pie$Perc <- df_pie$Freq / sum(df_pie$Freq) * 100
df_pie$label <- sprintf("%s\n%d (%.1f%%)", df_pie$Grupo, df_pie$Freq, df_pie$Perc)

g_pizza <- ggplot(df_pie, aes(x = "", y = Freq, fill = Grupo)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Proporção de Indivíduos com Multimorbidade",
       subtitle = "0–1 doença vs 2+ doenças",
       x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11))

print(g_pizza)
ggsave("Grafico_Pizza_0ou1_vs_2mais.png", plot = g_pizza, width = 7, height = 5)


cat("--- GERAÇÃO DE GRÁFICOS CONCLUÍDA ---\n")


# ETAPA 5: Modelagem e Validação Cruzada ---------------------------------------

# (1) Criar objeto mldr a partir do dados_modelagem (já imputado nos preditores)
mldr_obj <- mldr_from_dataframe(
  dados_modelagem,
  labelIndices = which(names(dados_modelagem) %in% labels),
  name = "ELSI"
)

# (2) Configurações
set.seed(42)
n_folds <- 10
n_repeats <- 5
metricas_escolhidas <- c("hamming-loss", "subset-accuracy", "accuracy", "F1", "macro-F1")

arquivo_saida <- "progresso_resultados_elsi.csv"

# (3) Sempre gerar partições novamente (rodar tudo do zero)
cat("Gerando partições (", n_repeats, "x", n_folds, ")...\n", sep = "")
parts_list <- lapply(1:n_repeats, function(i) create_kfold_partition(mldr_obj, k = n_folds))

# (4) Se já existir arquivo, faz backup e RECOMEÇA do zero (sobrescreve)
if (file.exists(arquivo_saida)) {
  backup <- paste0("backup_", format(Sys.time(), "%Y%m%d_%H%M%S_"), arquivo_saida)
  ok_backup <- file.copy(arquivo_saida, backup, overwrite = TRUE)
  if (ok_backup) {
    cat("Backup criado:", backup, "\n")
  } else {
    cat("Aviso: não consegui criar backup do CSV antigo.\n")
  }
}

cabecalho <- data.frame(
  Modelo = character(),
  Repeticao = integer(),
  Fold = integer(),
  accuracy = numeric(),
  subset_accuracy = numeric(),
  hamming_loss = numeric(),
  F1 = numeric(),
  macro_F1 = numeric()
)

# Sobrescreve e escreve header
write.table(cabecalho, arquivo_saida, sep = ";", row.names = FALSE, col.names = TRUE)

# (5) Ordem: RF primeiro (mais rápido), depois SVM
lista_modelos <- c(
  "RF-BR", "RF-DBR", "RF-CC", "MBR-RF",
  "SVM-BR", "SVM-DBR", "SVM-CC"
)

# Seed determinística por (rep, fold, modelo) -> garante que a ordem NÃO muda o resultado
seed_por_execucao <- function(rep, fold, modelo, modelos) {
  as.integer(100000 + rep * 1000 + fold * 10 + match(modelo, modelos))
}

rodar_modelo <- function(nome_modelo, train, test, seed_exec) {
  gc()
  # garante reprodutibilidade por execução
  set.seed(seed_exec)
  
  m <- switch(nome_modelo,
              "RF-BR"   = br(train, "RF",  ntree = 100, seed = seed_exec),
              "RF-DBR"  = dbr(train, "RF", ntree = 100, seed = seed_exec),
              "RF-CC"   = cc(train, "RF",  ntree = 100, seed = seed_exec),
              
              "SVM-BR"  = br(train, "SVM",  seed = seed_exec),
              "SVM-DBR" = dbr(train, "SVM", seed = seed_exec),
              "SVM-CC"  = cc(train, "SVM",  seed = seed_exec),
              
              "MBR-RF"  = mbr(train, "RF", ntree = 100, seed = seed_exec),
              
              stop("Modelo não reconhecido: ", nome_modelo)
  )
  
  pred <- predict(m, test)
  return(pred)
}

cat("--- Iniciando Loop de Treinamento (RODAR TUDO DO ZERO) ---\n")

for (r in 1:n_repeats) {
  for (k in 1:n_folds) {
    
    fold_data <- partition_fold(parts_list[[r]], k)
    tr <- fold_data$train
    ts <- fold_data$test
    
    for (mod in lista_modelos) {
      
      seed_exec <- seed_por_execucao(r, k, mod, lista_modelos)
      cat(sprintf("[%s] Rep %d/%d - Fold %d/%d ... ", mod, r, n_repeats, k, n_folds))
      
      tryCatch({
        pred <- rodar_modelo(mod, tr, ts, seed_exec)
        
        metrics <- multilabel_evaluate(ts, pred, measures = metricas_escolhidas)
        
        linha <- data.frame(
          Modelo = mod,
          Repeticao = r,
          Fold = k,
          accuracy = as.numeric(metrics["accuracy"]),
          subset_accuracy = as.numeric(metrics["subset-accuracy"]),
          hamming_loss = as.numeric(metrics["hamming-loss"]),
          F1 = as.numeric(metrics["F1"]),
          macro_F1 = as.numeric(metrics["macro-F1"])
        )
        
        write.table(linha, arquivo_saida, sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
        cat("SALVO\n")
        
        rm(pred, metrics, linha); gc()
      }, error = function(e) {
        cat("ERRO: ", e$message, "\n", sep = "")
        gc()
      })
      
    }
    
    rm(fold_data, tr, ts); gc()
  }
}

cat("--- ETAPA 5 CONCLUÍDA: resultados em ", arquivo_saida, " ---\n", sep = "")

# ETAPA EXTRA: FEATURE IMPORTANCE ----------------------------------------------
cat("\nCalculando Importância das Variáveis (Feature Importance)...\n")

var_imp_list <- list()
cols_preditores <- setdiff(names(dados_modelagem), labels)

# Garantir que variáveis character virem factor (RF não lida bem com character)
for (c in cols_preditores) {
  if (is.character(dados_modelagem[[c]])) {
    dados_modelagem[[c]] <- factor(dados_modelagem[[c]])
  }
}

for (lbl in labels) {
  
  # Dataset temporário: preditores + 1 label
  data_temp <- dados_modelagem[, c(cols_preditores, lbl), drop = FALSE]
  
  # Forçar CLASSIFICAÇÃO: label como fator 0/1
  y <- suppressWarnings(as.integer(data_temp[[lbl]]))
  y[!(y %in% c(0,1))] <- NA
  data_temp[[lbl]] <- factor(y, levels = c(0,1), labels = c("Nao", "Sim"))
  rm(y)
  
  # Garantir que não ficou NA na label
  data_temp <- data_temp[!is.na(data_temp[[lbl]]), , drop = FALSE]
  
  set.seed(123)
  rf_model <- randomForest(
    as.formula(paste(lbl, "~ .")),
    data = data_temp,
    ntree = 100,
    importance = TRUE,
    na.action = na.omit
  )
  
  imp <- importance(rf_model)
  
  # Em classificação, normalmente existe MeanDecreaseGini
  # Se não existir (casos raros), cai para IncNodePurity (regressão) ou última coluna disponível
  if ("MeanDecreaseGini" %in% colnames(imp)) {
    imp_val <- imp[, "MeanDecreaseGini"]
  } else if ("IncNodePurity" %in% colnames(imp)) {
    imp_val <- imp[, "IncNodePurity"]
  } else {
    imp_val <- imp[, ncol(imp)]
  }
  
  df_imp <- data.frame(
    Variavel = rownames(imp),
    Importancia = as.numeric(imp_val),
    Doenca = lbl,
    row.names = NULL
  )
  
  var_imp_list[[lbl]] <- df_imp
  
  rm(rf_model, imp, imp_val, df_imp, data_temp); gc()
}

df_importance_all <- do.call(rbind, var_imp_list)

df_importance_final <- df_importance_all %>%
  group_by(Variavel) %>%
  summarise(Media_Importancia = mean(Importancia, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Media_Importancia)) %>%
  head(15)

g_imp <- ggplot(df_importance_final, aes(x = reorder(Variavel, Media_Importancia), y = Media_Importancia)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +
  labs(title = "Importância das Variáveis (Média entre Doenças)",
       subtitle = "Random Forest por label (Mean Decrease Gini)",
       x = "", y = "Importância Média") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "bold"))

print(g_imp)
ggsave("Grafico_Feature_Importance.png", plot = g_imp, width = 9, height = 6)
write.csv(df_importance_all, "feature_importance_por_label.csv", row.names = FALSE)
write.csv(df_importance_final, "feature_importance_top15_media.csv", row.names = FALSE)

cat("Feature importance SALVA: Grafico_Feature_Importance.png + CSVs.\n")

# ETAPA 6: DIAGNÓSTICO E RECUPERAÇÃO -------------------------------------------

if (file.exists(arquivo_saida)) {
  dados_res <- read.csv(arquivo_saida, sep = ";")
  tabela_final <- dados_res %>%
    group_by(Modelo) %>%
    summarise(
      Accuracy = mean(accuracy),
      Subset_Accuracy = mean(subset_accuracy),
      Hamming_Loss = mean(hamming_loss),
      F1_Measure = mean(F1),
      Macro_F1 = mean(macro_F1),
      Folds_Concluidos = n()
    ) %>%
    arrange(desc(F1_Measure))
  print("--- RESULTADO FINAL ---")
  print(tabela_final)
  write.csv(tabela_final, "tabela_final_elsi.csv", row.names = FALSE)
}
# Todas as variáveis (média entre doenças) - SEM cortar em 15
df_importance_media_all <- df_importance_all %>%
  group_by(Variavel) %>%
  summarise(Media_Importancia = mean(Importancia, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Media_Importancia))

# Salvar tudo
write.csv(df_importance_media_all, "feature_importance_media_TODAS.csv", row.names = FALSE)

# Ver no console (top 50 para não poluir)
print(head(df_importance_media_all, 50))
g_all <- ggplot(df_importance_media_all, aes(x = reorder(Variavel, Media_Importancia), y = Media_Importancia)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +
  labs(title = "Importância das Variáveis (todas as preditoras)",
       subtitle = "Média do Mean Decrease Gini entre as doenças (RF por label)",
       x = "", y = "Importância Média") +
  theme_minimal()

print(g_all)
ggsave("Grafico_Feature_Importance_TODAS.png", plot = g_all, width = 12, height = 18)

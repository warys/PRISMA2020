# ============================================================
# PRISMA2020mod 流程图封装函数
# 基于修改版 PRISMA2020mod (修复了硬编码标签和detail数据显示问题)
# ============================================================
# 安装: install.packages("~/Desktop/PRISMA2020mod_1.1.4.9000.tar.gz", repos=NULL)
# 用法:
#   1. library(PRISMA2020mod)
#   2. source("~/Desktop/prisma_wrapper_mod.R")
#   3. data <- PRISMA_data(read.csv("你的数据.csv"))
#   4. prisma_full(data)           # 完全版
#   5. prisma_simplified(data)     # 精简版
# ============================================================

#' PRISMA 完全版流程图
#'
#' 包含: Previous studies + Other methods + 侧边阶段标签 + 数据库/注册库明细 + Meta分析
#'
#' @param data PRISMA_data() 输出的数据列表
#' @param output 输出文件前缀，默认 NULL 只返回plot对象
#' @param save 是否保存，默认 FALSE
#' @param fontsize 字体大小，默认 10
#' @return 返回 plot 对象
prisma_full <- function(data,
                        output = NULL,
                        save = FALSE,
                        fontsize = 10,
                        interactive = FALSE) {

  plot <- PRISMA_flowdiagram(
    data,
    fontsize         = fontsize,
    interactive      = interactive,
    previous         = TRUE,
    other            = TRUE,
    meta_analysis    = TRUE,
    side_boxes       = TRUE,
    detail_databases = TRUE,
    detail_registers = TRUE,
    title_colour     = "Goldenrod1",
    greybox_colour   = "Gainsboro",
    main_colour      = "Black"
  )

  if (save || !is.null(output)) {
    prefix <- ifelse(is.null(output), "~/Desktop/PRISMA_Full", output)
    PRISMA_save(plot, paste0(prefix, ".html"), overwrite = TRUE)
    PRISMA_save(plot, paste0(prefix, ".png"),   overwrite = TRUE)
    cat("已保存: ", prefix, ".html + .png\n", sep = "")
  }

  invisible(plot)
}

#' PRISMA 精简版流程图
#'
#' 仅含 Databases and registers 主流程 + 侧边阶段标签
#' 无 Previous studies / Other methods / Meta-analysis
#'
#' @param data PRISMA_data() 输出的数据列表
#' @param output 输出文件前缀，默认 NULL 只返回plot对象
#' @param save 是否保存，默认 FALSE
#' @param fontsize 字体大小，默认 10
#' @return 返回 plot 对象
prisma_simplified <- function(data,
                              output = NULL,
                              save = FALSE,
                              fontsize = 10,
                              interactive = FALSE) {

  plot <- PRISMA_flowdiagram(
    data,
    fontsize         = fontsize,
    interactive      = interactive,
    previous         = FALSE,
    other            = FALSE,
    meta_analysis    = FALSE,
    side_boxes       = TRUE,
    detail_databases = TRUE,
    detail_registers = TRUE,
    title_colour     = "Goldenrod1",
    greybox_colour   = "Gainsboro",
    main_colour      = "Black"
  )

  if (save || !is.null(output)) {
    prefix <- ifelse(is.null(output), "~/Desktop/PRISMA_Simplified", output)
    PRISMA_save(plot, paste0(prefix, ".html"), overwrite = TRUE)
    PRISMA_save(plot, paste0(prefix, ".png"),   overwrite = TRUE)
    cat("已保存: ", prefix, ".html + .png\n", sep = "")
  }

  invisible(plot)
}

#' 快速从CSV生成PRISMA流程图
#'
#' @param csv_path CSV文件路径
#' @param version "full" 或 "simplified"/"s"
#' @param output 输出文件前缀
#' @param fontsize 字体大小，默认 10
prisma_quick <- function(csv_path = NULL,
                         version = "simplified",
                         output = NULL,
                         fontsize = 10,
                         interactive = FALSE) {

  if (is.null(csv_path)) {
    csv_path <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020mod")
    cat("使用默认模板CSV:", csv_path, "\n")
  }

  raw <- read.csv(csv_path)
  data <- PRISMA_data(raw)

  if (is.null(output)) {
    ver <- ifelse(grepl("^s", tolower(version)), "Simplified", "Full")
    output <- paste0("~/Desktop/PRISMA_", ver)
  }

  if (grepl("^s", tolower(version))) {
    prisma_simplified(data, output = output, fontsize = fontsize, interactive = interactive)
  } else {
    prisma_full(data, output = output, fontsize = fontsize, interactive = interactive)
  }
}

cat("✅ PRISMA2020mod 封装函数已加载\n")
cat("   包: PRISMA2020mod (修改版，修复了硬编码标签问题)\n")
cat("   可用函数:\n")
cat("     prisma_full(data, save=TRUE)       — 完全版\n")
cat("     prisma_simplified(data, save=TRUE) — 精简版\n")
cat("     prisma_quick(version='simplified') — 一键生成\n")

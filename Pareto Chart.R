library(ggplot2)
library(scales)
library(dplyr)


# ── 1. CHECK SHEET: collect defect data ──────────────────────

build_check_sheet <- function() {

  cat("\n", strrep("=", 55), "\n")
  cat("        CHECK SHEET — Defect Data Entry\n")
  cat(strrep("=", 55), "\n")
  cat("Enter each defect type and its observed count.\n")
  cat("Type 'done' when finished.\n\n")

  defects <- character(0)
  counts  <- integer(0)

  repeat {

    defect <- trimws(readline("Defect name (or 'done' to finish): "))

    # ── exit condition ──────────────────────────────────────
    if (tolower(defect) == "done") {
      if (length(defects) < 2) {
        cat("  [!] Please enter at least 2 defects before finishing.\n\n")
        next
      }
      break
    }

    # ── empty input guard ───────────────────────────────────
    if (nchar(defect) == 0) {
      cat("  [!] Defect name cannot be empty.\n\n")
      next
    }

    # ── duplicate handling ──────────────────────────────────
    if (defect %in% defects) {
      existing <- counts[defects == defect]
      cat(sprintf("  [!] '%s' already recorded (%d occurrences).\n", defect, existing))
      update <- trimws(readline("      Add more occurrences? (y/n): "))
      if (tolower(update) != "y") next
    }

    # ── count input with validation ─────────────────────────
    repeat {
      raw <- trimws(readline(sprintf("  Count for '%s': ", defect)))
      n   <- suppressWarnings(as.integer(raw))

      if (is.na(n) || n <= 0) {
        cat("  [!] Please enter a positive whole number.\n")
        next
      }
      break
    }

    # ── store or accumulate ─────────────────────────────────
    if (defect %in% defects) {
      counts[defects == defect] <- counts[defects == defect] + n
    } else {
      defects <- c(defects, defect)
      counts  <- c(counts,  n)
    }

    cat(sprintf("  \u2713  Recorded: %s \u2014 %d occurrences\n\n",
                defect, counts[defects == defect]))
  }

  # Return a named integer vector  {defect_name: count}
  setNames(counts, defects)
}


# ── 2. DISPLAY CHECK SHEET TABLE ─────────────────────────────

display_check_sheet <- function(check_sheet) {

  total <- sum(check_sheet)

  # Sort descending by count
  cs <- sort(check_sheet, decreasing = TRUE)

  cat("\n", strrep("=", 55), "\n")
  cat("        CHECK SHEET SUMMARY\n")
  cat(strrep("=", 55), "\n")
  cat(sprintf("  %-30s %7s  %6s\n", "Defect", "Count", "%"))
  cat("  ", strrep("-", 48), "\n")

  for (i in seq_along(cs)) {
    pct <- cs[i] / total * 100
    cat(sprintf("  %-30s %7d  %5.1f%%\n", names(cs)[i], cs[i], pct))
  }

  cat("  ", strrep("-", 48), "\n")
  cat(sprintf("  %-30s %7d  100.0%%\n", "TOTAL", total))
  cat(strrep("=", 55), "\n\n")
}


# ── 3. PARETO CHART ───────────────────────────────────────────

plot_pareto <- function(check_sheet) {

  total <- sum(check_sheet)

  # Build tidy data frame, sorted descending
  df <- data.frame(
    defect = names(check_sheet),
    count  = as.integer(check_sheet),
    stringsAsFactors = FALSE
  ) |>
    arrange(desc(count)) |>
    mutate(
      defect      = factor(defect, levels = defect),   # preserve sort order
      cum_count   = cumsum(count),
      cum_pct     = cum_count / total * 100,
      bar_colour  = ifelse(cum_pct <= 80, "vital_few", "useful_many")
    )

  # Scale factor: maps count axis → percentage axis
  scale_factor <- total / 100

  # ── ggplot2 chart ─────────────────────────────────────────
  p <- ggplot(df, aes(x = defect)) +

    # Bars
    geom_col(aes(y = count, fill = bar_colour),
             width = 0.6, colour = "white", linewidth = 0.4) +

    # Count labels above bars
    geom_text(aes(y = count, label = count),
              vjust = -0.5, fontface = "bold", size = 3.2, colour = "#333333") +

    # Cumulative % line  (y values multiplied by scale_factor to sit on left axis)
    geom_line(aes(y = cum_pct * scale_factor, group = 1),
              colour = "#D64045", linewidth = 0.9) +

    geom_point(aes(y = cum_pct * scale_factor),
               colour = "#D64045", size = 2.8) +

    # Cumulative % labels on each point
    geom_text(aes(y = cum_pct * scale_factor,
                  label = paste0(round(cum_pct, 1), "%")),
              vjust = -0.9, hjust = -0.1,
              colour = "#D64045", size = 2.8) +

    # 80% reference line (convert 80% to left-axis scale)
    geom_hline(yintercept = 80 * scale_factor,
               linetype = "dashed", colour = "#D64045",
               linewidth = 0.7, alpha = 0.6) +

    annotate("text",
             x     = nrow(df) - 0.1,
             y     = 80 * scale_factor * 1.03,
             label = "80%",
             colour = "#D64045", size = 3, hjust = 1) +

    # Dual y-axes: left = count, right = percentage
    scale_y_continuous(
      name   = "Defect Count",
      limits = c(0, max(df$count) * 1.20),
      sec.axis = sec_axis(
        transform = ~ . / scale_factor,
        name      = "Cumulative Percentage (%)",
        labels    = label_percent(scale = 1)
      )
    ) +

    # Bar colours
    scale_fill_manual(
      values = c(vital_few = "#2C6FAC", useful_many = "#A8C8E8"),
      labels = c(vital_few = "Vital few (\u226480%)", useful_many = "Useful many (>80%)"),
      name   = NULL
    ) +

    # Labels & theme
    labs(
      title   = "Pareto Chart \u2014 Defect Analysis",
      x       = "Defect Type",
      caption = "Bars: defect count  |  Line: cumulative %  |  Dashed line: 80% threshold"
    ) +

    theme_minimal(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = "#F8F9FA", colour = NA),
      panel.background   = element_rect(fill = "#F8F9FA", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#DDDDDD", linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 14, colour = "#1A1A2E",
                                        margin = margin(b = 10)),
      axis.text.x        = element_text(angle = ifelse(
                                          max(nchar(levels(df$defect))) > 8, 30, 0),
                                        hjust = 1, size = 9),
      axis.title.y.right = element_text(colour = "#D64045"),
      axis.text.y.right  = element_text(colour = "#D64045"),
      legend.position    = "top",
      legend.justification = "right",
      plot.caption       = element_text(colour = "#888888", size = 8,
                                        margin = margin(t = 8))
    )

  # Save and display
  output_path <- "pareto_chart.png"
  ggsave(output_path, plot = p,
         width  = max(8, nrow(df) * 1.2),
         height = 6,
         dpi    = 150,
         bg     = "#F8F9FA")
  cat(sprintf("  Chart saved \u2192 %s\n", output_path))

  print(p)
  invisible(p)
}


# ── 4. MAIN ───────────────────────────────────────────────────

main <- function() {

  cat("\nPareto / Check Sheet Tool\n")
  cat("Identify the 'vital few' defects driving the majority of problems.\n\n")

  use_demo <- trimws(readline("Load demo data? (y/n): "))

  if (tolower(use_demo) == "y") {

    check_sheet <- c(
      "Wrong dimensions"  = 45L,
      "Surface scratch"   = 32L,
      "Missing component" = 18L,
      "Colour mismatch"   = 12L,
      "Broken seal"       =  9L,
      "Label error"       =  6L,
      "Packaging damage"  =  3L
    )
    cat("\n  Demo data loaded.\n")

  } else {
    check_sheet <- build_check_sheet()
  }

  display_check_sheet(check_sheet)
  plot_pareto(check_sheet)
}

# Run
main()


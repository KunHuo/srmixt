#' Barplot for weights
#'
#' @param object an object from [sr_gwqs] function.
#' @param label logical, whether to show weight values.
#' @param label.size label szie.
#' @param label.digits label digits.
#' @param bar.color bar color.
#' @param bar.width bar width.
#' @param ... unused.
#'
#' @return a ggplot2 object.
#' @export
sr_gwqs_barplot <- function(object,
                            label = FALSE,
                            label.size = 12,
                            label.digits = 3,
                            bar.color = NULL,
                            bar.width = 0.65,
                            ...){

  exec_plot <- function(d, direction){

    d$labels <- sprintf("%s", srmisc::fmt_digits(d[["mean_weight"]], label.digits))
    d <- d[order(d[["mean_weight"]]), ]
    d[["mix_name"]] <- factor(d[["mix_name"]],
                              levels = unique(d[["mix_name"]]),
                              labels = srmisc::get_var_label(object$data, unique(d[["mix_name"]]), default = ".name"))
    xbreaks <- pretty(c(0, max(d[["mean_weight"]])))

    plot <- ggplot2::ggplot(d) +
      ggplot2::geom_bar(ggplot2::aes_string( x = "mean_weight", y = "mix_name", fill = "mix_name"),
                        stat = "identity", width = bar.width, show.legend = FALSE) +
      ggplot2::geom_vline(xintercept = 1 / length(object$exposure),
                          linetype = 2,
                          linewidth = 0.25,
                          color = "#374E55FF") +
      srmisc::gg_theme_sci() +
      srmisc::gg_delete_y_title() +
      srmisc::gg_xbreaks_continuous(breaks = xbreaks) +
      srmisc::gg_xlab(sprintf("Mean weight for %s", direction)) +
      ggplot2::coord_cartesian(clip = "off")

    if(label){
      plot <- plot +
        ggplot2::geom_text(ggplot2::aes_string(x = "mean_weight", y = "mix_name", label = "labels"),
                           hjust = -0.2,
                           family = "serif",
                           size = label.size / 2.847 )
    }

    if(!is.null(bar.color)){
      plot <- plot +
        ggplot2::scale_fill_discrete(bar.color)
    }

    plot
  }

  plots <- Map(\(x, direction){
    if(!is.null(x)){
      exec_plot(x$final_weights, direction)
    }
  }, object[1:2], names(object[1:2]))

  plots <- plots[sapply(plots, \(x) { !is.null(x) })]

  if(length(plots) == 1L){
    plots <- plots[[1]]
  }else{
    plots <- Map(\(p, tag){
      p + srmisc::gg_tags(tag)
    }, plots, LETTERS[1:2])
    plots <- patchwork::wrap_plots(plots)
  }

  plots
}

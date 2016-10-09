theme_dlin <- function() {
  library(RColorBrewer)
  library(extrafont)

  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  panel.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[8]
  color.title = palette[9]
  font = 'Cabin'

  theme_bw(base_size=12) +

    # Set background color
    theme(panel.background=element_rect(fill=panel.background, color=panel.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25, linetype = 3)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks.y=element_blank()) +
    theme(axis.ticks.x=element_line(color = palette[5], size = 0.2)) +
    theme(strip.background=element_rect(fill=color.background, color=color.background)) +

    # Set font
    theme(text=element_text(family=font)) +

    # Format the legend
    theme(legend.position="right") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.key = element_rect(color=color.background)) +
    theme(legend.text = element_text(size=9,color=color.axis.text)) +

    # Set title and axis labels
    theme(plot.title=element_text(color=color.title, size=18, hjust=0)) +
    theme(axis.text.x=element_text(size=12,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=12,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=14,color=color.axis.title)) +
    theme(axis.title.y=element_text(size=14,color=color.axis.title)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

## --- log loss curve ----

LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1 - eps)
  -1 / length(actual) * (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted)))
}

## --- log loss ----

plot_log_loss <- function(probs, actuals) {
  require(ggplot2)

  eps = 1:35 / 100
  loss = c()

  d$prob =
  for (e in eps) {
    loss = c(loss, LogLoss(actuals, probs, e))
  }

  df = data.frame(eps, loss)

  ggplot(df, aes(eps, loss)) + geom_line() + geom_point() +
    theme_dlin() + labs(title = 'Log Loss Curve', y = 'Log Loss', x = 'eps')
}

## --- roc curve ---

plot_roc_curve <- function(actual, models, names) {
  require(ggplot2)
  require(dplyr)

  df = data.frame()

  for (i in 1:length(models)) {
    name = names[[i]]

    probs = models[[i]]

    tpr = c()
    fpr = c()

    min_prob = max(round(min(probs) * 20), 1)
    max_prob = min(max(probs) * 20, 19)

    for (i in min_prob:max_prob / 20) {
      crtab = table(probs > i, actual)
      tpr = c(tpr, crtab[2,2] / sum(crtab[,2]))
      fpr = c(fpr, crtab[2,1] / sum(crtab[,1]))
    }

    df = rbind_all(list(df, data.frame(tpr, fpr, name), data.frame(tpr = .99,fpr = .99,name)))
  }

  ggplot(df, aes(fpr, tpr, color = name)) + geom_line() + geom_point() + xlim(c(0,1)) + ylim(c(0,1)) +
    theme_dlin() + labs(title = 'Model ROC Curve', y = 'True Positive Rate', x = 'False Positive Rate',
                        legend = NULL)
}


## --- model calibration ----

plot_model_calibration <- function(probs, actuals) {
  prob_bin = cut(probs, breaks = 0:20 / 20, include.lowest = TRUE)

  df = data.frame(prob_bin, actuals)

  group_by(df, prob_bin) %>%
    summarise(n = mean(actuals), t = n()) %>%
    ggplot(aes(prob_bin, weight = n)) +
    geom_text(aes(label = sprintf('%d\n(%.1f)',t,n*100), y = n, family = 'Cabin'), vjust = -.5) +
    geom_bar() + theme_dlin() + theme(axis.text.x=element_text(angle=45, hjust = 1)) + ylim(c(0,1)) +
    labs(title = 'Model Calibration', x = 'Binned Model Probability', y = 'Actual Outcome Rate')

}

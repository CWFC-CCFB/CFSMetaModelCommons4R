#############################################################
# This file is part of the CFSStandGrowth4R library located at :
# https://github.com/CWFC-CCFB/CFSStandGrowth4R
#
# Copyright (C) 2022-2024 His Majesty the King in Right of Canada
# Authors: Mathieu Fortin, Canadian Forest Service.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed with the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
#
# Please see the license at http://www.gnu.org/copyleft/lesser.html.
#############################################################


#'
#' Create a Plot of Observed and Predicted Values
#'
#' The predicted values are optional.
#'
#' @param dataObject a data.frame object with the observations
#' @param predictions a data.frame object with the predictions
#' @param title a character string
#' @param textsize the font size (20 by default)
#'
#' @export
createGOFplot <- function(dataObject, predictions = NULL, title = NULL, textsize = 20) {
  dataset <- .formatObservationDataSet(dataObject)

  predEnabled <- !is.null(predictions)

  if (predEnabled) {
    predictions$predL95 <- predictions$Pred - predictions$Variance^.5 * stats::qnorm(0.975)
    predictions$predU95 <- predictions$Pred + predictions$Variance^.5 * stats::qnorm(0.975)
    predictions[which(predictions$predL95 < 0), "predL95"] <- 0
  }

  plot <- ggplot2::ggplot()
  if ("lower95" %in% colnames(dataset) & "upper95" %in% colnames(dataset)) {
    plot <- plot +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=lower95, ymax=upper95, x=age, group=stratum), dataset, alpha = .1)
    maxY <- max(dataset$upper95)
  } else {
    maxY <- 0
  }
  maxY <- max(maxY, max(dataset$Estimate))
  if (predEnabled) {
    maxY <- max(maxY, max(predictions$predU95))
    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin=predL95, ymax=predU95, x=AgeYr), predictions, alpha = .5)
  }


  outputType <- unique(dataset$OutputType)
  if (length(outputType) != 1) {
    stop("There seems to be more than one output type in the data!")
  }
  if (grepl("BasalArea", outputType)) {
    yLabel <- bquote('Basal area'~(m^2~ha^{-1}))
  } else if (grepl("Volume", outputType)) {
    yLabel <- bquote('Volume'~(m^3~ha^{-1}))
  } else if (grepl("Biomass", outputType)) {
    yLabel <- bquote('Biomass'~(Mg~ha^{-1}))
  } else if (grepl("DominantHeight", outputType)) {
    yLabel <- bquote('Dominant height'~(m))
  } else if (grepl("StemDensity", outputType)) {
    yLabel <- bquote('Density'~(Trees~ha^{-1}))
  }

  plot <- plot +
    ggplot2::geom_line(ggplot2::aes(y=Estimate, x=age, group=stratum), dataset, lty = "dashed")

  if (predEnabled) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(y=Pred, x=AgeYr), predictions, lty = "solid", linewidth = 1.5)
  }

  plot <- plot +
    ggplot2::xlab("Age (yr)") +
    ggplot2::ylab(yLabel) +
    ggplot2::ylim(0, maxY + 1) +
    ggplot2::xlim(0, max(dataset$age) + 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size=textsize),
                   axis.text.x = ggplot2::element_text(size=textsize, color = "black"),
                   axis.text.y = ggplot2::element_text(size=textsize, color = "black"),
                   axis.line = ggplot2::element_line(color = "black"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.ticks.length = ggplot2::unit(3,"mm"),
                   panel.border = ggplot2::element_blank())
  if (is.null(title)) {
    plot <- plot + ggplot2::ggtitle(outputType)
  } else {
    plot <- plot + ggplot2::ggtitle(title)
  }
  return(plot)
}

.formatObservationDataSet <- function(dataset) {
  isVarianceAvailable <- "TotalVariance" %in% colnames(dataset)

  if (isVarianceAvailable)
  {
    dataset$lower95 <- dataset$Estimate - dataset$TotalVariance^.5 * stats::qnorm(0.975)
    dataset[which(dataset$lower95 < 0), "lower95"] <- 0
    dataset$upper95 <- dataset$Estimate + dataset$TotalVariance^.5 * stats::qnorm(0.975)
  }

  dataset$age <- dataset$StratumAgeYr + dataset$timeSinceInitialDateYr
  dataset$stratum <- paste(dataset$OutputType, dataset$StratumAgeYr, sep="_")
  return(dataset)
}

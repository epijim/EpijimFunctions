#' Violin plots
#'
#' Make violin plots - based off this https://github.com/mbjoseph/comdis/blob/master/vioplot2.R
#' @param Var1_left Variable one, left side.
#' @param Var1_right Variable one, right side.
#' @param Var2_left Variable two, left side.
#' @param Var2_right Variable two, right side.
#' @keywords packages
#' @export
#' @examples
#' data <- iris
#' jb_vioplot(
#'  Var1_left,
#'  Var1_right,
#'  Var2_left,
#'  Var2_right,
#'  title="This is a title",
#'  xlabel="Something",
#'  group1title="On the left",
#'  group2title="On the right")

jb_vioplot <- function (Var1_left,Var1_right,Var2_left,Var2_right,
                        title="Title",
                        xlabel="xlabel",
                        group1title="Group 1",
                        group2title="Group 2") 
{
  # original function
  jb_vioplotplotter <- function (x,
                                 ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                                 horizontal = FALSE, col = "magenta", border = "black", lty = 1, 
                                 lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
                                 at, add = FALSE, wex = 1, drawRect = TRUE, side="both") 
  {
    datas <- list(x, ...)
    n <- length(datas)
    if (missing(at)) 
      at <- 1:n
    upper <- vector(mode = "numeric", length = n)
    lower <- vector(mode = "numeric", length = n)
    q1 <- vector(mode = "numeric", length = n)
    q2 <- vector(mode = "numeric", length = n)
    q3 <- vector(mode = "numeric", length = n)
    med <- vector(mode = "numeric", length = n)
    base <- vector(mode = "list", length = n)
    height <- vector(mode = "list", length = n)
    baserange <- c(Inf, -Inf)
    args <- list(display = "none")
    radj <- ifelse(side == "right", 0, 1)
    ladj <- ifelse(side == "left", 0, 1)
    if (!(is.null(h))) 
      args <- c(args, h = h)
    med.dens <- rep(NA, n)
    for (i in 1:n) {
      data <- datas[[i]]
      data.min <- min(data)
      data.max <- max(data)
      q1[i] <- quantile(data, 0.25)
      q2[i] <- quantile(data, 0.5)
      q3[i] <- quantile(data, 0.75)
      med[i] <- median(data)
      iqd <- q3[i] - q1[i]
      upper[i] <- min(q3[i] + range * iqd, data.max)
      lower[i] <- max(q1[i] - range * iqd, data.min)
      est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                                 data.max))
      smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                       args))
      med.dat <- do.call("sm.density", 
                         c(list(data, xlim=est.xlim,
                                eval.points=med[i], display = "none")))
      med.dens[i] <- med.dat$estimate
      hscale <- 0.4/max(smout$estimate) * wex
      base[[i]] <- smout$eval.points
      height[[i]] <- smout$estimate * hscale
      med.dens[i] <- med.dens[i] * hscale
      t <- range(base[[i]])
      baserange[1] <- min(baserange[1], t[1])
      baserange[2] <- max(baserange[2], t[2])
    }
    if (!add) {
      xlim <- if (n == 1) 
        at + c(-0.5, 0.5)
      else range(at) + min(diff(at))/2 * c(-1, 1)
      if (is.null(ylim)) {
        ylim <- baserange
      }
    }
    if (is.null(names)) {
      label <- 1:n
    }
    else {
      label <- names
    }
    boxwidth <- 0.05 * wex
    if (!add) 
      plot.new()
    if (!horizontal) {
      if (!add) {
        plot.window(xlim = xlim, ylim = ylim)
        axis(2)
        axis(1, at = at, label = label)
      }
      box()
      for (i in 1:n) {
        polygon(x = c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])), 
                y = c(base[[i]], rev(base[[i]])), 
                col = col, border = border, 
                lty = lty, lwd = lwd)
        if (drawRect) {
          lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
                lty = lty)
          rect(at[i] - radj*boxwidth/2, 
               q1[i], 
               at[i] + ladj*boxwidth/2, 
               q3[i], col = rectCol)
          # median line segment
          lines(x = c(at[i] - radj*med.dens[i], 
                      at[i], 
                      at[i] + ladj*med.dens[i]),
                y = rep(med[i],3))
        }
      }
    }
    else {
      if (!add) {
        plot.window(xlim = ylim, ylim = xlim)
        axis(1)
        axis(2, at = at, label = label)
      }
      box()
      for (i in 1:n) {
        polygon(c(base[[i]], rev(base[[i]])), 
                c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])), 
                col = col, border = border, 
                lty = lty, lwd = lwd)
        if (drawRect) {
          lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
                lty = lty)
          rect(q1[i], at[i] - radj*boxwidth/2, q3[i], at[i] + 
                 ladj*boxwidth/2, col = rectCol)
          lines(y = c(at[i] - radj*med.dens[i], 
                      at[i], 
                      at[i] + ladj*med.dens[i]),
                x = rep(med[i],3))
        }
      }
    }
    invisible(list(upper = upper, lower = lower, median = med, 
                   q1 = q1, q3 = q3))
  }
  # Prepare data
  values <- c(A1, A2, B1, B2)
  treatment <- c(
    rep(x = "A",length(A1)),
    rep(x = "A",length(A2)),
    rep(x = "B",length(B1)),
    rep(x = "B",length(B2))
  )
  group <- c(
    rep(c(1), length(A1)),
    rep(c(2), length(A2)),
    rep(c(1), length(B1)),
    rep(c(2), length(B2))
  )
  
  plot(x=NULL, y=NULL,
       xlim = c(0.5, 2.5), ylim=c(min(values), max(values)),
       type="n", ann=FALSE, axes=F)
  axis(1, at=c(1, 2),  labels=c("A", "B"))
  axis(2)
  for (i in unique(treatment)) {
    for (j in unique(group)){
      jb_vioplotplotter(values[which(treatment == i & group == j)],
               at = ifelse(i == "A", 1, 2),
               side = ifelse(j == 1, "left", "right"),
               col = ifelse(j == 1, "purple", "lightblue"),
               add = T)
    }
  }
  title(title, xlab=xlabel)
  legend("bottomright", fill = c("purple", "lightblue"),
         legend = c(group1title, group2title), box.lty=0)
}
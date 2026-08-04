# ---------------------------------------------------------------------------
# Dose-response explorer
# ---------------------------------------------------------------------------
# Interactive companion to the spline figures in the paper. Pick an outcome and
# an estimation group, drag the dose slider, and watch the three causal
# parameters move together:
#
#   ATT(d)   the total effect at dose d, the HEIGHT of the curve
#   ACRT(d)  the marginal effect at dose d, the SLOPE of the curve at d
#   ACR      the dose-weighted average of ACRT, one number, so a flat line
#
# The MODEL switch is the point of the app. "Linear" is what the paper actually
# estimates: ATT(d) is a straight line through the origin and ACRT(d) collapses
# onto the ACR, because a single slope is assumed. "Spline" is the visual layer
# only. Flipping between them shows what the linear restriction hides.
#
# Data: data/results/spline_curves.csv, written by RUN_SPLINE_DOSE_RESPONSE in
# scripts/05_figures.R. Run from the package root with:
#   Rscript -e 'shiny::runApp("shiny/spline_explorer")'

suppressMessages({library(shiny); library(data.table); library(ggplot2)})

find_curves <- function() {
  for (p in c("data/results/spline_curves.csv",
              "../../data/results/spline_curves.csv"))
    if (file.exists(p)) return(normalizePath(p))
  stop("spline_curves.csv not found. Run scripts/05_figures.R first.")
}
CURVES <- fread(find_curves())
BASIS  <- local({
  p <- sub("spline_curves", "spline_basis", find_curves())
  if (file.exists(p)) fread(p) else NULL
})
suppressMessages(library(splines))

ui <- fluidPage(
  withMathJax(),
  titlePanel("Dose-response explorer: ATT, ACRT and the ACR"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("outcome", "Outcome", choices = sort(unique(CURVES$outcome)),
                  selected = "Work in own muni"),
      selectInput("group", "Estimation group",
                  choices = c("Inc-bind", "Inc-becomes", "Pooled Increase"),
                  selected = "Inc-bind"),
      radioButtons("model", "Model",
                   choices = c("Linear (estimated)" = "linear",
                               "Spline (visual)"    = "spline"),
                   selected = "spline"),
      uiOutput("dose_ui"),
      radioButtons("view", "View",
                   choices = c("Dose-response" = "curve",
                               "Spline basis"  = "basis"),
                   selected = "curve"),
      checkboxInput("show_quad", "Overlay quadratic", FALSE),
      checkboxInput("show_ci", "Confidence bands", TRUE),
      checkboxInput("show_tangent", "Tangent at d", TRUE),
      hr(),
      htmlOutput("readout")
    ),
    mainPanel(
      width = 9,
      plotOutput("plot", height = "520px"),
      hr(),
      uiOutput("equations")
    )
  )
)

server <- function(input, output, session) {

  sel <- reactive({
    d <- CURVES[outcome == input$outcome & group == input$group]
    if (!nrow(d)) return(NULL)
    list(att = d[curve == "ATT"], acrt = d[curve == "ACRT"],
         attq = d[curve == "ATT_quad"], acrtq = d[curve == "ACRT_quad"],
         acr = d[curve == "ACRT", acr][1], dmax = max(d$dose, na.rm = TRUE))
  })

  # Curves under the selected model. Linear is the estimated object: a single
  # slope, so ATT is acr*d and ACRT is acr everywhere.
  cur <- reactive({
    s <- sel(); req(s)
    if (input$model == "spline") {
      list(att = s$att[, .(dose, y, lo, hi)],
           acrt = s$acrt[, .(dose, y, lo, hi)], acr = s$acr)
    } else {
      g <- s$att$dose
      list(att  = data.table(dose = g, y = s$acr * g, lo = NA_real_, hi = NA_real_),
           acrt = data.table(dose = g, y = rep(s$acr, length(g)),
                             lo = NA_real_, hi = NA_real_),
           acr  = s$acr)
    }
  })

  # The three groups have different dose ranges (0.50 / 0.85 / 1.00), so the
  # slider is rebuilt when the view changes rather than kept at a stale value.
  output$dose_ui <- renderUI({
    s <- sel(); req(s)
    sliderInput("dose", "Dose d (percentage points)", min = 0,
                max = round(s$dmax, 3), value = round(s$dmax / 2, 3),
                step = round(s$dmax / 200, 4))
  })

  at_d <- reactive({
    cv <- cur(); s <- sel(); req(cv, s)
    d <- if (is.null(input$dose) || !is.finite(input$dose)) s$dmax / 2 else input$dose
    d <- max(0, min(s$dmax, d))
    list(d = d,
         att  = approx(cv$att$dose,  cv$att$y,  xout = d, rule = 2)$y,
         acrt = approx(cv$acrt$dose, cv$acrt$y, xout = d, rule = 2)$y,
         acr  = cv$acr)
  })

  output$readout <- renderUI({
    v <- at_d(); req(v)
    extra <- if (input$model == "linear")
      "Under the linear model ACRT is constant and equal to the ACR by construction."
    else
      sprintf("ACRT is %s than the ACR here, so a further percentage point at this dose bites %s than the sample average.",
              if (v$acrt < v$acr) "steeper" else "flatter",
              if (v$acrt < v$acr) "harder" else "less")
    HTML(sprintf(
      "<div style='font-size:15px;line-height:1.7'>At <b>d = %.3f pp</b><br>
       <span style='color:#1b7837'><b>ATT(d) = %+.3f</b></span><br>
       <span style='color:#b2182b'><b>ACRT(d) = %+.3f</b></span><br>
       <span style='color:#2166ac'><b>ACR = %+.3f</b></span><br><br>
       <span style='font-size:13px;color:#555'>%s</span></div>",
      v$d, v$att, v$acrt, v$acr, extra))
  })

  # MathJax needs a SINGLE backslash in the delivered string, so each command is
  # written "\\alpha" in the R source. Doubling it again renders as literal text.
  output$equations <- renderUI({
    v <- at_d(); req(v)
    eq_lin_mod <- paste0(
      "$$\\textbf{Linear:}\\quad Y_{it} = \\alpha_i + \\gamma_t + ",
      "\\beta\\,d_{it} + \\mathbf{X}_{it}\'\\phi + \\varepsilon_{it}$$")
    eq_sp_mod <- paste0(
      "$$\\textbf{Spline:}\\quad Y_{it} = \\alpha_i + \\gamma_t + ",
      "B(d_{it})\'b + \\mathbf{X}_{it}\'\\phi + \\varepsilon_{it}$$")
    eq_par <- paste0(
      "$$\\text{ATT}(d) = \\bigl(B(d) - B(0)\\bigr)\'b, \\qquad ",
      "\\text{ACRT}(d) = \\frac{\\partial\\,\\text{ATT}(d)}{\\partial d}, \\qquad ",
      "\\text{ACR} = \\int \\text{ACRT}(d)\\,\\omega(d)\\,\\mathrm{d}d$$")
    eq_now <- if (input$model == "linear") paste0(
      "$$B(d) = d \\;\\Rightarrow\\; \\text{ATT}(d) = \\hat\\beta d, \\quad ",
      "\\text{ACRT}(d) = \\text{ACR} = \\hat\\beta = ",
      sprintf("%.3f", v$acr), "$$") else paste0(
      "$$\\text{ACRT}(d)\\ \\text{varies with } d, \\quad ",
      "\\text{ACR} = ", sprintf("%.3f", v$acr),
      "\\ \\text{is its dose-weighted average}$$")
    withMathJax(
      div(style = "font-size:14px",
        HTML("Both fits run on the same sample, this group\'s municipalities plus
              never-changers, with the dose \\(d_{it}=0\\) for never-changers.
              They differ in one term only, how the dose enters:"),
        HTML(eq_lin_mod),
        HTML(eq_sp_mod),
        HTML("The linear model is the spline with \\(B(d)=d\\), a single column.
              The causal parameters have one definition either way. ATT is the
              height of the fitted curve relative to zero dose, ACRT is its
              slope, and the ACR averages that slope over \\(\\omega(d)\\), the
              distribution of doses municipalities actually chose:"),
        HTML(eq_par),
        HTML(eq_now),
        HTML("The spline is deliberately more flexible than the data support. It
              is a visual device, not an estimating equation, and the headline
              numbers come from the linear fit."))) 
  })

  # What B(d)'b actually expands to: three piecewise-cubic columns, each
  # scaled by its own coefficient, summing to ATT(d). Shows why the spline is
  # not a polynomial in d.
  basis_plot <- function() {
    s <- sel(); req(s, BASIS)
    bb <- BASIS[group == input$group & outcome == input$outcome]
    if (!nrow(bb)) return(NULL)
    dg <- seq(bb$bk_lo[1], bb$bk_hi[1], length.out = 250)
    B  <- ns(dg, knots = c(bb$knot1[1], bb$knot2[1]),
             Boundary.knots = c(bb$bk_lo[1], bb$bk_hi[1]))
    cf <- c(bb$b1[1], bb$b2[1], bb$b3[1])
    wt <- sweep(unclass(B), 2, cf, "*")
    dd <- rbindlist(lapply(1:3, function(k) data.table(
      dose = dg, y = wt[, k],
      part = sprintf("b%d x B%d(d)   [b%d = %.3f]", k, k, k, cf[k]))))
    tot <- data.table(dose = dg, y = as.numeric(unclass(B) %*% cf))
    tot[, y := y - y[1]]
    ggplot() +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.9) +
      geom_vline(xintercept = c(bb$knot1[1], bb$knot2[1]),
                 linetype = "dotted", colour = "grey45") +
      geom_line(data = dd, aes(dose, y, colour = part), linewidth = 0.9) +
      geom_line(data = tot, aes(dose, y), colour = "#1b7837", linewidth = 1.4) +
      scale_colour_brewer(palette = "Set2", name = NULL) +
      labs(x = "dose d (cumulative muni rate change, pp)", y = NULL,
           title = sprintf("Spline basis decomposition  |  %s  |  %s",
                           input$outcome, input$group),
           subtitle = paste(
             "Thin lines: each basis column scaled by its coefficient. Thick green: their sum, which is ATT(d).",
             "Dotted verticals: the interior knots.",
             "\nThe green curve here is the green curve on the Dose-response tab.",
             "The red ACRT curve there is its slope, not another sum:",
             "basis lines -> (sum) -> ATT(d) -> (derivative) -> ACRT(d) -> (dose-weighted average) -> ACR.")) +
      theme_bw(base_size = 13) +
      theme(legend.position = "bottom",
            plot.subtitle = element_text(size = 10, colour = "grey30"))
  }

  output$plot <- renderPlot({
    if (identical(input$view, "basis")) { p <- basis_plot(); req(p); return(p) }
    s <- sel(); cv <- cur(); v <- at_d(); req(s, cv, v)

    ra <- range(c(cv$att$lo, cv$att$hi, cv$att$y), na.rm = TRUE)
    rc <- range(c(cv$acrt$lo, cv$acrt$hi, cv$acrt$y, cv$acr), na.rm = TRUE)
    if (diff(ra) < 1e-9) ra <- ra + c(-1, 1) * 0.1
    if (diff(rc) < 1e-9) rc <- rc + c(-1, 1) * 0.5
    sc  <- diff(ra) / diff(rc)
    f   <- function(y) (y - rc[1]) * sc + ra[1]
    inv <- function(y) (y - ra[1]) / sc + rc[1]

    acrt <- copy(cv$acrt)[, `:=`(y2 = f(y), lo2 = f(lo), hi2 = f(hi))]
    tang <- data.table(x = c(max(0, v$d - s$dmax * 0.18),
                             min(s$dmax, v$d + s$dmax * 0.18)))
    tang[, y := v$att + v$acrt * (x - v$d)]

    # Zero is the comparison baseline (a never-changer), so it gets a solid
    # black rule rather than the faint dotted grey it had.
    g <- ggplot() +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.9)
    if (isTRUE(input$show_ci) && input$model == "spline") g <- g +
      geom_ribbon(data = cv$att, aes(dose, ymin = lo, ymax = hi),
                  fill = "#1b7837", alpha = 0.12) +
      geom_ribbon(data = acrt, aes(dose, ymin = lo2, ymax = hi2),
                  fill = "#b2182b", alpha = 0.10)
    if (isTRUE(input$show_tangent)) g <- g +
      geom_line(data = tang, aes(x, y), colour = "#b2182b",
                linewidth = 1.1, alpha = 0.75)
    if (isTRUE(input$show_quad) && nrow(s$attq)) {
      g <- g +
        geom_line(data = s$attq, aes(dose, y), colour = "#7b3294",
                  linewidth = 1.0, linetype = "22") +
        geom_line(data = copy(s$acrtq)[, y2 := f(y)], aes(dose, y2),
                  colour = "#7b3294", linewidth = 0.8, linetype = "22", alpha = 0.7)
    }
    g +
      geom_line(data = cv$att, aes(dose, y),  colour = "#1b7837", linewidth = 1.0) +
      geom_line(data = acrt,   aes(dose, y2), colour = "#b2182b", linewidth = 0.9) +
      geom_hline(yintercept = f(cv$acr), colour = "#2166ac",
                 linetype = "longdash", linewidth = 0.8) +
      geom_vline(xintercept = v$d, colour = "grey35", linetype = "dashed") +
      geom_point(aes(v$d, v$att),     colour = "#1b7837", size = 3.6) +
      geom_point(aes(v$d, f(v$acrt)), colour = "#b2182b", size = 3.6) +
      annotate("text", x = s$dmax * 0.02, y = f(cv$acr), vjust = -0.6, hjust = 0,
               label = sprintf("ACR = %.3f", cv$acr), colour = "#2166ac", size = 4) +
      scale_x_continuous(limits = c(0, s$dmax)) +
      scale_y_continuous(name = "ATT(d): total effect",
                         sec.axis = sec_axis(~ inv(.), name = "ACRT(d): marginal effect")) +
      labs(x = "dose d (cumulative muni rate change, pp)",
           title = sprintf("%s  |  %s  |  %s model", input$outcome, input$group,
                           if (input$model == "linear") "linear" else "spline"),
           subtitle = if (input$model == "linear")
             "Green: ATT(d) = ACR x d. Red: ACRT(d), constant and equal to the ACR. This is what the paper estimates."
           else
             paste("Green: ATT(d), total effect (left). Red: ACRT(d), its slope (right). Blue dashed: the ACR.",
                   if (isTRUE(input$show_quad)) "Purple dashed: the quadratic fit on the same sample." else "")) +
      theme_bw(base_size = 13) +
      theme(axis.title.y.left  = element_text(colour = "#1b7837"),
            axis.title.y.right = element_text(colour = "#b2182b"),
            plot.subtitle = element_text(size = 10, colour = "grey30"))
  })
}

shinyApp(ui, server)

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
POINTS <- local({
  p <- sub("spline_curves", "spline_points", find_curves())
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
      checkboxGroupInput("models", "Models shown",
                   choices = c("Linear (estimated)" = "linear",
                               "Quadratic"          = "quad",
                               "Spline (visual)"    = "spline"),
                   selected = c("linear", "quad", "spline")),
      uiOutput("dose_ui"),
      radioButtons("view", "View",
                   choices = c("Dose-response" = "curve",
                               "Spline basis"  = "basis"),
                   selected = "curve"),
      checkboxInput("show_pts", "Municipality points", TRUE),
      checkboxInput("show_acrt", "ACRT curves (right axis)", TRUE),
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

  # All three models on one grid, in long form, so the plot can draw whichever
  # are ticked. Linear is the estimated object: a single slope, so ATT is acr*d
  # and ACRT is acr everywhere. Quadratic and spline come from the CSV.
  MODLAB <- c(linear = "Linear (estimated)", quad = "Quadratic", spline = "Spline (visual)")
  MODCOL <- c(`Linear (estimated)` = "#2166ac", Quadratic = "#7b3294",
              `Spline (visual)` = "#1b7837")

  cur <- reactive({
    s <- sel(); req(s)
    g <- s$att$dose
    lin_att  <- data.table(dose = g, y = s$acr * g, lo = NA_real_, hi = NA_real_)
    lin_acrt <- data.table(dose = g, y = rep(s$acr, length(g)), lo = NA_real_, hi = NA_real_)
    bind <- function(dt, m, cv) if (is.null(dt) || !nrow(dt)) NULL else
      copy(dt)[, .(dose, y, lo, hi, model = MODLAB[[m]], curve = cv)]
    att <- rbindlist(list(bind(lin_att, "linear", "ATT"),
                          bind(s$attq,  "quad",   "ATT"),
                          bind(s$att,   "spline", "ATT")), use.names = TRUE)
    acrt <- rbindlist(list(bind(lin_acrt, "linear", "ACRT"),
                           bind(s$acrtq,  "quad",   "ACRT"),
                           bind(s$acrt,   "spline", "ACRT")), use.names = TRUE)
    keep <- MODLAB[intersect(c("linear", "quad", "spline"), input$models)]
    list(att = att[model %in% keep], acrt = acrt[model %in% keep],
         acr = s$acr, keep = keep)
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
    pick <- function(dt, m) { x <- dt[model == m]
      if (!nrow(x)) NA_real_ else approx(x$dose, x$y, xout = d, rule = 2)$y }
    tab <- rbindlist(lapply(cv$keep, function(m)
      data.table(model = m, att = pick(cv$att, m), acrt = pick(cv$acrt, m))))
    list(d = d, tab = tab, acr = cv$acr,
         att  = if (nrow(tab)) tab$att[nrow(tab)]  else NA_real_,
         acrt = if (nrow(tab)) tab$acrt[nrow(tab)] else NA_real_)
  })

  output$readout <- renderUI({
    v <- at_d(); req(v)
    rows <- paste(apply(v$tab, 1, function(r) sprintf(
      "<tr><td style='color:%s'><b>%s</b></td><td align='right'>%+.3f</td><td align='right'>%+.3f</td></tr>",
      MODCOL[[r[["model"]]]], r[["model"]], as.numeric(r[["att"]]), as.numeric(r[["acrt"]]))),
      collapse = "")
    HTML(sprintf(
      "<div style='font-size:14px;line-height:1.6'>At <b>d = %.3f pp</b>
       <table style='width:100%%;margin-top:6px'>
       <tr><th align='left'></th><th align='right'>ATT(d)</th><th align='right'>ACRT(d)</th></tr>%s
       </table>
       <div style='margin-top:8px;color:#2166ac'><b>ACR = %+.3f</b></div>
       <div style='margin-top:8px;font-size:12px;color:#555'>The linear row is
       flat by construction, so its ACRT equals the ACR at every dose. Where the
       spline or quadratic sits below it, a further point of tax bites harder
       than the sample average at this dose.</div></div>",
      v$d, rows, v$acr))
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
    eq_q_mod <- paste0(
      "$$\\textbf{Quadratic:}\\quad Y_{it} = \\alpha_i + \\gamma_t + ",
      "\\beta_1 d_{it} + \\beta_2 d_{it}^2 + \\mathbf{X}_{it}\'\\phi + \\varepsilon_{it}$$")
    eq_par <- paste0(
      "$$\\text{ATT}(d) = \\bigl(B(d) - B(0)\\bigr)\'b, \\qquad ",
      "\\text{ACRT}(d) = \\frac{\\partial\\,\\text{ATT}(d)}{\\partial d}, \\qquad ",
      "\\text{ACR} = \\int \\text{ACRT}(d)\\,\\omega(d)\\,\\mathrm{d}d$$")
    eq_now <- paste0(
      "$$\\textbf{Linear: } B(d) = d \\;\\Rightarrow\\; \\text{ATT}(d) = \\hat\\beta d, \\quad ",
      "\\text{ACRT}(d) = \\text{ACR} = \\hat\\beta = ", sprintf("%.3f", v$acr), "$$",
      "$$\\textbf{Quadratic and spline: } \\text{ACRT}(d)\\ \\text{varies with } d, \\quad ",
      "\\text{ACR} = ", sprintf("%.3f", v$acr),
      "\\ \\text{remains its dose-weighted average}$$")
    withMathJax(
      div(style = "font-size:14px",
        HTML("Both fits run on the same sample, this group\'s municipalities plus
              never-changers, with the dose \\(d_{it}=0\\) for never-changers.
              They differ in one term only, how the dose enters:"),
        HTML(eq_lin_mod),
        HTML(eq_q_mod),
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
    validate(need(nrow(cv$att) > 0, "Tick at least one model."))

    show_acrt <- isTRUE(input$show_acrt)
    ra <- range(c(cv$att$lo, cv$att$hi, cv$att$y), na.rm = TRUE)
    rc <- range(c(cv$acrt$lo, cv$acrt$hi, cv$acrt$y, cv$acr), na.rm = TRUE)
    if (diff(ra) < 1e-9) ra <- ra + c(-1, 1) * 0.1
    if (diff(rc) < 1e-9) rc <- rc + c(-1, 1) * 0.5
    # With ACRT hidden the right axis carries nothing, so the transform becomes
    # the identity and the plot is scaled to the ATT curves alone.
    sc  <- if (show_acrt) diff(ra) / diff(rc) else 1
    f   <- if (show_acrt) function(y) (y - rc[1]) * sc + ra[1] else function(y) y
    inv <- if (show_acrt) function(y) (y - ra[1]) / sc + rc[1] else function(y) y

    acrt <- copy(cv$acrt)[, `:=`(y2 = f(y), lo2 = f(lo), hi2 = f(hi))]
    tang <- data.table(x = c(max(0, v$d - s$dmax * 0.18),
                             min(s$dmax, v$d + s$dmax * 0.18)))
    tang[, y := v$att + v$acrt * (x - v$d)]
    pts <- v$tab[!is.na(att)]

    # Colour is the MODEL, so the three fits can be compared directly. Solid is
    # ATT on the left axis, dashed is ACRT on the right. Zero is the comparison
    # baseline, a never-changer, so it gets a solid black rule.
    g <- ggplot() +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.9)
    if (isTRUE(input$show_pts) && !is.null(POINTS)) {
      pp <- POINTS[group == input$group & outcome == input$outcome & dose <= s$dmax]
      if (nrow(pp)) g <- g + geom_point(data = pp, aes(dose, y), colour = "grey72",
                                        size = 2.1, alpha = 0.85)
    }
    if (isTRUE(input$show_ci)) {
      sp <- cv$att[model == "Spline (visual)" & !is.na(lo)]
      sa <- acrt[model == "Spline (visual)" & !is.na(lo2)]
      if (nrow(sp)) g <- g + geom_ribbon(data = sp, aes(dose, ymin = lo, ymax = hi),
                                          fill = "#1b7837", alpha = 0.10)
      if (show_acrt && nrow(sa)) g <- g + geom_ribbon(data = sa, aes(dose, ymin = lo2, ymax = hi2),
                                          fill = "#1b7837", alpha = 0.08)
    }
    if (isTRUE(input$show_tangent) && show_acrt) g <- g +
      geom_line(data = tang, aes(x, y), colour = "grey35",
                linewidth = 1.0, alpha = 0.7)

    g <- g +
      geom_line(data = cv$att, aes(dose, y, colour = model), linewidth = 1.05) +
      geom_vline(xintercept = v$d, colour = "grey35", linetype = "dashed") +
      geom_point(data = pts, aes(v$d, att, colour = model), size = 3.4)
    if (show_acrt) g <- g +
      geom_line(data = acrt, aes(dose, y2, colour = model),
                linewidth = 0.85, linetype = "42") +
      geom_point(data = pts, aes(v$d, f(acrt), colour = model), size = 3.4,
                 shape = 17)
    g +
      scale_colour_manual(values = MODCOL, name = NULL, drop = FALSE) +
      scale_x_continuous(limits = c(0, s$dmax)) +
      (if (show_acrt)
         scale_y_continuous(name = "ATT(d): total effect",
                            sec.axis = sec_axis(~ inv(.), name = "ACRT(d): marginal effect"))
       else scale_y_continuous(name = "ATT(d): total effect")) +
      labs(x = "dose d (cumulative muni rate change, pp)",
           title = sprintf("%s  |  %s", input$outcome, input$group),
           subtitle = if (show_acrt) paste(
             "Solid = ATT(d), read on the left axis. Dashed = ACRT(d), read on the right.",
             "Circles mark ATT at the chosen dose, triangles mark ACRT.",
             "\nThe blue linear fit is what the paper estimates: its ATT is a straight ray and its ACRT is flat at the ACR.",
             "Grey points: one treated municipality each, its own pre/post change less the never-changers', at the dose it took.")
             else paste(
             "ATT(d), the total effect of a dose-d increase relative to a never-changer.",
             "Circles mark the chosen dose.",
             "\nThe blue linear fit is what the paper estimates, a straight ray of slope ACR.",
             "Green band is the spline confidence interval.")) +
      theme_bw(base_size = 13) +
      theme(legend.position = "bottom",
            axis.title.y.left  = element_text(colour = "grey20"),
            axis.title.y.right = element_text(colour = "grey20"),
            plot.subtitle = element_text(size = 10, colour = "grey30"))
  })
}

shinyApp(ui, server)

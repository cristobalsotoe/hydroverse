tm_scale_intervals <- function (n = 5, style = ifelse(is.null(breaks), "pretty", "fixed"), 
          style.args = list(), breaks = NULL, interval.closure = "left", 
          label.style = "discrete", label.select = TRUE, midpoint = NULL, 
          as.count = FALSE, values = NA, values.repeat = FALSE, values.range = NA, 
          values.scale = NA, value.na = NA, value.null = NA, value.neutral = NA, 
          labels = NULL, label.na = NA, label.null = NA, label.format = tm_label_format()) 
{
  structure(c(list(FUN = "tmapScaleIntervals"), as.list(environment())), 
            class = c("tm_scale_intervals", "tm_scale", "list"))
}




tmapScaleIntervals <- function (x1, scale, legend, chart, o, aes, layer, layer_args, 
          sortRev, bypass_ord, submit_legend = TRUE) 
{
  cls = data_class(x1, midpoint_enabled = !is.null(scale$midpoint))
  maincls = class(scale)[1]
  if (attr(cls, "unique") && is.null(scale$breaks)) {
    scale$ticks = NA
    cli::cli_inform("The visual variable {.arg {aes}} of the layer {.str {layer}} contains a unique value. Therefore a discrete scale is applied (tm_scale_discrete).")
    return(tmapScaleDiscrete(x1, scale, legend, chart, o, 
                             aes, layer, layer_args, sortRev, bypass_ord, submit_legend = submit_legend))
  }
  if (!(cls[1] %in% c("num", "datetime", "date"))) {
    if (!is.factor(x1)) 
      x1 = as.factor(x1)
    x1 = as.integer(x1)
    cls = c("num", "int", "seq")
    warning(maincls, " is supposed to be applied to numerical or date/time data", 
            call. = FALSE)
  }
  x1 = without_units(x1)
  if (aes %in% c("pattern")) 
    stop("tm_scale_intervals cannot be used for layer ", 
         layer, ", aesthetic ", aes, call. = FALSE)

  
  # 2. Asegurar que la lógica de leyenda continua funcione para 'col'
  # (líneas 200-230 del código) ya maneja esto parcialmente
  scale = get_scale_defaults(scale, o, aes, layer, cls)
  show.messages <- o$show.messages
  show.warnings <- o$show.warnings
  fun = paste0("interval_", cls[1])
  midpoint = NULL
  scale = within(scale, {
    allna = all(is.na(x1))
    if (anyDuplicated(breaks)) 
      stop("breaks specified in the ", aes, ".scale scaling function contains duplicates.", 
           call. = FALSE)
    if (!is.null(breaks) && length(breaks) < 2 && cls[1] == 
        "num") 
      stop("breaks should contain at least 2 numbers", 
           call. = FALSE)
    udiv = identical(use_div(breaks, midpoint), TRUE)
    if (allna && is.null(breaks)) {
      if (show.messages) 
        message("Variable(s) \"", paste(aes, collapse = "\", \""), 
                "\" only contains NAs. Legend disabled for tm_scale_intervals, unless breaks are specified")
      chart = within(chart, {
        labels = label.na
        vvalues = c(value.na, value.na)
        breaks = c(0, 1)
        na.show = TRUE
        x1 = x1[1]
      })
      return = tmapScale_returnNA(n = length(x1), legend = legend, 
                                  chart = chart, value.na = value.na, label.na = label.na, 
                                  label.show = label.show, na.show = legend$na.show, 
                                  sortRev = sortRev, bypass_ord = bypass_ord)
    }
  })
  if (!is.null(scale$return)) 
    return(scale$return)
  scale = do.call(fun, list(scale = scale, x1 = x1, aes = aes, 
                            layer = layer, show.messages = show.messages, show.warnings = show.warnings))
  with(scale, {
    fun_getVV = paste0("tmapValuesVV_", aes)
    VV = do.call(fun_getVV, list(x = values, value.na = value.na, 
                                 isdiv = isdiv, n = n, dvalues = breaks, midpoint = midpoint, 
                                 range = values.range, scale = values.scale * o$scale, 
                                 are_breaks = TRUE, rep = values.repeat, o = o))
    vvalues = VV$vvalues
    value.na = VV$value.na
    sfun = paste0("tmapValuesScale_", aes)
    cfun = paste0("tmapValuesColorize_", aes)
    if (is.na(value.neutral)) 
      value.neutral = VV$value.neutral
    else value.neutral = do.call(sfun, list(x = do.call(cfun, 
                                                        list(x = value.neutral, pc = o$pc)), scale = values.scale))
    if (aes %in% c("size", "lwd")) {
      if (vvalues[1] == 0) {
        message_scale_interval_value0(aes, values, layer)
      }
    }
    mfun = paste0("tmapValuesSubmit_", aes)
    vvalues = do.call(mfun, list(x = vvalues, args = layer_args))
    value.na = do.call(mfun, list(x = value.na, args = layer_args))
    value.neutral = do.call(mfun, list(x = value.neutral, 
                                       args = layer_args))
    ids = classInt::findCols(q)
    vals = vvalues[ids]
    isna = is.na(vals)
    anyNA = any(isna)
    na.show = update_na.show(label.show, legend$na.show, 
                             anyNA)
    if (is.null(sortRev)) {
      ids = NULL
    }
    else if (is.na(sortRev)) {
      ids[] = 1L
    }
    else if (sortRev) {
      ids = (as.integer(n) + 1L) - ids
    }
    if (anyNA) {
      vals[isna] = value.na
      if (!is.null(sortRev)) 
        ids[isna] = 0L
    }
    if (is.log) {
      if (any((breaks%%1) != 0)) 
        message("non-rounded breaks occur, because style = \"log10_pretty\" is designed for large values")
      breaks <- 10^breaks
    }
    if (label.style == "discrete") {
      if (is.null(labels)) {
        labels = do.call("fancy_breaks", c(list(vec = breaks, 
                                                as.count = as.count, intervals = TRUE, interval.closure = int.closure), 
                                           label.format))
      }
      else {
        if (length(labels) != nbrks - 1 && show.warnings) 
          warning("number of legend labels should be ", 
                  nbrks - 1, call. = FALSE)
        labels = rep(labels, length.out = nbrks - 1)
        attr(labels, "align") <- label.format$text.align
      }
    }
    else {
      if (is.null(labels)) {
        labels = do.call("fancy_breaks", c(list(vec = breaks, 
                                                as.count = FALSE, intervals = FALSE, interval.closure = int.closure), 
                                           label.format))
      }
      else {
        if (length(labels) != length(breaks)) 
          cli::cli_abort("{.field tm_scale_intervals} {.arg labels} should have length {length(breaks)}")
      }
    }
    if (legend$reverse) {
      labels.brks <- attr(labels, "brks")
      labels.align <- attr(labels, "align")
      labels <- rev(labels)
      if (!is.null(labels.brks)) {
        attr(labels, "brks") = labels.brks[length(labels):1L, 
        ]
      }
      attr(labels, "align") = labels.align
      vvalues_rev = rev(vvalues)
    }
    else {
      vvalues_rev = vvalues
    }
    if (na.show) {
      labels.brks = attr(labels, "brks")
      labels.align = attr(labels, "align")
      if (!is.null(labels.brks)) {
        labels <- c(labels, paste(label.na, " ", sep = ""))
        attr(labels, "brks") = rbind(labels.brks, rep(nchar(label.na) + 
                                                        2, 2))
      }
      else {
        labels = c(labels, label.na)
      }
      attr(labels, "align") = labels.align
      vvalues = c(vvalues, value.na)
      vvalues_rev = c(vvalues_rev, value.na)
    }
    if (label.style == "discrete") {
      legend = within(legend, {
        nitems = length(labels)
        labels = labels
        dvalues = breaks
        vvalues = vvalues_rev
        vneutral = value.neutral
        na.show = get("na.show", envir = parent.env(environment()))
        scale = "intervals"
        layer_args = layer_args
      })
    }
    else {
      if ((o$continuous.nclass_per_legend_break%%2) != 
          0) 
        cli::cli_abort("{.field options} the tmap option {.arg continuous.nclass_per_legend_break} should be even")
      labels_select = c(rep(label.select, length.out = length(breaks)), 
                        {
                          if (na.show) TRUE else NULL
                        })
      vvalues = c(unlist(mapply(function(hd, tl) {
        paste(c(rep(hd, o$continuous.nclass_per_legend_break/2), 
                rep(tl, o$continuous.nclass_per_legend_break/2)), 
              collapse = "_")
      }, c(NA_character_, vvalues_rev[1L:(length(breaks) - 
                                            1L)]), c(vvalues_rev[1L:(length(breaks) - 1L)], 
                                                     NA_character_))), {
                                                       if (na.show) value.na else NULL
                                                     })
      nitems = length(labels)
      legend = within(legend, {
        nitems = nitems
        labels = labels
        dvalues = breaks
        vvalues = vvalues
        vneutral = value.neutral
        na.show = get("na.show", envir = parent.env(environment()))
        scale = "intervals"
        layer_args = layer_args
        is_discrete = TRUE
        labels_select = labels_select
        tr = trans_identity
        limits = range(breaks)
      })
    }
    chartFun = paste0("tmapChart", toTitleCase(chart$summary))
    chartArgs = list(chart, bin_colors = vvalues, breaks_def = breaks, 
                     na.show = na.show, x1 = x1)
    if ("breaks" %in% names(chart) && !is.null(chart$breaks)) {
      chartArgs["bin_colors"] = list(NULL)
      chartArgs$breaks_def = chart$breaks
    }
    chart = do.call(chartFun, chartArgs)
    if (submit_legend) {
      if (bypass_ord) {
        format_aes_results(vals, legend = legend, chart = chart)
      }
      else {
        format_aes_results(vals, ids, legend, chart = chart)
      }
    }
    else {
      list(vals = vals, ids = ids, legend = legend, chart = chart, 
           bypass_ord = bypass_ord)
    }
  })
}

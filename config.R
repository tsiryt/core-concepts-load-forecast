wrap_lines <- function(msg, max_length = 90) {
  my_regex <- paste0('(.{1,', max_length, '})(\\msg|$)') # '(.{1,90})(\\msg|$)'
  return(gsub(my_regex, '\\1\n', {msg}))
}

#' Log messages using `layout_glue_colors`'s style.
#' Long messages are wrapped on another line.
layout_wrap_long_lines <- layout_glue_generator(
  format = paste(
    "{crayon::bold(colorize_by_log_level(level, levelr))}",
    '[{crayon::italic(format(time, "%Y-%m-%d %H:%M:%S"))}]',
    "{grayscale_by_log_level({wrap_lines(msg)}, levelr)}"
  )
)
attr(layout_wrap_long_lines, "generator") <- quote(layout_wrap_long_lines())

log_layout(layout_wrap_long_lines)
log_threshold(INFO)

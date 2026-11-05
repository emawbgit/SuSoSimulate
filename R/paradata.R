# ---------------------------------------------------------------------------
# paradata.R -- paradata.tab and system files (interview__actions,
# interview__diagnostics, interview__errors, interview__comments,
# assignment__actions), all derived from the same event stream so they are
# mutually consistent.
#
# Format (SuSo 24.x, checked against real CLASS_PH v17/v18 paradata):
#   interview__id, order, event, responsible, role, timestamp_utc
#   (YYYY-MM-DDThh:mm:ss.mmm), tz_offset (hh:mm:ss; 00:00:00 for server events),
#   parameters.
# AnswerSet: var||value||rv  (rv "1,3"; multi "1, 2"; yes/no "yes|no" logged per
#   click; linked multi "1|2"; text list "a|b"; GPS "lat,lon[acc]alt";
#   timestamps "YYYY-MM-DD hh:mm:ss"). AnswerRemoved: var||rv.
# Final completion is preceded (same timestamp) by the system block
#   VariableDisabled > QuestionDeclaredValid > QuestionDeclaredInvalid > VariableSet.
# Cycles: Completed > Paused > Restarted > Resumed > ... > Completed, and
#   ReceivedBySupervisor > RejectedBySupervisor > ReceivedByInterviewer > Resumed > ...
# ---------------------------------------------------------------------------

.role_labels <- c("<UNKNOWN ROLE>" = 0, Interviewer = 1, Supervisor = 2, Headquarter = 3,
                  Administrator = 4, "API User" = 5)
.action_labels <- c(SupervisorAssigned = 0, InterviewerAssigned = 1, FirstAnswerSet = 2, Completed = 3,
  Restarted = 4, ApprovedBySupervisor = 5, ApprovedByHeadquarter = 6, RejectedBySupervisor = 7,
  RejectedByHeadquarter = 8, Deleted = 9, Restored = 10, UnapprovedByHeadquarter = 11, Created = 12,
  InterviewReceivedByTablet = 13, Resumed = 14, Paused = 15, TranslationSwitched = 16,
  OpenedBySupervisor = 17, ClosedBySupervisor = 18, InterviewSwitchedToCawiMode = 19,
  InterviewSwitchedToCapiMode = 20, InterviewReceivedBySupervisor = 21)
.assign_action_labels <- c(Unknown = 0, Created = 1, Archived = 2, Deleted = 3, ReceivedByTablet = 4,
  Unarchived = 5, AudioRecordingChanged = 6, Reassigned = 7, QuantityChanged = 8, WebModeChanged = 9)

para_value <- function(node, v) {
  clean <- function(s) gsub("[\t\r\n]+", " ", s)
  if (node$kind == "variable") {
    if (is_na1(v)) return("")
    if (is.logical(v)) return(if (v) "True" else "False")
    if (inherits(v, c("POSIXct", "Date"))) {                     # DateTime variables: M/D/YYYY h:mm:ss AM|PM (docs)
      lt <- as.POSIXlt(v, tz = "UTC"); h <- lt$hour %% 12; if (h == 0) h <- 12
      return(sprintf("%d/%d/%d %d:%02d:%02d %s", lt$mon + 1, lt$mday, lt$year + 1900, h, lt$min, as.integer(lt$sec),
                     if (lt$hour < 12) "AM" else "PM"))
    }
    return(clean(fmt_date_val(if (is.numeric(v)) fmt_num(v) else v)))
  }
  switch(node$qtype,
    multi = {
      if (inherits(v, "sus_yesno")) return(paste0(paste(fmt_num(v$yes), collapse = ", "), "|", paste(fmt_num(v$no), collapse = ", ")))
      if (is.list(v)) return(paste(vapply(v, function(r) paste(fmt_num(r), collapse = ", "), ""), collapse = "|"))
      paste(fmt_num(codes_of(v)), collapse = ", ")
    },
    single = paste(fmt_num(v), collapse = ", "),
    list = clean(paste(unname(unclass(v)), collapse = "|")),
    gps = sprintf("%s,%s[%s]%s", fmt_num(v$Latitude), fmt_num(v$Longitude), fmt_num(v$Accuracy), fmt_num(v$Altitude)),
    area = if (inherits(v, "sus_area")) area_string(v) else clean(as.character(v)),
    date = if (inherits(v, "POSIXct")) format(v, "%Y-%m-%d %H:%M:%S", tz = "UTC") else format(v, "%Y-%m-%d"),
    numeric = fmt_num(v),
    clean(as.character(v)))
}

alt_value <- function(node, v) {
  if (node$kind != "question") return(NULL)
  if (node$qtype == "numeric" && is.numeric(v)) return(v + sample(c(-1, 1, 10), 1))
  if (node$qtype == "single" && is.null(node$lnk) && nrow(node$options) > 1) return(pick(setdiff(node$options$code, v)))
  NULL
}

# identifying (cover) and hidden-scope answers arrive with the assignment
is_preloaded <- function(nd, e) e$kind == "answer" && ((isTRUE(nd$featured) && !length(e$rv)) || identical(nd$scope, 3L))

build_paradata <- function(qn, states, meta, cfg, design) {
  off <- parse_offset(cfg$tz_offset); tzs <- fmt_offset(off)
  hq <- "headquarters"; srv <- "00:00:00"            # server-side events carry tz 00:00:00
  ev_all <- vector("list", length(states))
  comments <- list()
  for (i in seq_along(states)) {
    st <- states[[i]]; u <- st$unit
    int <- u$interviewer; sup <- u$supervisor
    E <- list(event = character(), resp = character(), role = integer(), t = numeric(), tz = character(), par = character())
    add <- function(event, resp, role, t, par = "", tz = tzs) {
      E$event <<- c(E$event, event); E$resp <<- c(E$resp, resp); E$role <<- c(E$role, role)
      E$t <<- c(E$t, as.numeric(t)); E$tz <<- c(E$tz, tz); E$par <<- c(E$par, par)
    }
    val <- function(nd, rv) env_get(st$vals, st_key(nd$varname, rv))
    pset <- function(nd, rv, v, t) add("AnswerSet", int, 1L, t, paste0(nd$varname, "||", para_value(nd, v), "||", rv_str(rv)))
    # one answer as the tablet logs it; yes/no questions log every click ("yes|no" accumulating)
    answer <- function(nd, rv, v, t) {
      if (!inherits(v, "sus_yesno")) return(pset(nd, rv, v, t))
      clicks <- nd$options$code[nd$options$code %in% c(v$yes, v$no)]
      k <- length(clicks)
      for (j in seq_len(k)) {
        done <- clicks[seq_len(j)]
        pset(nd, rv, sus_yesno(v$yes[v$yes %in% done], v$no[v$no %in% done], v$all), t - (k - j) * stats::runif(1, 1, 4))
      }
    }
    log <- log_items(st$alog)
    answers <- Filter(function(e) e$kind == "answer" && !is_preloaded(qn$nodes[[e$id]], e), log)
    # re-open: the interviewer revisits a few answers
    redo <- function(t) {
      for (e in pick(answers, stats::rgeom(1, 0.5))) {
        t <- t + answer_seconds(qn$nodes[[e$id]])
        answer(qn$nodes[[e$id]], e$rv, val(qn$nodes[[e$id]], e$rv), t)
      }
      t
    }
    # system re-evaluation emitted with the final completion, in HQ order:
    # VariableDisabled > QuestionDeclaredValid > QuestionDeclaredInvalid > VariableSet
    errs <- log_items(st$errors)
    err_keys <- vapply(errs, function(x) paste0(x$id, "|", rv_str(x$rv)), "")
    system_block <- function(t) {
      for (e in log_items(st$vdis)) add("VariableDisabled", "", 0L, t, paste0(qn$nodes[[e$id]]$varname, "||", rv_str(e$rv)))
      inv <- character()
      for (e in answers) {
        nd <- qn$nodes[[e$id]]; k <- paste0(nd$id, "|", rv_str(e$rv))
        if (k %in% err_keys) for (x in errs[err_keys == k]) inv <- c(inv, paste0(nd$varname, "||", rv_str(e$rv), "||", x$index - 1))
        else add("QuestionDeclaredValid", "", 0L, t, paste0(nd$varname, "||", rv_str(e$rv)))
      }
      for (p in inv) add("QuestionDeclaredInvalid", "", 0L, t, p)
      for (e in log) if (e$kind == "variable") {
        nd <- qn$nodes[[e$id]]; v <- val(nd, e$rv)
        if (!is_na1(v)) add("VariableSet", "", 0L, t, paste0(nd$varname, "||", para_value(nd, v), "||", rv_str(e$rv)))
      }
    }
    complete <- function(t, final) {
      if (final) system_block(t)
      add("Completed", int, 1L, t, pick(c("", "ok", "done")))
      add("Paused", int, 1L, t)
    }

    # creation, preload, interview
    t0 <- u$start_time; tc <- t0 - stats::runif(1, 5, 90)
    add("InterviewCreated", int, 1L, tc, fmt_num(meta$assignment_id[i]))   # HQ 26.x logs the assignment id (24.06 left it empty)
    add("SupervisorAssigned", int, 1L, tc)
    add("InterviewModeChanged", int, 1L, tc, "CAPI||")
    add("InterviewerAssigned", int, 1L, tc, int)
    add("KeyAssigned", "", 0L, tc, meta$key[i])
    for (e in log) {
      nd <- qn$nodes[[e$id]]
      if (is_preloaded(nd, e)) pset(nd, e$rv, val(nd, e$rv), tc)
    }
    add("Resumed", int, 1L, t0, "Tablet")
    prev <- t0
    for (e in answers) {
      nd <- qn$nodes[[e$id]]; v <- val(nd, e$rv); rvs <- rv_str(e$rv)
      if (e$t - prev > 20 && stats::runif(1) < cfg$pause_rate / 25) {   # pause inside the gap since the last answer
        add("Paused", int, 1L, prev + 1); add("Resumed", int, 1L, e$t - 10, "Tablet")
      }
      prev <- e$t
      if (stats::runif(1) < cfg$correction_rate) {           # a wrong answer, then removed or overwritten
        a <- alt_value(nd, v)
        if (!is.null(a)) pset(nd, e$rv, a, e$t - stats::runif(1, 4, 8))
        if (is.null(a) || stats::runif(1) < 0.5) add("AnswerRemoved", int, 1L, e$t - stats::runif(1, 1, 3), paste0(nd$varname, "||", rvs))
      }
      answer(nd, e$rv, v, e$t)
      if (stats::runif(1) < cfg$comment_rate) {
        txt <- pick(c("respondent not sure", "value confirmed with respondent", "estimated by respondent", "see notes"))
        add("CommentSet", int, 1L, e$t + 2, paste0(nd$varname, "||", txt, if (nzchar(rvs)) paste0("||", rvs) else ""))
        comments[[length(comments) + 1]] <- list(i = i, node = nd, rv = e$rv, t = e$t + 2, who = int, role = 1L, txt = txt)
      }
    }

    # completion, optional re-open (Restarted) and HQ rejection cycle, final status
    s <- meta$status[i]
    reject <- s != 65 && stats::runif(1) < cfg$rejection_rate
    t <- st$end_time + stats::runif(1, 10, 60)
    if (stats::runif(1) < cfg$restart_rate) {
      complete(t, FALSE)
      t <- t + stats::runif(1, 60, 1800)
      add("Restarted", int, 1L, t, tz = srv); add("Resumed", int, 1L, t + 1, "Tablet")
      t <- redo(t + 2) + stats::runif(1, 10, 60)
    }
    complete(t, !reject)
    t <- t + stats::runif(1, 1800, 6 * 3600); add("ReceivedBySupervisor", "", 0L, t, tz = srv)
    if (reject) {
      t <- t + stats::runif(1, 3600, 36 * 3600)
      api <- stats::runif(1) < 0.4
      add("RejectedBySupervisor", if (api) "api_user" else hq, if (api) 5L else 3L, t,
          "Please review the flagged answers and resubmit", tz = if (api) srv else tzs)
      t <- t + stats::runif(1, 600, 7200); add("ReceivedByInterviewer", "", 0L, t, tz = srv)
      t <- t + stats::runif(1, 3600, 24 * 3600); add("Resumed", int, 1L, t, "Tablet")
      t <- redo(t + 2) + stats::runif(1, 10, 60)
      complete(t, TRUE)
      t <- t + stats::runif(1, 1800, 6 * 3600); add("ReceivedBySupervisor", "", 0L, t, tz = srv)
    }
    if (s %in% c(120, 130, 125)) { t <- t + stats::runif(1, 3600, 36 * 3600); add("ApproveBySupervisor", sup, 2L, t, "ok") }
    if (s == 130) { t <- t + stats::runif(1, 3600, 72 * 3600); add("ApproveByHeadquarter", hq, 3L, t, "") }
    if (s == 125) { t <- t + stats::runif(1, 3600, 72 * 3600); add("RejectedByHeadquarter", hq, 3L, t, "Please review the flagged answers") }
    if (s == 65) { t <- t + stats::runif(1, 3600, 24 * 3600); add("RejectedBySupervisor", sup, 2L, t, "Please verify and resubmit")
                   add("ReceivedByInterviewer", "", 0L, t + stats::runif(1, 600, 7200), tz = srv) }
    # all event times are on the local fieldwork clock; store as UTC (stable order for ties)
    tu <- E$t - off
    o <- order(tu, seq_along(tu))
    ev_all[[i]] <- data.frame(interview__id = meta$id[i], order = seq_along(o), event = E$event[o],
      responsible = E$resp[o], role = E$role[o],
      timestamp_utc = fmt_ts(as.POSIXct(tu[o], origin = "1970-01-01", tz = "UTC"), ms = TRUE),
      tz_offset = E$tz[o], parameters = E$par[o], stringsAsFactors = FALSE)
  }
  list(paradata = do.call(rbind, ev_all), comments = comments, per = ev_all)
}

build_system_files <- function(qn, states, meta, para, cfg) {
  off <- parse_offset(cfg$tz_offset)
  lab <- function(x, l) { attr(x, "label") <- l; x }
  key_cols <- function(df, i) { df$interview__key <- meta$key[i]; df$interview__id <- meta$id[i]; df }
  map <- c(InterviewCreated = 12, SupervisorAssigned = 0, InterviewModeChanged = 20, InterviewerAssigned = 1,
           Completed = 3, ApproveBySupervisor = 5, ApproveByHeadquarter = 6, RejectedBySupervisor = 7,
           RejectedByHeadquarter = 8, Restarted = 4)
  acts <- list(); diag <- list(); errs <- list()
  for (i in seq_along(states)) {
    p <- para$per[[i]]; st <- states[[i]]; u <- st$unit
    first <- which(p$event == "AnswerSet")[1]
    keep <- which(p$event %in% names(map) | seq_len(nrow(p)) == first)
    a <- p[keep, ]
    code <- ifelse(seq_len(nrow(p))[keep] == first & a$event == "AnswerSet", 2, map[a$event])
    rn <- character(nrow(a)); rr <- integer(nrow(a))
    for (j in seq_len(nrow(a))) {
      ev <- a$event[j]
      if (ev %in% c("SupervisorAssigned", "Completed", "RejectedByHeadquarter")) { rn[j] <- u$supervisor; rr[j] <- 2L }
      else if (ev == "Restarted") { rn[j] <- u$interviewer; rr[j] <- 1L }
      else if (ev %in% c("ApproveBySupervisor")) { rn[j] <- "headquarters"; rr[j] <- 3L }
      else if (ev == "ApproveByHeadquarter") { rn[j] <- ""; rr[j] <- NA_integer_ }
      else { rn[j] <- u$interviewer; rr[j] <- 1L }
    }
    ts <- as.POSIXct(a$timestamp_utc, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    resp <- ifelse(nzchar(a$responsible), a$responsible, u$interviewer)
    acts[[i]] <- data.frame(interview__key = meta$key[i], interview__id = meta$id[i],
      date = format(ts, "%Y-%m-%d"), time = format(ts, "%H:%M:%S"), action = as.numeric(code),
      originator = resp, role = as.numeric(ifelse(a$role == 0, 1, a$role)), responsible__name = rn,
      responsible__role = as.numeric(rr), stringsAsFactors = FALSE)
    pz <- p$event %in% c("Paused", "Resumed")
    pt <- as.numeric(as.POSIXct(p$timestamp_utc, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
    active <- 0; last <- NA
    for (j in seq_len(nrow(p))) {
      if (p$event[j] == "Resumed") last <- pt[j]
      if (p$event[j] == "Paused" && !is.na(last)) { active <- active + pt[j] - last; last <- NA }
    }
    s <- meta$status[i]
    diag[[i]] <- data.frame(interview__key = meta$key[i], interview__id = meta$id[i], interview__status = s,
      responsible = switch(as.character(s), "65" = u$interviewer, "100" = u$supervisor, "125" = u$supervisor, "headquarters"),
      interviewers = 1, rejections__sup = sum(p$event == "RejectedBySupervisor"),
      rejections__hq = sum(p$event == "RejectedByHeadquarter"),
      entities__errors = meta$n_errors[i], questions__comments = sum(p$event == "CommentSet"),
      interview__duration = sprintf("%02d.%s", as.integer(active %/% 86400), fmt_hms(active %% 86400)),
      n_questions_unanswered = st$n_unanswered, stringsAsFactors = FALSE)
    for (x in Filter(function(e) e$severity == 0, log_items(st$errors))) {   # warnings are not exported
      nd <- qn$nodes[[x$id]]; rv <- x$rv
      errs[[length(errs) + 1]] <- data.frame(interview__key = meta$key[i], interview__id = meta$id[i],
        roster = if (length(rv)) qn$levels[[nd$path[length(nd$path)]]]$file else "",
        id1 = rv[1], id2 = rv[2], id3 = rv[3], id4 = rv[4], variable = nd$varname, type = 2,
        message__number = x$index, message = x$msg %||% "", stringsAsFactors = FALSE)
    }
  }
  act <- do.call(rbind, acts)
  act$action <- haven::labelled(act$action, .action_labels, "Type of action taken")
  act$role <- haven::labelled(act$role, .role_labels, "System role of the person performing the action")
  act$responsible__role <- haven::labelled(act$responsible__role, .role_labels, "System role of the person now responsible for the interview")
  act$date <- lab(act$date, "Date when the action was taken"); act$time <- lab(act$time, "Time when the action was taken")
  act$originator <- lab(act$originator, "Login name of the person performing the action")
  act$responsible__name <- lab(act$responsible__name, "Login name of the person now responsible for the interview")
  dg <- do.call(rbind, diag)
  dg$interview__status <- haven::labelled(dg$interview__status, .status_labels, "Last status of interview")
  dlabs <- c(responsible = "Last responsible person", interviewers = "Number of interviewers who worked on this interview",
             rejections__sup = "How many times this interview was rejected by supervisors",
             rejections__hq = "How many times this interview was rejected by HQ",
             entities__errors = "Number of questions and static texts with errors",
             questions__comments = "Number of questions with comments",
             interview__duration = "Active time it took to complete the interview, DD.HH:MM:SS",
             n_questions_unanswered = "Number of unanswered questions")
  for (n in names(dlabs)) dg[[n]] <- lab(dg[[n]], dlabs[[n]])
  er <- if (length(errs)) do.call(rbind, errs) else data.frame(interview__key = character(), interview__id = character(),
          roster = character(), id1 = numeric(), id2 = numeric(), id3 = numeric(), id4 = numeric(), variable = character(),
          type = numeric(), message__number = numeric(), message = character())
  er$type <- haven::labelled(as.numeric(er$type), c(Section = 1, Question = 2, StaticText = 3, Variable = 4),
                             "Type of the variable where the validation error occurred")
  cm <- if (length(para$comments)) do.call(rbind, lapply(seq_along(para$comments), function(j) {
    x <- para$comments[[j]]; rv <- x$rv; ts <- as.POSIXct(x$t - off, origin = "1970-01-01", tz = "UTC")
    data.frame(interview__key = meta$key[x$i], interview__id = meta$id[x$i],
      roster = if (length(rv)) qn$levels[[x$node$path[length(x$node$path)]]]$file else "",
      id1 = rv[1], id2 = rv[2], id3 = rv[3], id4 = rv[4], variable = x$node$varname, order = 1,
      date = format(ts, "%Y-%m-%d"), time = format(ts, "%H:%M:%S"), originator = x$who, role = x$role,
      comment = x$txt, stringsAsFactors = FALSE) })) else
    data.frame(interview__key = character(), interview__id = character(), roster = character(), id1 = numeric(),
      id2 = numeric(), id3 = numeric(), id4 = numeric(), variable = character(), order = numeric(), date = character(),
      time = character(), originator = character(), role = numeric(), comment = character())
  if (nrow(cm)) cm$order <- stats::ave(seq_len(nrow(cm)), cm$interview__id, FUN = seq_along)
  cm$role <- haven::labelled(as.numeric(cm$role), .role_labels, "System role of the person leaving the comment")
  # assignment actions: Created by HQ, ReceivedByTablet by interviewer
  ua <- !duplicated(meta$assignment_id)
  aa <- do.call(rbind, lapply(which(ua), function(i) {
    u <- states[[i]]$unit; tcr <- u$start_time - off - stats::runif(1, 86400, 5 * 86400)
    trc <- tcr + stats::runif(1, 3600, 86400)
    data.frame(assignment__id = meta$assignment_id[i],
      date = format(c(tcr, trc), "%Y-%m-%d"), time = format(c(tcr, trc), "%H:%M:%S"),
      action = c(1, 4), originator = c("headquarters", u$interviewer), role = c(3, 1),
      responsible__name = u$interviewer, responsible__role = 1, old__value = "", new__value = "", comment = "",
      stringsAsFactors = FALSE) }))
  aa$action <- haven::labelled(aa$action, .assign_action_labels, "Type of action taken")
  aa$role <- haven::labelled(aa$role, .role_labels, "System role of the person performing the action")
  aa$responsible__role <- haven::labelled(aa$responsible__role, .role_labels, "System role of the person now responsible for the assignment")
  fix_keys <- function(df) {
    if (!is.null(df$interview__key)) attr(df$interview__key, "label") <- "Interview key (identifier in XX-XX-XX-XX format)"
    if (!is.null(df$interview__id)) attr(df$interview__id, "label") <- "Unique 32-character long identifier of the interview"
    df
  }
  # id1..idK: one per roster nesting level of the questionnaire (HQ drops the unused ones)
  depth <- max(1L, vapply(qn$levels, function(l) as.integer(l$depth %||% 0L), 1L))
  drop_ids <- function(df) df[, setdiff(names(df), sprintf("id%d", seq_len(4)[seq_len(4) > depth])), drop = FALSE]
  er <- drop_ids(er); cm <- drop_ids(cm)   # ponytail: rosters deeper than 4 levels keep id1..id4 only
  out <- list(interview__actions = fix_keys(act), interview__diagnostics = fix_keys(dg), interview__errors = fix_keys(er),
              interview__comments = fix_keys(cm), assignment__actions = aa)
  for (f in names(out)) for (c in names(out[[f]])) {
    l <- .sys_labels[[f]][[c]] %||% .sys_labels$common[[c]]
    if (!is.null(l)) attr(out[[f]][[c]], "label") <- l
  }
  out
}

# Variable labels of SuSo 24.x system files
.sys_labels <- list(
  common = list(interview__key = "Interview key (identifier in XX-XX-XX-XX format)",
    interview__id = "Unique 32-character long identifier of the interview",
    roster = "Name of the roster containing the variable",
    id1 = "Roster ID of the 1st level of nesting", id2 = "Roster ID of the 2nd level of nesting",
    id3 = "Roster ID of the 3rd level of nesting", id4 = "Roster ID of the 4th level of nesting",
    action = "Type of action taken", assignment__id = "Assignment id (identifier in numeric format)"),
  interview__actions = list(date = "Date when the action was taken", time = "Time when the action was taken",
    originator = "Login name of the person performing the action",
    role = "System role of the person performing the action",
    responsible__name = "Login name of the person now responsible for the interview",
    responsible__role = "System role of the person now responsible for the interview"),
  interview__errors = list(variable = "Variable name for the question, where validation error occurred",
    type = "Type of the variable where the validation error occurred",
    message__number = "Numeric index of the validation rule that has fired", message = "Text of the error message"),
  interview__comments = list(variable = "Variable name for the commented question", order = "Sequential order of the comment",
    date = "Date when the comment was left", time = "Time when the comment was left",
    originator = "Login name of the person leaving the comment", role = "System role of the person leaving the comment",
    comment = "Text of the comment"),
  assignment__actions = list(date = "Utc Date when the action was taken", time = "Utc Time when the action was taken",
    originator = "Login name of the person performing the action",
    role = "System role of the person performing the action",
    responsible__name = "Login name of the person now responsible for the assignment",
    responsible__role = "System role of the person now responsible for the assignment",
    old__value = "Value before changes", new__value = "Value after changes", comment = "Text of the comment"))

